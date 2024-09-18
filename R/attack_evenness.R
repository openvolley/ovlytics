rescale1 <- function(z) z / sum(z)
aev_f <- function(p, p_expected = c(0.25, 0.25, 0.25, 0.125, 0.125)) {
    if (length(p) > 5 || length(p_expected) > 5) return(NA_real_)
    ## pad values if needed
    p_expected <- head(c(p_expected, rep(0.2, 5 - length(p_expected))), 5) ## should vary by role though
    p <- head(c(p, rep(0, 5 - length(p))), 5)
    ## normalize p and p_expected to sum to 1, calculate (total abs difference of p from p_expected) / 2
    ## interpretation: the difference from 1 is the proportion of attacks that would need to go to a different attacker to make the distribution match our reference distribution
    1 - sum(abs(rescale1(p) - rescale1(p_expected))) / 2
}

rot_forward <- function (z, by = 1L) do_rot(z, abs(by))
do_rot <- function (z, by) (((z + by) - 1L) %% 6) + 1L

## construct a string representation of the team lineup (player IDs separated by | and starting with the setter ID) for each row in z
get_lineup <- function(z, tm = "home") {
    lup <- rep(NA_character_, nrow(z))
    for (sp in 1:6) {
        idx <- which(z[[paste0(tm, "_setter_position")]] == sp) ## all rows with the home setter in this position
        temp <- z[idx, paste0(tm, "_player_id", rot_forward(1:6, sp - 1))] ## rot_forward so that the setter appears first in the lineup
        lup[idx] <- apply(temp, 1, paste, collapse = "|")
    }
    lup
}

## check the reference_props dataframe
check_ref_props <- function(rp) {
    if (!is.data.frame(rp) || !(setequal(names(rp), c("player_role", "p_expected")) || setequal(names(rp), c("player_role", "p_expected", "player_front_back")))) {
        stop("reference_props should be a data.frame with names player_role, p_expected, and optionally player_front_back")
    }
    if (any(rp$player_role %in% c("middle", "outside", "opposite", "setter", "libero"))) {
        if (!"player_front_back" %in% names(rp)) {
            if (any(duplicated(rp$player_role)) || !all(rp$player_role %in% c("middle", "outside", "opposite", "setter", "libero")) ||
                any(is.na(rp$p_expected) | rp$p_expected < 0 | rp$p_expected > 1)) {
                stop("reference_props has unexpected player_role format/values")
            }
        } else {
            if (any(duplicated(paste0(rp$player_role, "|", rp$player_front_back))) || !all(rp$player_role %in% c("middle", "outside", "opposite", "setter", "libero")) ||
                any(is.na(rp$p_expected) | rp$p_expected < 0 | rp$p_expected > 1)) {
                stop("reference_props has unexpected player_role format/values")
            }
        }
        invisible(TRUE)
    } else {
        stop("reference_props has unexpected format/values")
    }
}

lineup_filter_players <- function(z) {
    z %>% dplyr::filter(stringr::str_detect(paste0("|", .data$lineup, "|"), stringr::fixed(paste0("|", .data$player_id, "|"))))
}

rallies_ <- function(match_id, point_id) length(unique(paste0(match_id, "@", point_id))) ## count rallies
points_won_ <- function(match_id, point_id, team, point_won_by) { ## counts point won
    tibble(match_id = match_id, point_id = point_id, team = team, point_won_by = point_won_by) %>%
        group_by(.data$match_id, .data$point_id) %>% dplyr::summarize(pw = .data$team[1] == .data$point_won_by[1], .groups = "drop") %>%
        dplyr::summarize(pw = sum(.data$pw, na.rm = TRUE)) %>% pull(.data$pw)
}

#' Reference proportions used in attack evenness calculations
#'
#' @return A tibble with columns `player_role` ("middle", "outside", "opposite", "setter", "libero"), `player_front_back` (whether that player is front or back court), and `p_expected` (the proportion of attacks that the player is expected to make under a perfectly even attack scenario)
#'
#' @seealso [ov_aev()]
#'
#' @export
ov_aev_reference_props <- function() {
    ## default reference proportions split by front/back court
    tribble(~player_role, ~player_front_back, ~p_expected,
            "middle",      "front",            0.25,
            "middle",      "back",             0,
            "outside",     "front",            0.25,
            "outside",     "back",             0.25,
            "opposite",    "front",            0.25,
            "opposite",    "back",             0.25,
            "setter",      "front",            0,     ## in theory we should not need to specify that setters and liberos make no attacks, because
            "setter",      "back",             0,     ##  these should be removed in the processing before this table is used. But to be sure, set
            "libero",      "front",            0,     ##  them to zero here
            "libero",      "back",             0)
}

## this is the original calculation that assumes a team is playing all 6 rotations. It has been superseded by aev0_by_rot but has been kept here temporarily for testing/comparison purposes
aev0 <- function(x, tm, reference_props = NULL, calculate_by = NULL, detail = FALSE) {
    if (is.null(reference_props)) reference_props <- tibble(player_role = c("middle", "outside", "opposite", "setter", "libero"), p_expected = c(0.125, 0.25, 0.25, 0, 0)) ## default reference proportions averaged across rotations
    check_ref_props(reference_props)
    h_pid_c <- paste0("home_player_id", 1:6)
    v_pid_c <- paste0("visiting_player_id", 1:6)
    pid_c <- paste0("player_id", 1:6)
    ## omit exp_so, because it relies on (a) receptions being part of x, and (b) those being representative of e.g. the broader league
    ## if (detail) {
    ##     lso <- x %>% dplyr::filter(.data$skill == "Reception") %>% group_by(.data$evaluation) %>% dplyr::summarize(skill = "Reception", .eso = mean(.data$point_won_by == .data$team, na.rm = TRUE))
    ##     x$.eso <- NULL
    ##     x <- x %>% left_join(lso, by = c("skill", "evaluation"))
    ## }
    x <- dplyr::filter(x, .data$team == tm)
    x$h_lineup <- get_lineup(x, "home")
    x$v_lineup <- get_lineup(x, "visiting")
    this <- bind_rows(x %>% dplyr::filter(.data$home_team == tm) %>% dplyr::rename(lineup = "h_lineup") %>% dplyr::select(-"v_lineup"),
                      x %>% dplyr::filter(.data$visiting_team == tm) %>% dplyr::rename(lineup = "v_lineup") %>% dplyr::select(-"h_lineup"))
    if (detail) {
        this_rec <- this %>% dplyr::filter(.data$skill == "Reception") %>%
            group_by(.data$lineup, .data$setter_id) %>%
            dplyr::summarize(##exp_so = mean(.data$.eso, na.rm = TRUE),
                       N_rec = n(), rec_eff = ovlytics::reception_eff(.data$evaluation), .groups = "drop")
    }
    this <- this %>% dplyr::filter(.data$skill == "Attack") %>%
        dplyr::filter(.data$player_id != .data$setter_id) %>% ## ignoring attacks by the setter
        lineup_filter_players() %>% ## remove any players not in the lineup, which will have come from scouting errors (attack assigned to wrong player)
        group_by(across(all_of(unique(c("lineup", "setter_id", calculate_by))))) %>%
        ## add some values for each lineup
        dplyr::mutate(N_attacks = n(),
                      N_rallies = if (detail) rallies_(.data$match_id, .data$point_id) else NA_integer_,
                      N_rallies_won = if (detail) points_won_(.data$match_id, .data$point_id, .data$team, .data$point_won_by) else NA_integer_,
                      kill_rate = mean(.data$evaluation == "Winning attack", na.rm = TRUE)) %>%
        ## now some values for each player in each lineup
        group_by(.data$player_id, .data$player_name, .data$player_role, .add = TRUE) %>%
        dplyr::summarize(N_attacks = .data$N_attacks[1], N_rallies = .data$N_rallies[1], N_rallies_won = .data$N_rallies_won[1], rally_win_rate = .data$N_rallies_won / .data$N_rallies, kill_rate = .data$kill_rate[1], p = n() / .data$N_attacks, .groups = "drop") %>%
        group_by(across(all_of(unique(c("lineup", "setter_id", calculate_by))))) ## drop setter and re-group
    this <- ungroup(this) %>% left_join(reference_props, by = "player_role") %>% ## add the expected proportions for each player
        mutate(p_expected = if_else(is.na(.data$p_expected), 0.2, .data$p_expected)) %>% ## fill missing values
        group_by(across(all_of(unique(c("lineup", "setter_id", calculate_by)))))
    ## calculate the aev per lineup, keeping track of other info as we go
    this <- this %>% dplyr::summarize(N_attacks = .data$N_attacks[1], N_rallies = .data$N_rallies[1], N_rallies_won = .data$N_rallies_won[1], rally_win_rate = .data$rally_win_rate[1], kill_rate = .data$kill_rate[1], aev = aev_f(.data$p, p_expected = .data$p_expected), .groups = "drop")
    if (detail) {
        ## add passing info
        this <- this %>% left_join(this_rec, by = c("lineup", "setter_id"))
    } else {
        this <- this %>% dplyr::select(-"N_rallies", -"N_rallies_won", -"rally_win_rate")
    }
    this
}

aev0_by_rot <- function(x, tm, reference_props = NULL, calculate_by = NULL, detail = FALSE) {
    if (is.null(reference_props)) reference_props <- ov_aev_reference_props()
    check_ref_props(reference_props)
    h_pid_c <- paste0("home_player_id", 1:6)
    v_pid_c <- paste0("visiting_player_id", 1:6)
    pid_c <- paste0("player_id", 1:6)
    ## omit exp_so, because it relies on (a) receptions being part of x, and (b) those being representative of e.g. the broader league
    ## if (detail) {
    ##     lso <- x %>% dplyr::filter(.data$skill == "Reception") %>% group_by(.data$evaluation) %>% dplyr::summarize(skill = "Reception", .eso = mean(.data$point_won_by == .data$team, na.rm = TRUE))
    ##     x$.eso <- NULL
    ##     x <- x %>% left_join(lso, by = c("skill", "evaluation"))
    ## }
    x <- dplyr::filter(x, .data$team == tm)
    ## lineups as strings in the form "SETTER_ID|PID1|PID2|PID3|PID4|PID5"
    x$h_lineup <- get_lineup(x, "home")
    x$v_lineup <- get_lineup(x, "visiting")
    ## also lineup but by position on court, so that given the players who made no attacks in a calculation group we can figure out where they were on court
    x$h_lineup_order <- apply(x[, paste0("home_player_id", 1:6)], 1, as.character, simplify = FALSE)
    x$v_lineup_order <- apply(x[, paste0("visiting_player_id", 1:6)], 1, as.character, simplify = FALSE)
    ## and the front-court players as a single string, just because this is faster than using lineup_order to infer front/back court players
    x$h_front_lineup <- apply(x[, paste0("home_player_id", 2:4)], 1, function(z) paste0("|", paste0(z, collapse = "|"), "|"))
    x$v_front_lineup <- apply(x[, paste0("visiting_player_id", 2:4)], 1, function(z) paste0("|", paste0(z, collapse = "|"), "|"))
    this <- bind_rows(x %>% dplyr::filter(.data$home_team == tm,
                                          ## exclude scouting errors where the designated setter is not actually on court
                                          .data$setter_id == .data$home_player_id1 | .data$setter_id == .data$home_player_id2 | .data$setter_id == .data$home_player_id3 |
                                          .data$setter_id == .data$home_player_id4 | .data$setter_id == .data$home_player_id5 | .data$setter_id == .data$home_player_id6) %>%
                      dplyr::rename(lineup = "h_lineup", lineup_order = "h_lineup_order", front_lineup = "h_front_lineup") %>%
                      dplyr::select(-"v_lineup", -"v_lineup_order", -"v_front_lineup") %>%
                      mutate(player_front_back = case_when(.data$player_id == .data$home_player_id2 ~ "front",
                                                           .data$player_id == .data$home_player_id3 ~ "front",
                                                           .data$player_id == .data$home_player_id4 ~ "front",
                                                           .data$player_id == .data$home_player_id1 ~ "back",
                                                           .data$player_id == .data$home_player_id5 ~ "back",
                                                           .data$player_id == .data$home_player_id6 ~ "back"),
                             setter_position = .data$home_setter_position,
                             setter_front_back = case_when(.data$home_setter_position %in% c(2, 3, 4) ~ "front",
                                                           .data$home_setter_position %in% c(1, 5, 6) ~ "back")),
                      x %>% dplyr::filter(.data$visiting_team == tm,
                                          ## exclude scouting errors where the designated setter is not actually on court
                                          .data$setter_id == .data$visiting_player_id1 | .data$setter_id == .data$visiting_player_id2 | .data$setter_id == .data$visiting_player_id3 |
                                          .data$setter_id == .data$visiting_player_id4 | .data$setter_id == .data$visiting_player_id5 | .data$setter_id == .data$visiting_player_id6) %>%
                      dplyr::rename(lineup = "v_lineup", lineup_order = "v_lineup_order", front_lineup = "v_front_lineup") %>%
                      dplyr::select(-"h_lineup", -"h_lineup_order", -"h_front_lineup") %>%
                      mutate(player_front_back = case_when(.data$player_id == .data$visiting_player_id2 ~ "front",
                                                           .data$player_id == .data$visiting_player_id3 ~ "front",
                                                           .data$player_id == .data$visiting_player_id4 ~ "front",
                                                           .data$player_id == .data$visiting_player_id1 ~ "back",
                                                           .data$player_id == .data$visiting_player_id5 ~ "back",
                                                           .data$player_id == .data$visiting_player_id6 ~ "back"),
                             setter_position = .data$visiting_setter_position,
                             setter_front_back = case_when(.data$visiting_setter_position %in% c(2, 3, 4) ~ "front",
                                                           .data$visiting_setter_position %in% c(1, 5, 6) ~ "back")))
    if (detail) {
        this_rec <- this %>% dplyr::filter(.data$skill == "Reception") %>%
            group_by(.data$lineup, .data$setter_id) %>%
            dplyr::summarize(##exp_so = mean(.data$.eso, na.rm = TRUE),
                       N_rec = n(), rec_eff = ovlytics::reception_eff(.data$evaluation), .groups = "drop")
    }
    ## pull roles from `this`, assuming that (i) roles are fixed for a given lineup, and (ii) all players appear at least once in every lineup here, otherwise we won't have a role for them. TODO get around that for SHM/SMH because we are assuming roles for those
    lineup_roles <- this %>% dplyr::filter(!is.na(.data$setter_id)) %>%
        distinct(.data$lineup, .data$setter_id, .data$team_id, .data$player_id, player_role0 = .data$player_role) %>%
        ## discard any ambiguous ones (multiple roles for the same player in the same lineup)
        dplyr::filter(!is.na(.data$player_id)) %>% dplyr::add_count(.data$lineup, .data$setter_id, .data$team_id, .data$player_id) %>%
        dplyr::filter(.data$n == 1) %>% dplyr::select(-"n")

    this <- this %>% dplyr::filter(.data$skill == "Attack") %>%
        dplyr::filter(.data$player_id != .data$setter_id, .data$player_role != "libero") %>% ## ignoring attacks by the setter or libero
        ## group by match and set (we assume roles are constant per set) and lineup
        lineup_filter_players() %>% ## remove any players not in the lineup, which will have come from scouting errors (attack assigned to wrong player)
        group_by(across(all_of(unique(c("team_id", "lineup", "lineup_order", "front_lineup", "setter_id", "setter_position", calculate_by))))) %>%
        ## add some values for each lineup
        dplyr::mutate(N_attacks = n(),
                      N_rallies = if (detail) rallies_(.data$match_id, .data$point_id) else NA_integer_,
                      N_rallies_won = if (detail) points_won_(.data$match_id, .data$point_id, .data$team, .data$point_won_by) else NA_integer_,
                      kill_rate = mean(.data$evaluation == "Winning attack", na.rm = TRUE)) %>%
        ## now some values for each player in each lineup
        group_by(.data$player_id, .data$player_name, .data$player_role, .data$player_front_back, .add = TRUE) %>%
        dplyr::summarize(N_attacks = .data$N_attacks[1], N_rallies = .data$N_rallies[1], N_rallies_won = .data$N_rallies_won[1], rally_win_rate = .data$N_rallies_won / .data$N_rallies, kill_rate = .data$kill_rate[1], n = n(), p = n() / .data$N_attacks, .groups = "drop") %>%
        group_by(across(all_of(unique(c("team_id", "lineup", "lineup_order", "front_lineup", "setter_id", "setter_position", calculate_by))))) ## drop setter and re-group

    ## now we want to expand by player_id, i.e. fill in players that made no attacks for a given group in that dataframe
    ## this is slow. but can't see an obvious way to speed it up
    add_missing_players <- function(z, expected_player_ids, setter_id) {
        ## missing_pids <- setdiff(setdiff(expected_player_ids, setter_id), z$player_id)
        missing_pids <- unique(expected_player_ids[!expected_player_ids %in% c(setter_id, z$player_id)]) ## marginally faster?
        new <- z[rep(1, length(missing_pids)), ]
        ## this is faster than mutate, presumably because it avoids dplyr's data type checks
        new$player_id <- missing_pids
        new$player_name <- new$player_role <- new$player_front_back <- NA_character_
        new$n <- new$p <- 0
        bind_rows(z, new)
    }
    this <- this %>% reframe(add_missing_players(across(everything()), expected_player_ids = .data$lineup_order[[1]], setter_id = .data$setter_id[1])) %>% ungroup
    ## fill in their positions front/back
    idx <- which(is.na(this$player_front_back))
    ##this$player_front_back[idx] <- sapply(idx, function(i) { pos <- which(this$lineup_order[[i]] == this$player_id[i]); if (pos %in% 2:4) "front" else "back" }) ## the `else` result will not be correct if the player was not on court, but that should not happen (??)
    ## this is faster
    this$player_front_back[idx] <- ifelse(stringi::stri_detect_fixed(this$front_lineup[idx], paste0("|", this$player_id[idx], "|")), "front", "back")
    ## fill in roles
    this <- left_join(this, lineup_roles, by = c("lineup", "setter_id", "team_id", "player_id")) %>%
        mutate(player_role = if_else(is.na(.data$player_role), .data$player_role0, .data$player_role)) %>% dplyr::select(-"player_role0")
    ## at this point we should have 5 players for each match, lineup and setter position, filled in with 0s where they did not attack (the 5 includes the back-row middle) and their roles
    ## add the expected proportions for each player based on role and front/back
    this <- this %>% left_join(reference_props, by = c("player_role", "player_front_back")) %>%
        mutate(p_expected = if_else(is.na(.data$p_expected), 0.25, .data$p_expected), ## fill missing values
               n_expected = .data$p_expected * .data$N_attacks)
    ## check: this %>% count(player_role, player_front_back, p_expected)
    ## turn p_expected into n_expected and aggregate over setter_position; but retaining individual players and the calculate_by group (which will be match_id by default)
    this <- this %>% group_by(across(all_of(unique(c("lineup", "setter_id", calculate_by, "player_id"))))) %>%
        dplyr::summarize(p = sum(.data$n) / sum(.data$N_attacks), p_expected = sum(.data$n_expected) / sum(.data$N_attacks),
                         rally_win_rate = mean_weighted(.data$rally_win_rate, .data$N_rallies, na.rm = FALSE),
                         kill_rate = mean_weighted(.data$kill_rate, .data$N_attacks, na.rm = FALSE),
                         N_attacks = sum(.data$N_attacks), N_rallies = sum(.data$N_rallies), N_rallies_won = sum(.data$N_rallies_won), .groups = "drop_last")
    ## now we can calculate aev per lineup and calculate_by group, keeping track of other info as we go
    this <- this %>% dplyr::summarize(N_attacks = .data$N_attacks[1], N_rallies = .data$N_rallies[1], N_rallies_won = .data$N_rallies_won[1], rally_win_rate = .data$rally_win_rate[1], kill_rate = .data$kill_rate[1], aev = aev_f(.data$p, p_expected = .data$p_expected), .groups = "drop")
    if (detail) {
        ## add passing info
        this <- this %>% left_join(this_rec, by = c("lineup", "setter_id"))
    } else {
        this <- this %>% dplyr::select(-"N_rallies", -"N_rallies_won", -"rally_win_rate")
    }
    this
}

#' Calculate attack evenness
#'
#' Attack evenness is a measure of how balanced a team's attack is. Teams that rely heavily on one or two attackers will have a relatively low evenness value, whereas teams that use all of their attackers will be higher. See <https://untan.gl/attack-evenness.html> for further background.
#' Evenness is calculated for each lineup used by a team and averaged (weighting by the number of attacks that each lineup made) to get an overall evenness value. Liberos are not expected to attack, and setter attacks (dumps) are ignored. Evenness can be calculated on grouped data: by default, it is calculated by match but averaged over matches when reporting the final result.
#'
#' Note that calculation by group and averaging will not generally give the same results as calculating the average result in one step (e.g. calculating for several matches and averaging those results will probably not give the same answer as calculating for all matches pooled). This is expected: say that my team under-utilizes a particular hitter in one match, and over-utilizes her in another. In both of those matches my team's attack was uneven, and so the average of those two results should return a low evenness value indicating that on average my team's attack was uneven. But if the two matches are pooled and the data analyzed all together, the under-utilization in the first match might be balanced by the over-utilization in the second match, giving a higher attack evenness.
#'
#' @references <https://untan.gl/attack-evenness.html>
#'
#' @param x data.frame: the `plays` data.frame as returned by [datavolley::read_dv()] or [peranavolley::pv_read()]
#' @param team string: the team to calculate attack evenness for. If not provided, it will be calculated separately for all teams in `x`
#' @param rotation string: the calculation needs to know what position each player is playing, so that it can work out how many attacks that player should have made under a "perfectly even attack" scenario. This is important for middle hitters, who are usually replaced by the libero in back court and therefore cannot make attacks at those times. The `rotation` parameter can be one of:
#' - "player_role": use the player roles as specified in the "player_role" column in `x`. These are typically the player roles defined in the roster, but it is left to the user to populate this column
#' - "SHM": assume a setter-hitter-middle rotation order (i.e. outside hitter is in position 2 when the setter is in 1) and infer the player roles from that
#' - "SMH": assume a setter-middle-hitter rotation order and infer the player roles from that
#' - "none": don't assume player roles, which will mean that under a "perfectly even attack" scenario, each player (excluding the setter) will be expected to make 20% of attacks. This option is probably of limited use
#' @param reference_props data.frame or NULL: if `NULL`, the default attack profile as returned by [ov_aev_reference_props()] will be used (recommended). Otherwise, provide a data.frame with the same format as that returned by [ov_aev_reference_props()]
#' @param calculate_by character: names variables in `x` to group by when doing the calculations. Note that "lineup" is always used as a calculation grouping variable (it is populated inside the function). See Details
#' @param report_by character: names of variables in `x` to group by for the final results. Note that results are always effectively grouped by team. Any `report_by` variables will be used in addition to team. Note that:
#' - `report_by` can include "lineup" and/or "setter_id" (these variables are used internally in calculations)
#' - `report_by` variables must also be present in `calculate_by`, with the exception of "lineup" and "setter_id"
#' @param min_N_attacks integer: minimum number of attacks that must be made in order to be included in the calculations. Attacks are counted by lineup and `calculate_by` variables (if any). If `calculate_by` is empty and `min_N_attacks` is 10, then only lineups that made 10 or more attacks (in total) will be included in the calculations. If `min_N_attacks` is 10 and `calculate_by` is "match_id", then calculations will be done match by match, using only lineups in a match that made 10 or more attacks in that match
#' @param detail logical: if `TRUE`, the returned data frame will include additional columns: "aev_sd" (standard deviation of aev across the calculate_by groups), "rally_win_rate", "kill_rate", "rec_eff" (reception efficiency), "N_rallies" (number of rallies played). Note that including these details makes the calculation noticeably slower
## "exp_so" (expected sideout rate),
#' @return A tibble with at least the columns "team", "aev", and "N_attacks". If `detail` was TRUE, additional columns will also be present (see the `detail` parameter)
#'
#' @examples
#'
#' px <- plays(dv_read(ovdata_example()))
#'
#' ## for a single team
#' ov_aev(px, rotation = "SHM", team = "GKS Katowice")
#'
#' ## for all teams in px, and with extra detail
#' ov_aev(px, rotation = "SHM", detail = TRUE)
#'
#' ## for a single team, calculated by set number but aggregate results when reporting
#' ov_aev(px, team = "GKS Katowice", rotation = "SHM", calculate_by = "set_number")
#'
#' ## for a single team, calculated and reported by match and set number
#' ov_aev(px, team = "GKS Katowice", rotation = "SHM", calculate_by = c("match_id", "set_number"),
#'        report_by = c("match_id", "set_number"))
#'
#' ## for a single team, calculated by match and setter position, and reported by setter position
#' px <- ov_augment_plays(px, to_add = "setters", use_existing = FALSE)
#' ov_aev(px, team = "GKS Katowice", rotation = "SHM",
#'        calculate_by = c("match_id", "setter_position"), report_by = "setter_position")
#'
#' @export
ov_aev <- function(x, team, rotation, reference_props = NULL, calculate_by = "match_id", report_by, min_N_attacks = 10, detail = FALSE) {
    detail <- isTRUE(detail)
    ## deal with rotation
    if (length(rotation) < 1) rotation <- "none" ## don't assume a rotation, which will result in the assumption that each player is expected to hit 20% of attacks
    rotation <- match.arg(rotation, c("none", "player_role", "SHM", "SMH"))
    if (rotation %in% c("SHM", "SMH")) {
        x <- ovlytics::ov_augment_plays(x, to_add = c("player_role", "setters"), rotation = rotation, use_existing = FALSE)
        rotation <- "player_role" ## for the recursive call below, use "player_role" because that column is now populated as we want it
    } else if (rotation == "player_role") {
        ## use the existing player_role column
        if (!"player_role" %in% names(x)) {
            stop("rotation has been specified as 'player_role' but there is no 'player_role' column in x")
        }
        ## update setter_id column so that we know that it's the ID is the person in home_setter_position or visiting_setter_position
        x <- ov_augment_plays(x, to_add = "setters", use_existing = FALSE)
    } else {
        x$player_role <- ""
        ## update setter_id column so that we know that it's the ID is the person in home_setter_position or visiting_setter_position
        x <- ov_augment_plays(x, to_add = "setters", use_existing = FALSE)
    }
    if (missing(report_by)) report_by <- NULL
    if (length(report_by) > 0) {
        if (!all(setdiff(report_by, c("lineup", "setter_id")) %in% calculate_by)) stop("all report_by variables must also be present in calculate_by")
    }
    if (missing(team) || length(team) < 1) {
        ## calculate for each team
        return(bind_rows(lapply(sort(unique(na.omit(x$team))), function(tm) {
            ov_aev(x, team = tm, rotation = rotation, reference_props = reference_props, calculate_by = calculate_by, report_by = report_by, min_N_attacks = min_N_attacks, detail = detail)
        })))
    } else {
        if (!team %in% x$team) stop("team '", team, "' does not appear in x")
    }
    temp <- aev0_by_rot(x = x, tm = team, reference_props = reference_props, calculate_by = calculate_by, detail = detail)
    ## temp contains the results calculated by lineup plus calculate_by groups (if any)
    out <- temp %>% ungroup %>% dplyr::filter(.data$N_attacks >= min_N_attacks)
    ## group by report_by if provided. We are aggregating results across lineups but within report_by (which if not provided will effectively be "team", because we are always dealing with a single team's results at this point
    if (length(report_by) > 0) out <- out %>% group_by(across(all_of(report_by)))
    if (detail) {
        out %>% dplyr::filter(!is.na(.data$aev), !is.na(.data$N_attacks)) %>%
            dplyr::summarize(team = team, aev_sd = sqrt(var_weighted(.data$aev, wt = .data$N_attacks)), aev = mean_weighted(.data$aev, .data$N_attacks),
                             rally_win_rate = mean_weighted(.data$rally_win_rate, .data$N_rallies),
                             kill_rate = mean_weighted(.data$kill_rate, .data$N_attacks), ## exp_so = mean_weighted(.data$exp_so, .data$N_rec),
                             rec_eff = mean_weighted(.data$rec_eff, .data$N_rec), N_attacks = sum(.data$N_attacks), N_rallies = sum(.data$N_rallies), ##detail = list(temp),
                             .groups = "drop")
    } else {
        out %>% dplyr::filter(!is.na(.data$aev), !is.na(.data$N_attacks)) %>%
            dplyr::summarize(team = team, aev_sd = sqrt(var_weighted(.data$aev, wt = .data$N_attacks)),
                             aev = mean_weighted(.data$aev, .data$N_attacks), N_attacks = sum(.data$N_attacks), .groups = "drop")
    }
}
