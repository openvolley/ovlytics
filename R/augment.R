#' Add some extra columns to a plays object
#'
#' @param x data.frame: the `plays` data.frame as returned by [datavolley::read_dv()] or [peranavolley::pv_read()]
#' @param to_add character: columns to add
#' * "receiving_team" adds the columns "receiving_team" (team name) and "receiving_team_id"
#' * "winners" adds the columns "set_won_by", "set_won_by_id" (the name and ID of the team that won the current set), "match_won_by", "match_won_by_id" (the name and ID of the team that won the current match), "team_won_set" and "team_won_match" (did the team making the action in the current row win the set/match), and "home_sets_won" and "visiting_sets_won" (the number of sets won by the home and visiting teams)
#' * "touch_summaries" adds a number of columns named "ts_*" that summarize a team touch (e.g. columns "ts_pass_quality", "ts_pass_evaluation_code" give the pass quality and pass evaluation code of the reception or dig associated with a given team touch). "touch_summaries" also adds a column named `freeball_over`, which disambiguates the action of putting a freeball over the net from the action of digging such a ball. Many scouts code both of these as a "Freeball". The `freeball_over` column will be `TRUE` if it was a freeball being put over the net, and `FALSE` otherwise (including freeball digs). Freeballs over and freeball digs will still both have "Freeball" in their `skill` column
#' * "setters" adds the columns "home_setter_id", "visiting_setter_id" (the player IDs of the home and visiting setter on court), and "setter_id", "setter_position", and "setter_front_back" (the player ID and position of the setter from the team performing the current action)
#' * "followed" adds the columns "followed_timeout", "followed_technical_timeout", and "followed_sub"
#' * "player_role" add the column "player_role" which gives the role (outside, middle, opposite, setter) for the active player on each row of `x`. This assumes a standard rotation as specified by `rotation`. Note that `player_role` does NOT include libero, although this can be inferred from the `meta` component of a full datavolley object
#' * "all" is a shortcut for all of the above
#'
#' @param rotation string: (only relevant when `to_add` includes "player_role") either "SHM" (assume a setter-hitter-middle rotation order, i.e. outside hitter is in position 2 when the setter is in 1), or "SMH" (setter-middle-hitter)
#' @param use_existing logical: if `TRUE` and all of the columns associated with a given `to_add` choice are already present in `x`, they won't be re-generated
#'
#' @return `x` with the extra columns added
#'
#' @export
ov_augment_plays <- function(x, to_add = c("receiving_team", "touch_summaries", "setters"), rotation = "SHM", use_existing = TRUE) {
    assert_that(is.character(to_add))
    assert_that(is.flag(use_existing), !is.na(use_existing))
    to_add <- tolower(to_add)
    known_to_add <- c("receiving_team", "touch_summaries", "setters", "followed", "winners", "player_role", "attacker_name") ## the allowed values
    if ("all" %in% to_add) to_add <- known_to_add
    if (!all(to_add %in% known_to_add)) {
        warning("unrecognized 'to_add' values, ignoring: ", paste0(setdiff(to_add, known_to_add)))
    }
    if ("player_role" %in% to_add) rotation <- match.arg(rotation, c("SHM", "SMH"))

    if ("receiving_team" %in% to_add && (!all(c("receiving_team", "receiving_team_id") %in% names(x)) || !use_existing)) {
        x <- dplyr::mutate(x, receiving_team = case_when(.data$serving_team %eq% .data$home_team ~ .data$visiting_team,
                                                         .data$serving_team %eq% .data$visiting_team ~ .data$home_team),
                           receiving_team_id = case_when(.data$serving_team %eq% .data$home_team ~ .data$visiting_team_id,
                                                      .data$serving_team %eq% .data$visiting_team ~ .data$home_team_id))
    }

    if ("player_role" %in% to_add && (!"player_role" %in% names(x) || !use_existing)) {
        x <- mutate(x, player_current_position = case_when(.data$player_id == .data$home_player_id1 ~ 1,
                                                           .data$player_id == .data$visiting_player_id1 ~ 1,
                                                           .data$player_id == .data$home_player_id2 ~ 2,
                                                           .data$player_id == .data$visiting_player_id2 ~ 2,
                                                           .data$player_id == .data$home_player_id3 ~ 3,
                                                           .data$player_id == .data$visiting_player_id3 ~ 3,
                                                           .data$player_id == .data$home_player_id4 ~ 4,
                                                           .data$player_id == .data$visiting_player_id4 ~ 4,
                                                           .data$player_id == .data$home_player_id5 ~ 5,
                                                           .data$player_id == .data$visiting_player_id5 ~ 5,
                                                           .data$player_id == .data$home_player_id6 ~ 6,
                                                           .data$player_id == .data$visiting_player_id6 ~ 6),
                    temp_setter_position = case_when(.data$team == .data$home_team ~ .data$home_setter_position,
                                                     .data$team == .data$visiting_team ~ .data$visiting_setter_position))
        if (rotation == "SHM") {
            x <- dplyr::mutate(x,
                               player_role = case_when(.data$temp_setter_position == 1 & .data$player_current_position %in% c(2, 5) ~ "outside",
                                                       .data$temp_setter_position == 1 & .data$player_current_position %in% c(3, 6) ~ "middle",
                                                       .data$temp_setter_position == 1 & .data$player_current_position == 4 ~ "opposite",
                                                       .data$temp_setter_position == 1 & .data$player_current_position == 1 ~ "setter",
                                                       .data$temp_setter_position == 2 & .data$player_current_position %in% c(3, 6) ~ "outside",
                                                       .data$temp_setter_position == 2 & .data$player_current_position %in% c(4, 1) ~ "middle",
                                                       .data$temp_setter_position == 2 & .data$player_current_position == 5 ~ "opposite",
                                                       .data$temp_setter_position == 2 & .data$player_current_position == 2 ~ "setter",
                                                       .data$temp_setter_position == 3 & .data$player_current_position %in% c(4, 1) ~ "outside",
                                                       .data$temp_setter_position == 3 & .data$player_current_position %in% c(5, 2) ~ "middle",
                                                       .data$temp_setter_position == 3 & .data$player_current_position == 6 ~ "opposite",
                                                       .data$temp_setter_position == 3 & .data$player_current_position == 3 ~ "setter",
                                                       .data$temp_setter_position == 4 & .data$player_current_position %in% c(5, 2) ~ "outside",
                                                       .data$temp_setter_position == 4 & .data$player_current_position %in% c(6, 3) ~ "middle",
                                                       .data$temp_setter_position == 4 & .data$player_current_position == 1 ~ "opposite",
                                                       .data$temp_setter_position == 4 & .data$player_current_position == 4 ~ "setter",
                                                       .data$temp_setter_position == 5 & .data$player_current_position %in% c(6, 3) ~ "outside",
                                                       .data$temp_setter_position == 5 & .data$player_current_position %in% c(1, 4) ~ "middle",
                                                       .data$temp_setter_position == 5 & .data$player_current_position == 2 ~ "opposite",
                                                       .data$temp_setter_position == 5 & .data$player_current_position == 5 ~ "setter",
                                                       .data$temp_setter_position == 6 & .data$player_current_position %in% c(1, 4) ~ "outside",
                                                       .data$temp_setter_position == 6 & .data$player_current_position %in% c(2, 5) ~ "middle",
                                                       .data$temp_setter_position == 6 & .data$player_current_position == 3 ~ "opposite",
                                                       .data$temp_setter_position == 6 & .data$player_current_position == 6 ~ "setter"))
        } else {
            x <- dplyr::mutate(x,
                               player_role = case_when(.data$temp_setter_position == 1 & .data$player_current_position %in% c(2, 5) ~ "middle",
                                                       .data$temp_setter_position == 1 & .data$player_current_position %in% c(3, 6) ~ "outside",
                                                       .data$temp_setter_position == 1 & .data$player_current_position == 4 ~ "opposite",
                                                       .data$temp_setter_position == 1 & .data$player_current_position == 1 ~ "setter",
                                                       .data$temp_setter_position == 2 & .data$player_current_position %in% c(3, 6) ~ "middle",
                                                       .data$temp_setter_position == 2 & .data$player_current_position %in% c(4, 1) ~ "outside",
                                                       .data$temp_setter_position == 2 & .data$player_current_position == 5 ~ "opposite",
                                                       .data$temp_setter_position == 2 & .data$player_current_position == 2 ~ "setter",
                                                       .data$temp_setter_position == 3 & .data$player_current_position %in% c(4, 1) ~ "middle",
                                                       .data$temp_setter_position == 3 & .data$player_current_position %in% c(5, 2) ~ "outside",
                                                       .data$temp_setter_position == 3 & .data$player_current_position == 6 ~ "opposite",
                                                       .data$temp_setter_position == 3 & .data$player_current_position == 3 ~ "setter",
                                                       .data$temp_setter_position == 4 & .data$player_current_position %in% c(5, 2) ~ "middle",
                                                       .data$temp_setter_position == 4 & .data$player_current_position %in% c(6, 3) ~ "outside",
                                                       .data$temp_setter_position == 4 & .data$player_current_position == 1 ~ "opposite",
                                                       .data$temp_setter_position == 4 & .data$player_current_position == 4 ~ "setter",
                                                       .data$temp_setter_position == 5 & .data$player_current_position %in% c(6, 3) ~ "middle",
                                                       .data$temp_setter_position == 5 & .data$player_current_position %in% c(1, 4) ~ "outside",
                                                       .data$temp_setter_position == 5 & .data$player_current_position == 2 ~ "opposite",
                                                       .data$temp_setter_position == 5 & .data$player_current_position == 5 ~ "setter",
                                                       .data$temp_setter_position == 6 & .data$player_current_position %in% c(1, 4) ~ "middle",
                                                       .data$temp_setter_position == 6 & .data$player_current_position %in% c(2, 5) ~ "outside",
                                                       .data$temp_setter_position == 6 & .data$player_current_position == 3 ~ "opposite",
                                                       .data$temp_setter_position == 6 & .data$player_current_position == 6 ~ "setter"))
        }
        x <- dplyr::select(x, -"temp_setter_position", -"player_current_position")
    }

    if ("touch_summaries" %in% to_add && (!all(c("ts_pass_quality", "ts_set_error", "ts_pass_evaluation_code", "freeball_over") %in% names(x)) || !use_existing)) {
        x <- x[, setdiff(names(x), c("ts_pass_quality", "ts_set_error", "ts_pass_evaluation_code"))]
        if (!"freeball_over" %in% names(x)) {
            ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
            x <- mutate(x, freeball_over = .data$skill %eq% "Freeball" &
                               lag(.data$match_id) %eq% .data$match_id & ##lead(.data$match_id) %eq% .data$match_id,
                               lag(.data$point_id) %eq% .data$point_id & ##lead(.data$point_id) %eq% .data$point_id,
                               ((!is.na(lead(.data$team)) & !is.na(lead(.data$team)) & lead(.data$team) != .data$team) | lag(.data$team) %eq% .data$team))
        }
        ts1 <- dplyr::filter(ungroup(x), !is.na(.data$team))
        touchsum <- dplyr::summarize(group_by(ts1, .data$match_id, .data$team, .data$team_touch_id),
                                     ts_pass_quality = single_value_or(na.omit(.data$evaluation[.data$skill %in% c("Reception", "Dig") | (.data$skill %eq% "Freeball" & !.data$freeball_over)]), or = NA_character_),
                                     ts_pass_evaluation_code = single_value_or(na.omit(.data$evaluation_code[.data$skill %in% c("Reception", "Dig") | (.data$skill %eq% "Freeball" & !.data$freeball_over)]), or = NA_character_),
                                     ts_set_error = any(.data$skill %eq% "Set" & .data$evaluation %eq% "Error"))

        touchsum <- dplyr::mutate(touchsum, ts_pass_quality = case_when(grepl("^Perfect", .data$ts_pass_quality) ~ "Perfect",
                                                                        grepl("^(Good|Positive)", .data$ts_pass_quality) ~ "Good",
                                                                        grepl("^OK", .data$ts_pass_quality) ~ "OK",
                                                                        grepl("^(Poor|Negative|No structured|Ball directly back over net)", .data$ts_pass_quality) ~ "Poor",
                                                                        grepl("Error", .data$ts_pass_quality) ~ "Error",
                                                                        TRUE ~ .data$ts_pass_quality))
        x <- left_join(x, dplyr::select(ungroup(touchsum), -"team"), by = c("match_id", "team_touch_id"))
    }

    if ("followed" %in% to_add && (!all(c("followed_timeout", "followed_technical_timeout", "followed_sub") %in% names(x)) || !use_existing)) {
        x <- augment_followed_cols(x)
    }

    wnames <- c("set_won_by", "set_won_by_id", "team_won_set", "match_won_by", "match_won_by_id", "team_won_match", "home_sets_won", "visiting_sets_won")
    if ("winners" %in% to_add && (!all(wnames %in% names(x)) || !use_existing)) {
        x <- x[, setdiff(names(x), wnames)]
        winners <- dplyr::summarize(group_by(dplyr::filter(x, !is.na(.data$match_id) & !is.na(.data$set_number)), .data$match_id, .data$set_number),
                                    set_won_by = case_when(max(.data$home_team_score, na.rm = TRUE) > max(.data$visiting_team_score, na.rm = TRUE) ~ "home",
                                                           max(.data$visiting_team_score, na.rm = TRUE) > max(.data$home_team_score, na.rm = TRUE) ~ "visiting"),
                                    .groups = "drop")
        ## set_won_by is "home" or "visiting"
        winners <- group_by(winners, .data$match_id) %>% mutate(home_sets_won = sum(.data$set_won_by == "home", na.rm = TRUE),
                                                                visiting_sets_won = sum(.data$set_won_by == "visiting", na.rm = TRUE),
                                                                match_won_by = case_when(.data$home_sets_won > .data$visiting_sets_won ~ "home",
                                                                                         .data$visiting_sets_won > .data$home_sets_won ~ "visiting")) %>%
            ungroup
        ## match_won_by is "home" or "visiting"
        ## some checks
        if (nrow(winners) != nrow(distinct(winners[, c("match_id", "set_number")]))) {
            warning("set/match_won_by code failed")
            winners <- mutate(winners, match_won_by = NA_character_, set_won_by = NA_character_, home_sets_won = NA_integer_, visiting_sets_won = NA_integer_)
        } else {
            tempx <- left_join(x, winners, by = c("match_id", "set_number"))
            if (nrow(tempx) != nrow(x)) {
                warning("set/match_won_by code failed")
                x <- mutate(x, match_won_by = NA_character_, set_won_by = NA_character_, home_sets_won = NA_integer_, visiting_sets_won = NA_integer_)
            } else {
                x <- tempx
            }
        }
        ## now fill with team names and ids
        x <- mutate(x,
                    set_won_by_id = case_when(.data$set_won_by == "home" ~ .data$home_team_id, .data$set_won_by == "visiting" ~ .data$visiting_team_id),
                    set_won_by = case_when(.data$set_won_by == "home" ~ .data$home_team, .data$set_won_by == "visiting" ~ .data$visiting_team),
                    team_won_set = .data$team_id == .data$set_won_by_id,
                    match_won_by_id = case_when(.data$match_won_by == "home" ~ .data$home_team_id, .data$match_won_by == "visiting" ~ .data$visiting_team_id),
                    match_won_by = case_when(.data$match_won_by == "home" ~ .data$home_team, .data$match_won_by == "visiting" ~ .data$visiting_team),
                    team_won_match = .data$team_id == .data$match_won_by_id)
    }

    if ("setters" %in% to_add && (!all(c("home_setter_id", "visiting_setter_id", "setter_id", "setter_position", "setter_front_back") %in% names(x)) || !use_existing)) {
        # Differentiate indoor and beach:

        if(all(c("home_player_id3", "home_player_id4", "home_player_id5", "home_player_id6") %in% names(x))){
            # INDOOR: The setter is identified per 6 player rotation, scouted in datavolley.
            x <- x[, setdiff(names(x), c("home_setter_id", "visiting_setter_id", "setter_id", "setter_position"))]
            x <- mutate(x, home_setter_id = case_when(.data$home_setter_position == 1 ~ .data$home_player_id1,
                                                      .data$home_setter_position == 2 ~ .data$home_player_id2,
                                                      .data$home_setter_position == 3 ~ .data$home_player_id3,
                                                      .data$home_setter_position == 4 ~ .data$home_player_id4,
                                                      .data$home_setter_position == 5 ~ .data$home_player_id5,
                                                      .data$home_setter_position == 6 ~ .data$home_player_id6),
                        visiting_setter_id = case_when(.data$visiting_setter_position == 1 ~ .data$visiting_player_id1,
                                                       .data$visiting_setter_position == 2 ~ .data$visiting_player_id2,
                                                       .data$visiting_setter_position == 3 ~ .data$visiting_player_id3,
                                                       .data$visiting_setter_position == 4 ~ .data$visiting_player_id4,
                                                       .data$visiting_setter_position == 5 ~ .data$visiting_player_id5,
                                                       .data$visiting_setter_position == 6 ~ .data$visiting_player_id6),
                        setter_id = case_when(.data$team_id == .data$home_team_id ~ .data$home_setter_id,
                                              .data$team_id == .data$visiting_team_id ~ .data$visiting_setter_id),
                        setter_position = case_when(.data$team == .data$home_team ~ .data$home_setter_position,
                                                    .data$team == .data$visiting_team ~ .data$visiting_setter_position),
                        setter_front_back = case_when(.data$setter_position %in% c(1, 5, 6) ~ "back",
                                                      .data$setter_position %in% c(2, 3, 4) ~ "front"))
        } else {
            # BEACH: Ha. Well. The setter is the one not attacking.
            x <- mutate(x, home_setter_id = case_when(.data$player_id == .data$home_player_id1 & .data$skill == "Attack" ~ .data$home_player_id2,
                                                      .data$player_id == .data$home_player_id2 & .data$skill == "Attack" ~ .data$home_player_id1,
                                                      TRUE ~ NA_character_),
                        visiting_setter_id = case_when(.data$player_id == .data$visiting_player_id1 & .data$skill == "Attack" ~ .data$visiting_player_id2,
                                                       .data$player_id == .data$visiting_player_id2 & .data$skill == "Attack" ~ .data$visiting_player_id1,
                                                       TRUE ~ NA_character_),
                        setter_id = case_when(.data$team_id == .data$home_team_id ~ .data$home_setter_id,
                                              .data$team_id == .data$visiting_team_id ~ .data$visiting_setter_id))
        }
    }
    x
}


## followed timeout, followed sub, followed TTO
augment_followed_cols <- function(x) {
    x <- x[, setdiff(names(x), c("followed_timeout", "followed_technical_timeout", "followed_sub"))]
    fs <- fto <- ftto <- rep(FALSE, nrow(x))
    x$substitution[grepl(">LUp", x$code, fixed = TRUE)] <- FALSE
    tempsub <- x$substitution
    tempsub[is.na(tempsub)] <- FALSE
    tempto <- x$skill %eq% "Timeout"
    temptto <- x$skill %eq% "Technical timeout"
    tempserve <- x$skill %eq% "Serve"
    this_fs <- FALSE; this_fto <- FALSE; this_ftto <- FALSE
    for (z in seq_len(nrow(x))[-1]) {
        if (tempsub[z]) this_fs <- TRUE
        if (tempto[z]) this_fto <- TRUE
        if (temptto[z]) this_ftto <- TRUE
        if (tempserve[z]) {
            fs[z] <- this_fs
            fto[z] <- this_fto
            ftto[z] <- this_ftto
        }
        if (tempserve[z] || x$end_of_set[z]) {
            this_fs <- FALSE; this_fto <- FALSE; this_ftto <- FALSE
        }
    }
    temp <- tibble(followed_sub = fs, followed_timeout = fto, followed_technical_timeout = ftto, match_id = x$match_id, point_id = x$point_id)
    ## followed sub
    x <- left_join(x, distinct(dplyr::select(dplyr::filter(temp, .data$followed_sub), "match_id", "point_id", "followed_sub")), by = c("match_id", "point_id"))
    x <- mutate(x, followed_sub = case_when(is.na(.data$followed_sub) ~ FALSE, TRUE ~ .data$followed_sub))
    ## followed TO
    x <- left_join(x, distinct(dplyr::select(dplyr::filter(temp, .data$followed_timeout), "match_id", "point_id", "followed_timeout")), by = c("match_id", "point_id"))
    x <- mutate(x, followed_timeout = case_when(is.na(.data$followed_timeout) ~ FALSE, TRUE ~ .data$followed_timeout))
    ## followed TTO
    x <- left_join(x, distinct(dplyr::select(dplyr::filter(temp, .data$followed_technical_timeout), "match_id", "point_id", "followed_technical_timeout")), by = c("match_id", "point_id"))
    mutate(x, followed_technical_timeout = case_when(is.na(.data$followed_technical_timeout) ~ FALSE, TRUE ~ .data$followed_technical_timeout))
}
