#' Infer the role of each player
#'
#' @details
#' The inference procedure assumes that each player plays the same role across the entire data set `x`. If `x` is drawn from multiple matches, and a player changes role from match to match, it will likely give misleading results for that player. In that case, it is probably better to run this inference on each match separately.
#'
#' @param x : a datavolley object (as returned by [datavolley::dv_read()]), a list of datavolley objects, or the `plays` component of a datavolley object
#' @param target_team string or function: team to report on. If this is a function, it should return `TRUE` when passed the target team name
#' @param method string: one of
#' - "meta" - rely on player metadata, i.e. the team rosters
#' - "SHM" - assume a setter-hitter-middle rotation order. When the setter is in position 1 the outside hitters are in 2 and 5, and middles in 3 and 6
#' - "SMH"  - setter-middle-hitter rotation order. When the setter is in position 1 the middles are in 2 and 5, and outside hitters in 3 and 6
#' - or "data" - try and identify player roles based on the ball touches that each player makes. Note that this assumes certain play characteristics: outsides hit pipes and in front row hit from the left except in P1 reception, opposites hit from the right (left in P1 reception), middles hit quick balls or slides or from zone 3
#' Method "meta" is the default if a datavolley object or list of objects is provided. Note that "SHM" and "SMH" cannot identify liberos, because they do not appear in the point-by-point on-court lineups.
#' @param fall_back logical: if `TRUE` and `method` is "meta" and x is a single datavolley object BUT player roles are not provided in the DataVolley file metadata section, fall back to method "data"
#' @param setter_tip_codes character: vector of attack combination codes that correspond to setter tips and other attacks that do not tend to be made from a consistent start zone. Only relevant if `method` is "data"
#'
#' @return A data.frame with columns `player_id`, `role`, and `score` (the approximate confidence in the role assignment, in the range 0 to 1)
#'
#' @examples
#' x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
#' ## guess roles according to the actions that the players made
#' rx <- ov_infer_player_roles(x, target_team = "Nova KBM Branik", method = "data")
#'
#' @export
ov_infer_player_roles <- function(x, target_team, method, fall_back = TRUE, setter_tip_codes = c("PP", "PR", "P2")) {
    if (missing(method)) {
        method <- if (("meta" %in% names(x)) || all(vapply(x, function(z) "meta" %in% names(z), FUN.VALUE = TRUE))) "meta" else "data"
    }
    assert_that(is.string(method))
    method <- tolower(method)
    method <- match.arg(method, c("meta", "data", "shm", "smh"))
    is_target_team <- if (is.character(target_team)) function(z) z %eq% target_team else target_team
    assert_that(is.function(is_target_team))
    was_single_dv <- FALSE
    if ("meta" %in% names(x)) {
        was_single_dv <- TRUE
        x <- list(x)
    }
    if (method == "meta") {
        if (!all(vapply(x,function(z) "meta" %in% names(z), FUN.VALUE = TRUE)))
            stop("need a datavolley object or list of such objects for method \"meta\"")
        target_players_from_meta <- function(z) {
            if (is_target_team(home_team(z))) {
                z$meta$players_h
            } else if (is_target_team(visiting_team(z))) {
                z$meta$players_v
            } else {
                NULL
            }
        }
        m <- do.call(rbind, lapply(x, target_players_from_meta))
        if (nrow(m) < 1) stop("target_team does not appear to be present in the data")
        roles <- m %>% group_by(.data$player_id, .data$role) %>%
            dplyr::summarize(n = n(), role = .data$role[.data$n == max(.data$n)], score = max(.data$n)/sum(.data$n)) %>%
            dplyr::select(-"n")
        ## old dv meta uses "pass-hitter" instead of "outside"
        roles <- mutate(roles, role = case_when(.data$role == "pass-hitter" ~ "outside", TRUE ~ .data$role))
        if (was_single_dv && all(roles$role %in% c(NA, "", "libero")) && fall_back) {
            ## for a single match, with no roles entered in the metadata, infer roles from data
            return(ov_infer_player_roles(x, target_team, method = "data"))
        }
    } else if (method %in% c("shm", "smh")) {
        role_ord <- if (method == "shm") c("setter", "outside", "middle", "opposite", "outside", "middle") else c("setter", "middle", "outside", "opposite", "middle", "outside")
        xs <- x %>% dplyr::filter(.data$skill == "Serve", is_target_team(.data$home_team) | is_target_team(.data$visiting_team)) ## start of each rall
        roles <- bind_rows(lapply(1:6, function(sp) { ## iterate over setter positions
            temp <- xs %>% dplyr::filter(is_target_team(.data$home_team), .data$home_setter_position == sp)
            bind_rows(lapply(1:6, function(pos) {
                ## the player in position pos will be role_ord[do_rot(pos, -(sp - 1))]
                temp %>% dplyr::count(.data[[paste0("home_player_id", pos)]]) %>%
                    mutate(role = role_ord[do_rot(pos, -(sp - 1))]) %>% dplyr::rename(player_id = paste0("home_player_id", pos))
            }))
        })) %>%
            ## and same for visiting team
            bind_rows(lapply(1:6, function(sp) {
                temp <- xs %>% dplyr::filter(is_target_team(.data$visiting_team), .data$visiting_setter_position == sp)
                bind_rows(lapply(1:6, function(pos) {
                    ## the player in position pos will be role_ord[do_rot(pos, -(sp - 1))]
                    temp %>% dplyr::count(.data[[paste0("visiting_player_id", pos)]]) %>%
                        mutate(role = role_ord[do_rot(pos, -(sp - 1))]) %>% dplyr::rename(player_id = paste0("visiting_player_id", pos))
                }))
            })) %>%
            group_by(.data$player_id, .data$role) %>% dplyr::summarize(n = sum(.data$n), .groups = "drop")
        roles <- roles %>% group_by(.data$player_id) %>% mutate(N = sum(.data$n)) %>% dplyr::slice_max(order_by = .data$n) %>%
            ## mutate(score = .data$n / .data$N) %>%
            ungroup
        ## instead of n / N as the score, calculate the lower bound of the confidence interval of that proportion
        ## doing this will give a lower score on roles that are inferred from smaller numbers of samples
        ## (in a semi-principled way, we are ignoring non-independence but it doesn't really matter that much)
        roles$score <- sapply(seq_len(nrow(roles)), function(i) suppressWarnings(prop.test(roles$n[i], roles$N[i])$conf.int[1]))
        roles <- roles %>% dplyr::select("player_id", "role", "score")
    } else {
        ## data method
        if (is.list(x) && all(vapply(x, function(z) "meta" %in% names(z), FUN.VALUE = TRUE))) {
            ## have been given list, presumably of datavolley objects
            ## reformat plays parts to data.frame
            x <- do.call(rbind, lapply(x, plays))
        }
        ## check that target team actually appears in the data
        if (!any(is_target_team(x$team))) stop("target team does not appear in the data")
        if (!"serving_team" %in% names(x)) stop("add serving_team column to x for method \"data\"")

        pids <- x %>% dplyr::filter(is_target_team(.data$team)) %>% dplyr::select("player_id") %>% na.omit %>% distinct
        replace_nas <- function(z, with = 0) replace(z, is.na(z), with)
        replace_infs <- function(z, with = 0) replace(z, is.infinite(z), with)

        ## first identify setters
        x <- ov_augment_plays(x, to_add = "setters", use_existing = FALSE)
        xs <- x %>% dplyr::filter(.data$skill == "Serve")
        setters <- bind_rows(lapply(1:6, function(i) {
            bind_rows(
                xs %>% dplyr::filter(is_target_team(.data$home_team), .data$home_setter_position == i) %>%
                dplyr::reframe(player_id = c(.data$home_player_id1, .data$home_player_id2, .data$home_player_id3,
                                             .data$home_player_id4, .data$home_player_id5, .data$home_player_id6),
                               is_setter = c(.data$home_player_id1 == .data$home_setter_id, .data$home_player_id2 == .data$home_setter_id, .data$home_player_id3 == .data$home_setter_id,
                                             .data$home_player_id4 == .data$home_setter_id, .data$home_player_id5 == .data$home_setter_id, .data$home_player_id6 == .data$home_setter_id)),
                xs %>% dplyr::filter(is_target_team(.data$visiting_team), .data$visiting_setter_position == i) %>%
                dplyr::reframe(player_id = c(.data$visiting_player_id1, .data$visiting_player_id2, .data$visiting_player_id3,
                                             .data$visiting_player_id4, .data$visiting_player_id5, .data$visiting_player_id6),
                               is_setter = c(.data$visiting_player_id1 == .data$visiting_setter_id, .data$visiting_player_id2 == .data$visiting_setter_id, .data$visiting_player_id3 == .data$visiting_setter_id,
                                             .data$visiting_player_id4 == .data$visiting_setter_id, .data$visiting_player_id5 == .data$visiting_setter_id, .data$visiting_player_id6 == .data$visiting_setter_id))
            )
        })) %>%
            group_by(.data$player_id) %>% dplyr::summarize(setter_N = n(), setter_actions = sum_narm(.data$is_setter), .groups = "drop")

        ## by player, total ball touches and whether they were recorded in the on-court lineup
        ## we ignore serves, so that we don't get tripped up by liberos serving in e.g. NCAA competitions
        touches <- x %>% dplyr::filter(is_target_team(.data$team), !is.na(.data$player_id), !is.na(.data$skill),
                                       .data$skill != "Serve") %>%
            group_by(.data$player_id) %>%
            dplyr::summarize(n_touches = n(),
                             pid_on_court = sum_narm((.data$team == .data$home_team &
                                                      (.data$home_player_id1 == .data$player_id | .data$home_player_id2 == .data$player_id | .data$home_player_id3 == .data$player_id) |
                                                      .data$home_player_id4 == .data$player_id | .data$home_player_id5 == .data$player_id | .data$home_player_id6 == .data$player_id) |
                                                     (.data$team == .data$visiting_team &
                                                      (.data$visiting_player_id1 == .data$player_id | .data$visiting_player_id2 == .data$player_id | .data$visiting_player_id3 == .data$player_id) |
                                                      .data$visiting_player_id4 == .data$player_id | .data$visiting_player_id5 == .data$player_id | .data$visiting_player_id6 == .data$player_id)),
                             .groups = "drop")
        ## a player with a high number of touches where they are NOT listed on court is probably a libero

        ## look at where each player attacks from
        x$target_setter_position <- x$home_setter_position
        x$target_setter_position[is_target_team(x$visiting_team)] <- x$visiting_setter_position[is_target_team(x$visiting_team)]

        attacks <- dplyr::filter(x, is_target_team(.data$team), .data$skill == "Attack", !.data$attack_code %in% setter_tip_codes) %>%
            mutate(p1r = .data$target_setter_position == 1 & .data$serving_team != .data$team)
        if (mean(is.na(attacks$start_zone)) > 0.3) warning("more than 30% of attacks are missing their start_zone, results for method = 'data' may be inaccurate")
        ## tabulate the number of attacks by this player in situation X alongside the total number of team attacks made when that player was in the appropriate position to potentially make those attacks
        roles <- bind_rows(lapply(pids$player_id, function(pid) {
            attacks %>% mutate(pid_front = ((.data$team == .data$home_team & (.data$home_player_id2 == pid | .data$home_player_id3 == pid | .data$home_player_id4 == pid)) | (.data$team == .data$visiting_team & (.data$visiting_player_id2 == pid | .data$visiting_player_id3 == pid | .data$visiting_player_id4 == pid))),
                               pid_back = ((.data$team == .data$home_team & (.data$home_player_id1 == pid | .data$home_player_id5 == pid | .data$home_player_id6 == pid)) | (.data$team == .data$visiting_team & (.data$visiting_player_id1 == pid | .data$visiting_player_id5 == pid | .data$visiting_player_id6 == pid)))) %>%
                dplyr::summarize(
                           ## attacks from zone 4 while in P1 rec
                           ta4_f_p1r = sum_narm(.data$start_zone == 4 & .data$pid_front & .data$p1r),
                           att4_p1r = sum_narm(.data$player_id == pid & .data$start_zone == 4 & .data$pid_front & .data$p1r),

                           ## other rotations
                           ta4_f_not_p1r = sum_narm(.data$start_zone == 4 & .data$pid_front & !.data$p1r),
                           att4_not_p1r = sum_narm(.data$player_id == pid & .data$start_zone == 4 & .data$pid_front & !.data$p1r),

                           ## right-side attacks excl zone 2 in P1 reception
                           taR = sum_narm(.data$start_zone == 2 & .data$pid_front & !.data$p1r) + sum_narm(.data$start_zone == 9 & .data$pid_back),
                           attR = sum_narm(.data$player_id == pid & .data$start_zone == 2 & .data$pid_front & !.data$p1r) + sum_narm(.data$player_id == pid & .data$start_zone == 9 & .data$pid_back),

                           ## pipes
                           ta8_b = sum_narm(.data$start_zone == 8 & .data$pid_back),
                           att8 = sum_narm(.data$player_id == pid & .data$start_zone == 8 & .data$pid_back),

                           ## middle attacks: zone 3 or quick or slide
                           ta3_f = sum_narm((.data$start_zone == 3 | grepl("(Quick|Slide ball attack)", .data$skill_type)) & .data$pid_front),
                           att3 = sum_narm(.data$player_id == pid & (.data$start_zone == 3 | grepl("(Quick|Slide ball attack)", .data$skill_type)) & .data$pid_front),

                           player_id = pid)
        }))
        ## could do similar for block touches, but it's not clear that this will really add more information

        ## use in-system sets to downweight non-setter roles being confused as setter?

        ## we use the *lower* confidence interval bound as our estimate of the proportion
        ## this is a not-particularly-principled way to downweight values based on very few data points (e.g. 1 from 1) while still retaining a high score for players who made a lot of actions consistent with a given role
        roles <- roles %>% full_join(setters, by = "player_id") %>%
            full_join(touches, by = "player_id") %>%
            ## and score per role
            mutate(outside_score = replace_nas(prop.lower(.data$att8 + .data$att4_not_p1r, .data$ta4_f_not_p1r + .data$ta8_b)),
                   opposite_score = replace_nas(prop.lower((.data$ta8_b - .data$att8) + .data$att4_p1r + .data$attR, .data$ta4_f_p1r + .data$ta8_b + .data$taR)),
                   middle_score = replace_nas(prop.lower(.data$att3, .data$ta3_f)),
                   setter_score = replace_nas(prop.lower(.data$setter_actions, .data$setter_N)),
                   libero_score = replace_nas(prop.lower(.data$n_touches - .data$pid_on_court, .data$n_touches)))##%>%
        ## pick the role with the highest score
        role_idx <- max.col(roles[, c("setter_score", "middle_score", "outside_score", "opposite_score", "libero_score")])
        role_idx[is.na(role_idx)] <- 6L
        temp <- rowSums(roles[, c("setter_score", "middle_score", "outside_score", "opposite_score", "libero_score")], na.rm = TRUE)
        role_idx[is.na(temp) | temp < 0.01] <- 6L ## to NA
        roles$role <- c("setter", "middle", "outside", "opposite", "libero", NA_character_)[role_idx]
        roles$score <- NA_real_
        nnaidx <- !is.na(roles$role)
        roles$score[nnaidx] <- apply(roles[nnaidx, c("setter_score", "middle_score", "outside_score", "opposite_score", "libero_score")], 1, max)
        roles <- roles %>% dplyr::select("player_id", "role", "score")
    }
    roles
}

prop.lower <- function(x, n) suppressWarnings(vapply(seq_along(x), function(i) if (is.na(x[i]) || n[i] < 1) NA_real_ else prop.test(x[i], n[i])$conf.int[1], FUN.VALUE = 0.0))
