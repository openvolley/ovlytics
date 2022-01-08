#' Add some extra columns to a plays object
#'
#' @param x data.frame: the `plays` data.frame as returned by [datavolley::read_dv()] or [peranavolley::pv_read()]
#' @param to_add character: columns to add
#' - "receiving_team" adds the receiving team name
#' - "touch_summaries" adds a number of columns named "ts_*" that summarize a team touch (e.g. columns "ts_pass_quality", "ts_pass_evaluation_code" give the pass quality and pass evaluation code of the reception or dig associated with a given team touch)
#' - "followed" adds the columns "followed_timeout", "followed_technical_timeout", and "followed_sub"
#' - "setters" adds the columns "home_setter_id", "visiting_setter_id" (the player IDs of the home and visiting setter on court), and "setter_id", "setter_position", and "setter_front_back" (the player ID and position of the setter from the team performing the current action)
#' @param use_existing logical: if `TRUE` and all of the columns associated with a given `to_add` choice are already present in `x`, they won't be re-generated
#'
#' @return `x` with the extra columns added
#'
#' @export
ov_augment_plays <- function(x, to_add = c("receiving_team", "touch_summaries", "setters"), use_existing = TRUE) {
    assert_that(is.character(to_add))
    assert_that(is.flag(use_existing), !is.na(use_existing))
    to_add <- tolower(to_add)
    known_to_add <- c("receiving_team", "touch_summaries", "setters", "followed") ## the allowed values
    if (!all(to_add %in% known_to_add)) {
        warning("unrecognized 'to_add' values, ignoring: ", paste0(setdiff(to_add, known_to_add)))
    }

    if ("receiving_team" %in% to_add && (!"receiving_team" %in% names(x) || !use_existing)) {
        x <- dplyr::mutate(x, receiving_team = case_when(.data$serving_team %eq% .data$home_team ~ .data$visiting_team,
                                                  .data$serving_team %eq% .data$visiting_team ~ .data$home_team))
    }

    if ("touch_summaries" %in% to_add && (!all(c("ts_pass_quality", "ts_set_error", "ts_pass_evaluation_code") %in% names(x)) || !use_existing)) {
        x <- x[, setdiff(names(x), c("ts_pass_quality", "ts_set_error", "ts_pass_evaluation_code"))]
        if (!"freeball_over" %in% names(x)) {
            ## "Freeball" skill can be used both for sending a freeball to the opposition as well as receiving one, so disambiguate these usages
            x <- dplyr::mutate(x, freeball_over = .data$skill %eq% "Freeball" & lead(.data$match_id) %eq% .data$match_id & lead(.data$set_number) %eq% .data$set_number & !lead(.data$team) %eq% .data$team)
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

    if ("setters" %in% to_add && (!all(c("home_setter_id", "visiting_setter_id", "setter_id", "setter_position", "setter_front_back") %in% names(x)) || !use_existing)) {
        x <- x[, setdiff(names(x), c("home_setter_id", "visiting_setter_id", "setter_id", "setter_position", "setter_front_back"))]
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
