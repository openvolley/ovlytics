#' Tabulate setter repeat patterns
#'
#' Note: analysis is done on the basis of attack actions, and simply assumes that the setter on court made the set.
#'
#' @param x data.frame: the `plays` data.frame as returned by [datavolley::read_dv()] or [peranavolley::pv_read()]
#' @param setter_id string: (optional) the player ID of the setter to analyze (or provide `setter_name`). If neither `setter_id` nor `setter_name` are provided, all setters will be analyzed separately, and collated results returned
#' @param setter_name string: (optional) the name of the setter to analyze (ignored if `setter_id` is provided). If neither `setter_id` nor `setter_name` are provided, all setters will be analyzed separately, and collated results returned
#' @param exclude_attacks character: vector of attack codes to exclude
#' @param exclude_negative_reception logical: if `TRUE`, exclude attacks following poor reception (likely to be out-of-system and therefore might not represent attacks on which the setter had genuine options)
#' @param exclude_highballs logical: if `TRUE`, exclude highball attacks (likely to be out-of-system and therefore might not represent attacks on which the setter had genuine options)
#'
#' @return A data.frame with columns "team", "setter_name", "setter_id", "player_name", "player_id", "category", "opportunities", "repeats", "repeat%"
#'
#' @examples
#'
#' x <- plays(ovdata_example("NCA-CUB", as = "parsed"))
#' set_reps <- ov_setter_repetition(x, setter_name = "LOLETTE RODRIGUEZ")
#'
#' library(ggplot2)
#' ggplot(set_reps, aes(x = player_name, y = `repeat%`)) + geom_col() +
#'     geom_text(aes(x = player_name, label = paste0("N=", opportunities)),
#'               angle = 90, y = 100, hjust = 1, inherit.aes = FALSE) +
#'     facet_wrap(~category) +
#'     theme_bw() +
#'     theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
#'     labs(x = NULL, y = "Repeat percentage")
#'
#' @export
ov_setter_repetition <- function(x, setter_id, setter_name, exclude_attacks = c("PP", "PR", "P2"), exclude_negative_reception = TRUE, exclude_highballs = FALSE) {
    assert_that(is.data.frame(x))
    assert_that(all(c("home_setter_position", "visiting_setter_position", "team", "skill") %in% names(x)))
    if (!"setter_id" %in% names(x) || isTRUE(exclude_negative_reception)) x <- ov_augment_plays(x, c("touch_summaries", "setters"))
    if (missing(setter_id) || length(setter_id) < 1 || !nzchar(setter_id) || is.na(setter_id)) {
        if (!missing(setter_name)) {
            assert_that(is.string(setter_name), !is.na(setter_name), nzchar(setter_name))
            setter_id <- distinct(x[x$player_name %eq% setter_name, c("player_id", "player_name")])
            if (nrow(setter_id) == 1) {
                setter_id <- setter_id$player_id
            } else {
                stop("setter_name '", setter_name, "' could not be resolved to a single player_id")
            }
        } else {
            ## do all setters separately and collate the results
            all_sid <- unique(na.omit(x$setter_id))
            return(bind_rows(lapply(all_sid, function(sid) ov_setter_repetition(x, setter_id = sid, exclude_attacks = exclude_attacks, exclude_negative_reception = exclude_negative_reception, exclude_highballs = exclude_highballs))))
        }
    }
    assert_that(is.string(setter_id), !is.na(setter_id), nzchar(setter_id))
    assert_that(length(exclude_attacks) < 1 || is.character(exclude_attacks))
    assert_that(is.flag(exclude_highballs), !is.na(exclude_highballs))
    assert_that(is.flag(exclude_negative_reception), !is.na(exclude_negative_reception))

    x <- mutate(x, attack_setter_id = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" & lag(.data$team) == .data$team ~ lag(.data$player_id)))
    ## exclude PP, PR, P2 (and perhaps other) attacks here
    ax <- dplyr::filter(x, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & !.data$setter_id == .data$player_id)
    if (isTRUE(exclude_highballs)) ax <- dplyr::filter(ax, !grepl("High ball", .data$skill_type))
    if (isTRUE(exclude_negative_reception)) ax <- dplyr::filter(ax, !(.data$phase %eq% "Reception" & .data$ts_pass_quality %eq% "Poor"))

    ## filter to setter on court
    target_sid <- setter_id
    ax <- dplyr::filter(ax, .data$setter_id %in% target_sid)
    ## also exclude sets not actually made by this setter, if sets have been scouted
    ax <- dplyr::filter(ax, is.na(.data$attack_setter_id) | .data$attack_setter_id == target_sid)
    if (nrow(ax) < 1) {
        warning("no data for setter ID ", target_sid)
        return(NULL)
    }
    ## augment setter_name
    sidn <- dplyr::rename(distinct(x[, c("team_id", "player_id", "player_name")]), setter_id = "player_id", setter_name = "player_name")
    temp <- nrow(ax)
    ax <- left_join(ax, sidn, by = c("team_id", "setter_id"))
    if (nrow(ax) != temp) stop("there are non-unique player_id entries")

    ## total repetitions = number of times a player was set twice in a row (in the same match and set - but a "repeat" could be in the next rally)
    ## repeat after kill, after error etc are the same

    ## note that we need to count the number of times each player made an attack that wasn't the last attack in the set (because if it was the last, there is no opportunity to make a repeat, so we don't want it included here as an opportunity).

    set_reps <- mutate(group_by(ax, .data$match_id, .data$set_number, .data$team, .data$setter_id),
                       next_was_repeat = lead(.data$player_id) == .data$player_id,
                       not_last_attack = row_number() < n(),
                       was_kill = .data$evaluation == "Winning attack",
                       was_error = .data$evaluation == "Error",
                       was_blocked = .data$evaluation == "Blocked")
    ## now summarize by player (attacker) and setter
    set_reps <- ungroup(dplyr::summarize(group_by(set_reps, .data$team, .data$setter_name, .data$setter_id, .data$player_name, .data$player_id),
                                         category = c("Overall", "After kill", "After error", "After blocked"),
                                         ## calculate the number of opportunities for repeating that we had
                                         opportunities = c(sum(.data$not_last_attack), sum(.data$was_kill & .data$not_last_attack), sum(.data$was_error & .data$not_last_attack), sum(.data$was_blocked & .data$not_last_attack)),
                                         ## and the number of repeats from those opportunities
                                         repeats = c(sum(.data$next_was_repeat, na.rm = TRUE), sum(.data$was_kill & .data$next_was_repeat, na.rm = TRUE), sum(.data$was_error & .data$next_was_repeat, na.rm = TRUE), sum(.data$was_blocked & .data$next_was_repeat, na.rm = TRUE)),
                                         ## and finally the repeat %
                                         `repeat%` = .data$repeats/.data$opportunities*100))

    ## for 'repeats within a rally" it's basically the same, but we group by match and point rather than match and set
    set_reps_rallies <- mutate(group_by(ax, .data$match_id, .data$point_id, .data$team, .data$setter_id),
                               not_last_attack = row_number() < n(),
                               was_repeat = lag(.data$player_id, default = "") == .data$player_id)
    ## summarize by player and setter
    set_reps_rallies <- ungroup(dplyr::summarize(group_by(set_reps_rallies, .data$team, .data$player_id, .data$player_name, .data$setter_id, .data$setter_name),
                                                 category = "Within rally",
                                                 opportunities = sum(.data$not_last_attack), ## the number of opportunities for a player is the number of attacks that they made, excluding the last attack of each rally (because there is no opportunity to repeat after these)
                                                 repeats = sum(.data$was_repeat),
                                                 `repeat%` = .data$repeats/.data$opportunities*100))

    set_reps <- dplyr::select(set_reps, "team", "setter_name", "setter_id", everything())
    dplyr::arrange(bind_rows(set_reps, set_reps_rallies), .data$team, .data$setter_id, .data$player_name, .data$player_id)
}
