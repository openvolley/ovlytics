#' Create a summary table of a team's matches in a season
#'
#' @param xl list: list of datavolley objects (each as returned by [datavolley::dv_read()]
#' @param target_team string: the name of the target team. Only one of `target_team` or `target_team_id` is required
#' @param target_team_id string: the team ID of the target team. Ignored if `target_team` has been provided
#' @param show_by string: either "match date" (show each match according to its date) or "filename" (show each match according to its filename. This might be useful if the match dates are being parsed incorrectly by [datavolley::dv_read()])
#'
#' @return A tibble with columns "Opponent", "Date" (or "File"), "Result", "Set scores", and one column for sets 1 to 5
#'
#' @examples
#' ## trivial example of a single-match "season"
#' library(datavolley)
#' x <- dv_read(dv_example_file())
#' ov_season_table(list(x), target_team = home_team(x))
#'
#' @export
ov_season_table <- function(xl, target_team, target_team_id, show_by = "match date") {
    show_by <- match.arg(show_by, c("match date", "filename"))
    if (missing(target_team)) target_team <- NULL else target_team_id <- NULL
    if ("plays" %in% names(xl)) {
        ## it's a single datavolley match
        xl <- list(xl)
    }
    out <- bind_rows(lapply(xl, function(x) {
        tidx <- if (is.null(target_team_id)) which(x$meta$teams$team == target_team) else which(x$meta$teams$team_id == target_team_id)
        if (length(tidx) != 1) return(NULL)
        was_home_team <- isTRUE(x$meta$teams$home_away_team[tidx] == "*")
        opp <- x$meta$teams$team[3 - tidx]
        dt <- if (show_by == "match date") x$meta$match$date else basename(gsub("\\\\", "/", x$meta$filename))
        match_score <- paste(x$meta$teams$sets_won, collapse = "-")
        won_match <- x$meta$teams$won_match[tidx]
        set_results <- paste0(x$meta$result$score_home_team, "-", x$meta$result$score_visiting_team)
        set_results <- paste(set_results[which(x$meta$result$played)], collapse = ", ")
        home_won_set <- (x$meta$result$score_home_team > x$meta$result$score_visiting_team)[which(x$meta$result$played)]
        set_smry <- x$plays %>% dplyr::filter(.data$skill == "Serve") %>% group_by(.data$set_number) %>% slice(1L) %>%
            dplyr::summarize(home_served = .data$team_id == .data$home_team_id, home_setter_in = .data$home_setter_position, visiting_setter_in = .data$visiting_setter_position)
        set_smry <- if (was_home_team) {
            paste0(case_when(set_smry$home_served ~ "S", TRUE ~ "R"), "-", set_smry$home_setter_in, case_when(home_won_set ~ "*", TRUE ~ ""))
        } else {
            paste0(case_when(set_smry$home_served ~ "R", TRUE ~ "S"), "-", set_smry$visiting_setter_in, case_when(home_won_set ~ "", TRUE ~ "*"))
        }
        set_smry <- setNames(c(set_smry, rep("-", 5 - length(set_smry))), 1:5)
        bind_cols(tibble(Opponent = opp, Date = dt, Result = match_score, `Set scores` = set_results, `W/L` = won_match), as.list(set_smry))
    }))
    if (!is.null(out) && nrow(out) > 0 && show_by == "filename") names(out)[names(out) == "Date"] <- "Filename"
    out
}
