#' Add information about liberos to a datavolley plays dataframe
#'
#' Add information about liberos to a datavolley plays dataframe
#'
#' DataVolley (`.dvw`) files do not include information about the libero on court. This function adds that information where it can. It assumes that the libero replaces the back-court middle blocker, except when a middle blocker is serving.
#'
#' The procedure is broadly:
#' - all recorded ball touches are checked on a per-set basis for each team: if there were no ball touches by any libero (for a given team) during a set, it is assuumed that no libero was used in that set by that team. If ball touches were recorded only for one libero, it is assumed that only a single libero was used by that team in that set, even if that team has multiple liberos available
#' - this information is then refined on a rally-by-rally basis: in a rally where the libero made a ball touch we can be sure that this libero was on court
#'
#' The function returns a copy of `x` with two columns added:
#' - the `libero_on_court` column will contain TRUE if a libero was on court for that data row, or FALSE if not on court, or NA if unknown
#' - the `libero_id` column will contain the `player_id` of the libero that was on court for that data row. If no libero was on court, the value "_NO_LIBERO_" will be used. If a libero was known to be on court but we could not identify who it was, the value "_MULTIPLE_LIBEROS_" will be used
#'
#' @param x data.frame: the `plays` data.frame as returned by [datavolley::read_dv()] or [peranavolley::pv_read()]. Can be `plays` from multiple matches combined using `rbind` or `bind_rows`
#' @param liberos character: vector of `player_id` values of liberos. Or a data.frame with `match_id`, `player_id`, `team`, `role` columns (only rows with `role` == "libero" will be used as liberos). The latter form will be necessary if `x` contains data from multiple matches and a player plays as a libero in some matches but other positions in other matches
#' @param middles character: vector of `player_id` values of middle blockers. Or a data.frame with `match_id`, `player_id`, `team`, `role` columns (only rows with `role` == "middle" will be used as middles)
#'
#' @return A data.frame of x augmented with columns `libero_id` and `libero_on_court`. See Details for the contents of these columns
#'
#' @examples
#' x <- ovdata_example("190301_kats_beds", as = "parsed")
#'
#' ## identify our liberos and middles
#' liberos <- c(x$meta$players_h$player_id[which(x$meta$players_h$role == "libero")],
#'              x$meta$players_v$player_id[which(x$meta$players_v$role == "libero")])
#' middles <- c(x$meta$players_h$player_id[which(x$meta$players_h$role == "middle")],
#'              x$meta$players_v$player_id[which(x$meta$players_v$role == "middle")])
#'
#' ## infer libero info
#' px <- ov_augment_liberos(plays(x), liberos = liberos, middles = middles)
#'
#' @export
ov_augment_liberos <- function(x, liberos, middles) {
    if (inherits(x, "datavolley") || (is.list(x) && all(c("meta", "plays") %in% names(x)))) {
        ## silently accept an entire datavolley object, even though we allegedly only accept the plays part of it
        x <- datavolley::plays(x)
    }
    if (!all(c("match_id", "set_number", "team", "skill", "player_id") %in% names(x))) {
        stop("expecting a data frame of play-by-play data (e.g. a datavolleyplays object)")
    }
    x <- x[, setdiff(names(x), c("libero_id", "libero_on_court"))] ## discard existing columns if they exist

    liberos_as_df <- FALSE
    if (is.data.frame(liberos)) {
        liberos_as_df <- TRUE
        if (!all(c("match_id", "team", "player_id", "role") %in% names(liberos))) {
            stop("if `liberos` is a data frame, it must contain columns \"match_id\", \"team\", \"player_id\", and \"role\"")
        }
    } else {
        liberos <- na.omit(unique(liberos))
    }
    if (is.data.frame(middles)) {
        if (!all(c("match_id", "team", "player_id", "role") %in% names(middles))) {
            stop("if `middles` is a data frame, it must contain columns \"match_id\", \"team\", \"player_id\", and \"role\"")
        }
    } else {
        middles <- na.omit(unique(middles))
    }

    ## this only used if liberos is a simple char vector
    z12 <- function(player_id, targets = liberos) {
        temp <- intersect(player_id, targets)
        if (length(temp) < 1) {
            "_NO_LIBERO_"
        } else if (length(temp) > 1) {
            "_MULTIPLE_LIBEROS_"
        } else {
            temp
        }
    }
    ## or if liberos is a match-by-match data frame of player roles
    liblabel <- function(player_ids) {
        temp <- unique(na.omit(player_ids))
        if (length(temp) < 1) {
            "_NO_LIBERO_"
        } else if (length(temp) > 1) {
            "_MULTIPLE_LIBEROS_"
        } else {
            temp
        }
    }
    ## check set-by-set
    ## if only one libero is recorded on court in a given set (for a given team), assume they were on court for all relevant points in that set
    ## no liberos recorded in a given set, assume no libero at all for that set
    if (liberos_as_df) {
        lrx <- liberos %>% dplyr::filter(.data$role == "libero") %>%
            mutate(player_was_libero = TRUE) %>%
            dplyr::select("match_id", "player_id", "team", "player_was_libero") %>% distinct
        x <- x %>% left_join(lrx, by = c("match_id", "player_id", "team")) %>%
            mutate(player_was_libero = case_when(is.na(.data$player_was_libero) ~ FALSE,
                                                 TRUE ~ .data$player_was_libero))
        temp <- x %>% group_by(.data$match_id, .data$set_number, .data$team) %>%
            dplyr::filter(!is.na(.data$team)) %>%
            dplyr::summarize(libero_id = liblabel(.data$player_id[.data$player_was_libero])) %>%
            ungroup
    } else {
        temp <- x %>% group_by(.data$match_id, .data$set_number, .data$team) %>%
            dplyr::filter(!is.na(.data$team)) %>%
            dplyr::summarize(libero_id = z12(.data$player_id)) %>%
            ungroup
    }
    ## so libero_id is known, or _NO_LIBERO_, or _MULTIPLE_LIBEROS_
    mpl <- x %>% inner_join(temp, by = c("match_id", "team", "set_number")) %>%
        dplyr::select("match_id", "point_id", "team", "libero_id")

    ## also add points where libero makes a play, since we know they must be on court then
    if (liberos_as_df) {
        mpl <- bind_rows(mpl, x %>% dplyr::filter(.data$player_was_libero) %>% dplyr::select("match_id", "point_id", "team", libero_id = "player_id"))
    } else {
        mpl <- bind_rows(mpl, x %>% dplyr::filter(.data$player_id %in% liberos) %>% dplyr::select("match_id", "point_id", "team", libero_id = "player_id"))
    }
    mpl <- distinct(mpl)

    ## but now we might have "_MULTIPLE_LIBEROS_" plus the actual libero, or some other multiple-libero-labelling conflict
    mpl <- mpl %>% group_by(.data$match_id, .data$point_id, .data$team) %>% mutate(N = n()) %>% ungroup %>%
        dplyr::filter(!(.data$N > 1 & .data$libero_id %in% c("_MULTIPLE_LIBEROS_", "_NO_LIBERO_")))

    ## checks (though I don't think these can now ever be true due to the filter above)
    if (nrow(dplyr::filter(mpl, .data$N > 1, .data$libero_id == "_MULTIPLE_LIBEROS_")) > 0) {
        stop(libero_error_issue_text("we could not resolve a conflict in the data with multiple liberos"))
    }
    if (nrow(dplyr::filter(mpl, .data$N > 1, .data$libero_id == "_NO_LIBERO_")) > 0) {
        stop(libero_error_issue_text("we could not resolve a conflict in the data"))
    }
    mpl <- dplyr::select(mpl, -"N") %>% ungroup

    ## libero not on court when middle serving
    if (is.data.frame(middles)) {
        mrx <- middles %>% dplyr::filter(.data$role == "middle") %>%
            mutate(player_was_middle = TRUE) %>%
            dplyr::select("match_id", "player_id", "team", "player_was_middle") %>% distinct
        x <- x %>% left_join(mrx, by = c("match_id", "player_id", "team")) %>%
            mutate(player_was_middle = case_when(is.na(.data$player_was_middle) ~ FALSE,
                                                 TRUE ~ .data$player_was_middle))
        mp_ms <- x %>% dplyr::filter(.data$skill == "Serve", .data$player_was_middle) %>% dplyr::select("match_id", "point_id", "team") %>% mutate(ms = TRUE)
    } else {
        mp_ms <- x %>% dplyr::filter(.data$skill == "Serve", .data$player_id %in% middles) %>% dplyr::select("match_id", "point_id", "team") %>% mutate(ms = TRUE)
    }
    ## want `libero_on_court` values to be logical TRUE, FALSE, or NA
    ## and `libero_id` to be the player_id or _MULTIPLE_LIBEROS_ or _NO_LIBERO_, or NA where libero_on_court is NA
    mpl <- mpl %>% left_join(mp_ms, by = c("match_id", "point_id", "team")) %>%
        mutate(libero_on_court = case_when(!is.na(.data$ms) | .data$libero_id %eq% "_NO_LIBERO_" ~ FALSE, TRUE ~ TRUE)) %>%
        mutate(libero_id = case_when(.data$libero_on_court ~ .data$libero_id, TRUE ~ "_NO_LIBERO_")) %>%
        dplyr::select(-"ms")

    ## ensure that we can't have a libero_id that isn't "_NO_LIBERO_" if we don't have a libero on court
    chk <- which(!mpl$libero_on_court & !mpl$libero_id %eq% "_NO_LIBERO_")
    if (length(chk) > 0) {
        mpl$libero_id[chk] <- NA_character_
        mpl$libero_on_court[chk] <- NA
    }

    ## ensure that we can't have a libero_id that is "_NO_LIBERO_" if we have a libero on court
    chk <- which(mpl$libero_on_court & mpl$libero_id %eq% "_NO_LIBERO_")
    if (length(chk) > 0) {
        mpl$libero_id[chk] <- NA_character_
        mpl$libero_on_court[chk] <- NA
    }

    ## so this is match, point, team, libero
    ## if libero is known to be off court, `libero_on_court` will be FALSE (and "_NO_LIBERO_" as the `libero_id`)
    ## if libero is known to be on court, `libero_on_court` will be TRUE and `libero_id` will be the ID, "_MULTIPLE_LIBEROS_"
    ## if not known whether libero is on or off court, `libero_on_court` will be NA

    ## check for duplicate match/point/team but multiple rows

    mpl <- mpl %>% mutate(mpt = paste0(.data$match_id, "@", .data$point_id, "@", .data$team))
    ## internal checking
    ## chk1 <- setdiff(x %>% mutate(mpt = paste0(.data$match_id, "@", .data$point_id, "@", .data$team)) %>% pull("mpt"),mpl$mpt)
    ##  all just NA team rows
    ## cat("chk1 ",all(grepl("NA$",chk1)),"\n")
    chk2 <- duplicated(mpl$mpt)
    dup_mpt <- mpl$mpt[chk2]
    for (thisdup_mpt in dup_mpt) {
        idx <- mpl$mpt %eq% thisdup_mpt
        this_libs <- mpl$libero_id[idx]
        if (all(mpl$libero_on_court[idx])) {
            ## resolve unique libero_id if possible
            this_libs <- unique(this_libs)
            if (length(this_libs) == 1) {
                mpl$libero_id[idx] <- this_libs
                next
            }
            this_libs <- setdiff(this_libs, c("_NO_LIBERO_", "_MULTIPLE_LIBEROS_"))
            if (length(this_libs) == 1) {
                mpl$libero_id[idx] <- this_libs
                next
            }
            ## otherwise don't know!
            mpl$libero_id[idx] <- NA_character_
        } else if (!any(mpl$libero_on_court[idx])) {
            mpl$libero_id[idx] <- "_NO_LIBERO_"
        } else {
            mpl$libero_on_court[idx] <- NA
            mpl$libero_id[idx] <- NA_character_
        }
    }
    mpl <- distinct(mpl)
    ## internal checking
    ## chk2 <- duplicated(mpl$mpt)
    ## dup_mpt <- mpl$mpt[chk2]
    ## #cat("dup ",sum(chk2),"\n")
    ## if (sum(chk2)>0) cat("dups:",str(mpl[mpl$mpt %in% dup_mpt,]),"\n")
    mpl <- dplyr::select(mpl, -"mpt")

    ## x with libero stuff
    rowcount <- nrow(x)
    x <- left_join(x, mpl, by = c("match_id", "point_id", "team"))
    if (nrow(x) != rowcount) {
        stop(libero_error_issue_text("we ended up with more data rows than we started with"))
    }

    ## discard anything not on a ball-touch row
    idx <- is.na(x$skill) | x$skill %in% c("Timeout", "Technical timeout")
    x$libero_on_court[idx] <- NA
    x$libero_id[idx] <- NA_character_
    x
}

libero_error_issue_text <- function(errtxt) paste0("Sorry, there was an error in ov_augment_liberos: ", errtxt, ". If this error persists, please lodge an issue at https://github.com/openvolley/ovlytics/issues with a reproducible example")
