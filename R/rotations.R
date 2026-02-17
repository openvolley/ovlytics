#' Infer the role of each player
#'
#' @param x : a datavolley object (as returned by [datavolley::dv_read()]), a list of datavolley objects, or the `plays` component of a datavolley object
#' @param target_team string or function: team to report on. If this is a function, it should return `TRUE` when passed the target team name
#' @param method string: one of
#' - "meta" - rely on player metadata, i.e. the team rosters
#' - "SHM" - assume a setter-hitter-middle rotation order. When the setter is in position 1 the outside hitters are in 2 and 5, and middles in 3 and 6
#' - "SMH"  - setter-middle-hitter rotation order. When the setter is in position 1 the middles are in 2 and 5, and outside hitters in 3 and 6
#' - or "data" - try and identify player roles based on the ball touches that each player makes.
#' Method "meta" is the default if a datavolley object or list of objects is provided. Note that "SHM" and "SMH" cannot identify liberos, because they do not appear in the point-by-point on-court lineups.
#' @param fall_back logical: if `TRUE` and `method` is "meta" and x is a single datavolley object BUT player roles are not provided in the DataVolley file metadata section, fall back to method "data"
#' @param setter_tip_codes character: vector of attack combination codes that correspond to setter tips. Only relevant if `method` is "data"
#'
#' @return A data.frame with columns `player_id`, `role`, and `score` (the confidence in the role assignment)
#'
#' @examples
#' x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
#' ## guess roles according to the actions that the players made
#' rx <- ov_infer_player_roles(x, target_team = "Nova KBM Branik", method = "data")
#'
#' @export
ov_infer_player_roles <- function(x, target_team, method, fall_back = TRUE, setter_tip_codes = c("PP")) {
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
        ## look at where each player attacks from
        ## exclude p1 reception here, since attacking in this mode is likely to have players out of position
        x$target_setter_position <- x$home_setter_position
        x$target_setter_position[is_target_team(x$visiting_team)] <- x$visiting_setter_position[is_target_team(x$visiting_team)]

        pids <- x %>% dplyr::filter(is_target_team(.data$team)) %>% dplyr::select("player_id") %>% na.omit %>% distinct
        replace_nas <- function(z, with = 0) replace(z, is.na(z), with)

        ## by player, proportion of this players hits that are quicks or slides
        quick_hits <- dplyr::filter(x, .data$skill %eq% "Attack" & is_target_team(.data$team)) %>%
            group_by(.data$player_id) %>% dplyr::summarize(quick = mean(grepl("Quick|Slide ball attack", .data$skill_type))) %>%
            full_join(pids, by = "player_id") %>% replace_nas()

        ## setters - will set, obviously, and attacks should be scouted as dumps
        ## by player, proportion of team touches in which this player made a set and quick/slide ball set
        ## liberos will set (probably high balls but not quicks/slides)
        sets <- dplyr::filter(x, is_target_team(.data$team)) %>%
            dplyr::mutate(ttid = paste0(.data$team_touch_id, .data$match_id)) %>%
            group_by(.data$player_id) %>%
            dplyr::summarize(ntt = length(unique(na.omit(.data$ttid))),
                             set_rate = sum(.data$skill %eq% "Set")/.data$ntt,
                             quick_set_rate = sum(grepl("Quick|Slide ball set", .data$skill_type))/.data$ntt,
                             high_set_rate = sum(.data$skill_type %eq% "High ball set")/.data$ntt) %>%
            dplyr::filter(!is.na(.data$player_id))

        ## no serves for liberos
        serves <- dplyr::filter(x, is_target_team(.data$team)) %>%
            mutate(ttid = paste0(.data$team_touch_id, .data$match_id)) %>%
            group_by(.data$player_id) %>% 
            dplyr::summarize(ntt = length(unique(na.omit(.data$ttid))),
                             serve_rate = sum(.data$skill %eq% "Serve")/.data$ntt) %>%
            dplyr::filter(!is.na(.data$player_id))

        ## count blocks (expect e.g. no blocks for liberos), blocks on quick attacks (likely to be blocked by middles)
        blocks1 <- dplyr::filter(x, is_target_team(.data$team)) %>%
            mutate(ttid = paste0(.data$team_touch_id, .data$match_id)) %>%
            group_by(.data$player_id) %>% 
            dplyr::summarize(ntt = length(unique(na.omit(.data$ttid))),
                             block_rate = sum(.data$skill %eq% "Block")/.data$ntt) %>%
            dplyr::filter(!is.na(.data$player_id))
        blocks2 <- dplyr::filter(x, is_target_team(.data$team) & .data$skill == "Block") %>%
            group_by(.data$player_id) %>%
            dplyr::summarize(quick_block_rate = mean(grepl("Quick|Slide ball block", .data$skill_type))) %>%
            full_join(pids, by = "player_id") %>% replace_nas()

        ## for outsides, expect attacks from 4 and 8, very few from 3
        ##   in P1/recep, attacks from 2
        ##   if standard roles, also very few from 9 (but back row might be variable??)
        ## for opposites, expect attacks from 2 and 9
        ##   in P1/recep, some from 4
        ##   if standard roles, hit from 9 in back row
        ## setters, tips should be the majority of attacks, plus look at set characteristics, above
        attacks <- dplyr::filter(x, is_target_team(.data$team) & .data$skill == "Attack")
        if (mean(is.na(attacks$start_zone)) > 0.3) warning("more than 30% of attacks are missing their start_zone, results for method = 'data' may be inaccurate")
        attacks <- dplyr::filter(attacks, !(.data$target_setter_position == 1 & !is_target_team(.data$serving_team))) %>% ## not P1 reception
            group_by(.data$player_id) %>%
            dplyr::summarize(n_attacks = n(),
                             att2 = mean(.data$start_zone %eq% 2),
                             att3 = mean(.data$start_zone %eq% 3),
                             att4 = mean(.data$start_zone %eq% 4),
                             att7 = mean(.data$start_zone %eq% 7),
                             att8 = mean(.data$start_zone %eq% 8),
                             att9 = mean(.data$start_zone %eq% 9),
                             atthb4 = mean(.data$start_zone %eq% 4 & .data$skill_type %eq% "High ball attack"),
                             dumps = mean(.data$attack_code %in% setter_tip_codes)) %>%
            full_join(pids, by = "player_id") %>% replace_nas()

        attacks1 <- dplyr::filter(x, is_target_team(.data$team)) %>%
            mutate(ttid = paste0(.data$team_touch_id, .data$match_id)) %>%
            group_by(.data$player_id) %>%
            dplyr::summarize(ntt = length(unique(na.omit(.data$ttid))),
                             attack_rate = sum(.data$skill %eq% "Attack")/.data$ntt) %>%
            dplyr::filter(!is.na(.data$player_id))

        attacks_p1r <- dplyr::filter(x, is_target_team(.data$team) & .data$skill=="Attack"
                                     & (.data$target_setter_position == 1 & !is_target_team(.data$serving_team))) %>% ## P1 reception
            group_by(.data$player_id) %>%
            dplyr::summarize(n_attacks_p1r = n(),
                             att2_p1r = mean(.data$start_zone %eq% 2),
                             att4_p1r = mean(.data$start_zone %eq% 4),
                             atthb4_p1r = mean(.data$start_zone %eq% 4 & .data$skill_type %eq% "High ball attack")) %>%
            full_join(pids, by = "player_id") %>% replace_nas()

        ## TODO: exclude attack info from players with < minimum number of attacks? or down-weight these in the classifier?

        receps <- dplyr::filter(x, is_target_team(.data$team)) %>%
            mutate(ttid = paste0(.data$team_touch_id, .data$match_id)) %>%
            group_by(.data$player_id) %>%
            dplyr::summarize(ntt = length(unique(na.omit(.data$ttid))),
                             reception_rate = sum(.data$skill %eq% "Reception")/.data$ntt) %>%
            dplyr::filter(!is.na(.data$player_id))

        ## player profiles
        pp <- pids %>% left_join(quick_hits, by = "player_id") %>%
            left_join(sets %>% dplyr::select(-"ntt"), by = "player_id") %>%
            left_join(serves %>% dplyr::select(-"ntt"), by = "player_id") %>%
            left_join(blocks1 %>% dplyr::select(-"ntt"), by = "player_id") %>%
            left_join(blocks2, by = "player_id") %>%
            left_join(attacks %>% dplyr::select(-"att7", -"att8", -"att9"), by = "player_id") %>%
            left_join(attacks1 %>% dplyr::select(-"ntt"), by = "player_id") %>%
            left_join(attacks_p1r, by = "player_id") %>%
            left_join(receps %>% dplyr::select(-"ntt"), by = "player_id")

        gthordiff <- function(z, th) ifelse(z>th, 0, z-th)^2
        lthordiff <- function(z, th) ifelse(z<th, 0, z-th)^2

        ## now combine the above indicators into scores for each position. Weightings here are somewhat arbitrary
        setter_score <- function(z) 1 - pmin(sqrt(z$quick^2 + gthordiff(z$set_rate, 0.6) + gthordiff(z$quick_set_rate, 0.15) + gthordiff(z$high_set_rate, 0.1) + gthordiff(z$dumps, 0.75)), 1)
        libero_score <- function(z) 1 - pmin(sqrt(z$serve_rate^2 + z$block_rate^2 + z$quick_set_rate^2 + z$attack_rate^2 + gthordiff(z$reception_rate, 0.2)), 1) ##TODO add dig rate
        middle_score <- function(z) 1 - pmin(sqrt(gthordiff(z$quick, 0.9) + lthordiff(z$set_rate, 0.05) + gthordiff(z$quick_block_rate, 0.2) + z$reception_rate^2), 1)
        outside_score <- function(z) 1 - pmin(sqrt(gthordiff(z$att4, 0.7) + gthordiff(z$atthb4, 0.2) + lthordiff(z$att3, 0.06) + lthordiff(z$att2, 0.05) + gthordiff(z$reception_rate, 0.2)), 1)
        opp_score <- function(z) 1 - pmin(sqrt(gthordiff(z$att2, 0.7) + lthordiff(z$att3, 0.06) + gthordiff(z$att4_p1r, 0.7) + z$reception_rate^2), 1)

        pp$setter <- setter_score(group_by(pp, .data$player_id))
        pp$libero <- libero_score(group_by(pp, .data$player_id))
        pp$middle <- middle_score(group_by(pp, .data$player_id))
        pp$outside <- outside_score(group_by(pp, .data$player_id))
        pp$opposite <- opp_score(group_by(pp, .data$player_id))

        ## now classify each player
        whichrole <- function(z) {
            temp <- sort(c(setter = z$setter, libero = z$libero, middle = z$middle, outside = z$outside, opposite = z$opposite), decreasing = TRUE)
            data.frame(player_id = z$player_id, role = names(temp)[1], score = temp[1], stringsAsFactors = FALSE)
        }
        roles <- do.call(rbind, lapply(1:nrow(pp), function(z) whichrole(pp[z, ])))
        rownames(roles) <- NULL
    }
    roles
}
