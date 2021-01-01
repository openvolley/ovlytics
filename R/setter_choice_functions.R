team_name_to_abbrev <- function(x, upper = FALSE) {
    ##gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1', x, perl = TRUE) ## doesn't like non-ascii (unicode) chars
    out <- vapply(stringr::str_split(x, "\\b"), function(z) paste0(stringr::str_trim(substr(z, 1, 1)), collapse = ""), FUN.VALUE = "", USE.NAMES = FALSE)
    if (upper) toupper(out) else out
}

#' Simulate a Bayesian Bandit choice for a given set of probabilities and a number of points
#'
#' @param dvw string or datavolley: a datavolley object as returned by [datavolley::dv_read()] or a path to datavolley file
#' @param play_phase character: one or both of "Reception", "Transition"
#' @param n_sim integer: number of simulations
#' @param priors numeric: prior distribution of the kill rate for the different attacking options
#' @param epsilon numeric: reward size
#' @param filter_sim logical:
#' @param attack_options string: either "use_data" or "use_history"
#' @param history_table data.frame: (only if `attack_options` is "use_history") the `prior_table` component of the object returned by [ov_create_history_table()]
#' @param attack_by string: either "code" or "zone"
#' @param exclude_attacks character: vector of attack codes to exclude
#' @param shiny_progress numeric: an optional two-element vector. If not `NULL` or `NA`, [shiny::setProgress()] calls will be made during simulation with `value`s in this range
#'
#' @seealso [ov_create_history_table()]
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' system.time({
#'   ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception",
#'                                          n_sim = 100, attack_by = "zone")
#' })
#' @export
ov_simulate_setter_distribution <- function(dvw, play_phase = c("Reception", "Transition"), n_sim = 500, priors = list(name = "beta", par1 = 1, par2 = 1),
                                            epsilon = 1, filter_sim = FALSE, attack_options = "use_data", history_table = NULL,
                                            attack_by = "code", exclude_attacks = c("PR"), shiny_progress = NULL) {
    ## TODO check input parms
    attack_by <- match.arg(attack_by, c("zone", "code"))
    attack_by_var <- switch(attack_by,
                            "code" = "attack_code",
                            "zone" = "start_zone")
    attack_options <- match.arg(attack_options, c("use_data", "use_history"))

    raw_data <- if (is.character(dvw)) dv_read(dvw) else dvw

    data <- ov_augment_plays(plays(raw_data), to_add = c("touch_summaries", "setters"))
    team_setter <- tidyr::drop_na(distinct(dplyr::select(dplyr::filter(data, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), "team", "setter_id")))
    data <- dplyr::filter(data, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase))

    sim <- actual <- rates <- NULL

    if ((is.null(history_table) || !is.data.frame(history_table) || nrow(history_table) < 1) && attack_options %eq% "use_history") stop("History table is missing or empty")

    do_shiny_progress <- tryCatch(!is.null(shiny_progress) && length(shiny_progress) == 2 && !any(is.na(shiny_progress)) && requireNamespace("shiny", quietly = TRUE), error = function(e) FALSE)
    n_outer <- nrow(distinct(team_setter[, c("team", "setter_id")]))
    spcount <- 0L
    cond <- NULL
    for (iTeam in unique(team_setter$team)) {
        iGameSetter <- dplyr::filter(team_setter, .data$team == iTeam)
        for (iSetter in iGameSetter$setter_id) {
            spcount <- spcount + 1L
            if (isTRUE(do_shiny_progress)) try(shiny::setProgress(value = spcount/n_outer*abs(diff(shiny_progress)) + min(shiny_progress)), silent = TRUE)
            data_game <- dplyr::filter(data, .data$team == iTeam & .data$setter_id == iSetter)

            if (nrow(data_game) < 20) next

            tbleChoice <- if (packageVersion("dplyr") >= "1.0.0") {
                              group_by(data_game, .data$setter_position, .data$ts_pass_evaluation_code, dplyr::across({{ attack_by_var }}))
                          } else {
                              dplyr::group_by_at(data_game, c("setter_position", "ts_pass_evaluation_code", attack_by_var))
                          }
            tbleChoice <- ungroup(dplyr::summarize(tbleChoice, KR = mean(.data$evaluation_code == "#"), n = n()))

            if (attack_options == "use_data") {
                this <- tbleChoice
            } else if (attack_options == "use_history") {
                this <- bind_rows(
                    dplyr::select(mutate(dplyr::filter(history_table, .data$team == iTeam & .data$setter_id == iSetter), n_s = .data$KR * .data$n), -"KR", -"team", -"setter_id"),
                    dplyr::select(mutate(tbleChoice, n_s = .data$KR * .data$n), -"KR"))
                if (packageVersion("dplyr") >= "1.0.0") {
                    this <- ungroup(dplyr::summarize(group_by(this, .data$setter_position, .data$ts_pass_evaluation_code, dplyr::across({{ attack_by_var }})), KR = sum(.data$n_s)/sum(.data$n), n = sum(.data$n)))
                } else {
                    this <- ungroup(dplyr::summarize(dplyr::group_by_at(this, c("setter_position", "ts_pass_evaluation_code", attack_by_var)), KR = sum(.data$n_s)/sum(.data$n), n = sum(.data$n)))
                }
            }
            probTable <- pivot_wider(dplyr::select(this, -"n"), names_from = {{ attack_by_var }}, values_from = .data$KR)
            if (FALSE) {
                ## use team_id in plot
                tableBB <- mutate(data_game, score = paste(.data$home_team_id, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team_id))
            } else {
                ## create a short team name automatically
                tableBB <- mutate(data_game, score = paste(team_name_to_abbrev(.data$home_team), .data$home_team_score,
                                                           "-",
                                                           .data$visiting_team_score, team_name_to_abbrev(.data$visiting_team)))
            }
            tableBB <- left_join(dplyr::select(tableBB, "set_number", "point_id", "score", "setter_position", "ts_pass_evaluation_code", {{ attack_by_var }}, "evaluation_code"), probTable, by = c("setter_position", "ts_pass_evaluation_code"))

            choice <- matrix(seq_len(ncol(tableBB) - 7), ncol = (ncol(tableBB) - 7), nrow = nrow(tableBB), byrow = TRUE)
            choice[which(is.na(as.matrix(tableBB[, seq(8, ncol(tableBB), by = 1)])))] <- NA

            probs <- as.matrix(tableBB[, seq(8, ncol(tableBB), by = 1)])
            probs[is.na(probs)] <- 0

            duration <- nrow(tableBB)

            ## preallocate vectors and assemble data frame outside of loop
            res_sim_num <- rep(seq_len(n_sim), each = duration)
            resn <- length(res_sim_num)
            res_ts_pass_evaluation_code <- rep(NA_character_, resn)
            res_setter_position <- rep(NA_integer_, resn)

            nArms <- ncol(probs)

            if (nrow(probs) < duration) probs <- probs[rep(1, duration), ]

            ##postPar_h = array(NA, dim = c(nArms, 2, n_sim))

            arm_seq <- reward_seq <- NULL

            for (nS in seq_len(n_sim)) {
                postPar <- matrix(c(priors$par1, priors$par2), ncol = nArms, nrow = 2, byrow = TRUE)
                ##rewards <- matrix(mapply(function(x, y) rbinom(n = 1, size = 1, prob = probs[x, y]),
                ##                         expand.grid(seq_len(duration), seq_len(nArms))[, 1], expand.grid(seq_len(duration), seq_len(nArms))[, 2]), nrow = duration, byrow = FALSE)
                ## faster version
                temp <- matrix(runif(duration * nArms), nrow = duration)
                rewards <- temp <= probs
                mode(rewards) <- "integer"
                if (is.null(choice)) choice <- matrix(seq_len(nArms), ncol = nArms, nrow = duration, byrow = TRUE)

                res_setter_position[res_sim_num == nS ] <- tableBB$setter_position
                res_ts_pass_evaluation_code[res_sim_num == nS] <- tableBB$ts_pass_evaluation_code

                for (d in seq_len(duration)) {
                    choiceD <- na.omit(choice[d, ])
                    theta <- vapply(choiceD, function(x) rbeta(1, postPar[1, x], postPar[2, x]), FUN.VALUE = numeric(1), USE.NAMES = FALSE)
                    theta_m <- max(theta)
                    arm_m <- choiceD[which.max(theta)]
                    arm_seq <- c(arm_seq, arm_m)
                    reward_seq <- c(reward_seq, rewards[d, arm_m])
                    postPar[, arm_m] <- postPar[, arm_m] + c(rewards[d, arm_m] * epsilon, epsilon - epsilon * rewards[d, arm_m])
                }
                ##postPar_h[,,nS] <- postPar
            }

            ## Bandit choice condiional on setter choices and attack outcomes
            res_cond <- matrix(NA_real_, nrow = duration, ncol = nArms)
            ObservedChoice <- match(tableBB[[attack_by_var]], colnames(probs))
            ObservedSuccess <- as.numeric(tableBB$evaluation_code == "#")
            postPar <- matrix(c(priors$par1, priors$par2), ncol = nArms, nrow = 2, byrow = TRUE)
            postParInit <- matrix(c(priors$par1, priors$par2), ncol = nArms, nrow = 2, byrow = TRUE)
            EKR <- matrix(NA_real_, nrow = duration, ncol = nArms)
            for (d in seq_len(duration)) {
                choiceD <- na.omit(choice[d, ])
                theta <- vapply(choiceD, function(x) rbeta(n_sim, postPar[1, x], postPar[2, x]), FUN.VALUE = numeric(n_sim), USE.NAMES = FALSE)
                meanTheta <- vapply(choiceD, function(x) postPar[1, x] / (postPar[1, x] + postPar[2, x]), FUN.VALUE = numeric(1), USE.NAMES = FALSE)
                propMax <- table(factor(apply(theta, 1, which.max), levels = seq_along(choiceD)))
                res_cond[d, choiceD] <- propMax / n_sim
                EKR[d, ] <- postPar[1,] / colSums(postPar[1:2, ])
                postPar[, ObservedChoice[d]] <- postPar[, ObservedChoice[d]] + c(ObservedSuccess[d] * epsilon, epsilon - ObservedSuccess[d] * epsilon)
            }
            res_cond_df <- data.frame(res_cond, stringsAsFactors = FALSE)
            colnames(res_cond_df) <- paste("Bandit choice", colnames(probs))
            EKR <- data.frame(EKR, stringsAsFactors = FALSE)
            colnames(EKR) <- paste("Expected KR", colnames(probs))

            cond <- bind_rows(cond, mutate(bind_cols(tableBB, res_cond_df, EKR), setter = iSetter, team = iTeam))

            res_attack_choice <- colnames(probs)[arm_seq]
            if (attack_by == "zone") res_attack_choice <- as.integer(res_attack_choice)
            res <- data.frame(sim_num = rep(seq_len(n_sim), each = duration), time = rep(seq_len(duration), n_sim),
                              chosen_arm = arm_seq, reward = reward_seq, ts_pass_evaluation_code = NA, setter_position = res_setter_position, attack_choice = res_attack_choice,
                              team = iTeam, setter = iSetter, stringsAsFactors = FALSE)

            tableBB$team <- iTeam
            tableBB$setter <- iSetter
            tbleChoice$team <- iTeam
            tbleChoice$setter <- iSetter

            sim <- bind_rows(sim, res)
            actual <- bind_rows(actual, tableBB)
            rates <- bind_rows(rates, tbleChoice)
        }
    }

    sim_filter <- NULL

    if (isTRUE(filter_sim)) {
        tbleC <- ungroup(dplyr::summarize(group_by(sim, .data$attack_choice, .data$setter_position, .data$ts_pass_evaluation_code, .data$sim_num, .data$team, .data$setter),
                                          KR = mean(.data$reward), n = n()))
        tbleC <- left_join(tbleC, dplyr::rename(rates, true_KR = "KR", true_n = "n"), by = c("attack_choice" = {{ attack_by_var }}, "setter_position",
                                                                                             "ts_pass_evaluation_code", "team", "setter"))
        summaryTC <- mutate(ungroup(tbleC), trueKR_up = .data$true_KR + 1.64*sqrt(.data$true_KR * (1 - .data$true_KR) / .data$true_n),
                            trueKR_down = .data$true_KR - 1.64*sqrt(.data$true_KR * (1 - .data$true_KR) / .data$true_n),
                            keepSim = case_when(.data$KR <= .data$trueKR_up & .data$KR >= .data$trueKR_down ~ TRUE,
                                                TRUE ~ FALSE))
        summaryTC <- mutate(unique(dplyr::select(dplyr::filter(summaryTC, !.data$keepSim), "sim_num", "team", "setter")), keepSim = FALSE)

        sim_filter <- dplyr::filter(left_join(sim, summaryTC, by = c("setter", "team", "sim_num")), is.na(.data$keepSim))
    }
    list(simulations = sim, actual = actual, rates = rates, filtered_simulations = sim_filter, attack_by_var = attack_by_var, raw_data = raw_data, conditional_simulations = cond)
}

#' Plot a simulated setter distribution
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param overlay_set_number boolean: if `TRUE`, overlay set number and score in the plot
#' @param font_size numeric: font size
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Transition",
#'                                           n_sim = 150, attack_by = "zone")
#' ov_plot_ssd(setter, overlay_set_number = TRUE)
#' @export
ov_plot_ssd <- function(ssd, overlay_set_number = FALSE, font_size = 11) {
    assert_that(is.flag(overlay_set_number), !is.na(overlay_set_number))
    bbtrajqi <- mutate(group_by(ssd$simulations, .data$sim_num, .data$team, .data$setter),
                       traj = cumsum(.data$reward))
    bbtrajqi <- ungroup(dplyr::summarize(group_by(bbtrajqi, .data$time, .data$team, .data$setter),
                                         trajqi05 = quantile(.data$traj, 0.05), trajqi95 = quantile(.data$traj, 0.95),
                                         trajqim = mean(.data$traj)))
    tbC <- mutate(group_by(ssd$actual, .data$team, .data$setter),
                  pts = cumsum(.data$evaluation_code == "#"), time = row_number())

    g <- ggplot(tbC) +
        geom_line(data = bbtrajqi, aes_string(x = "time", y = "trajqim", group = "setter"), col = "orange") +
        geom_ribbon(data = bbtrajqi, aes_string(x = "time", ymin = "trajqi05", ymax = "trajqi95", group = "setter"), col = "white", fill = "orange", alpha = 0.25) +
        geom_line(aes_string(x = "time", y = "pts", group = "setter")) +
        ##ggrepel::geom_label_repel(data = bbtraj %>% mutate(end_label = if_else(time == max(time), paste0("Bandit ", cumsum(kp), "pts"), NA_character_)), 
        ##                          aes(x = time, y = cumsum(kp), label = end_label),nudge_x = 3, nudge_y = 2, segment.size = .1, col = "orange")+
        labs(x = "Game history", y = "Cumulative points scored") + facet_wrap("team", scales = "free")

    if (overlay_set_number) {
        data_label <- ungroup(dplyr::select(dplyr::filter(group_by(ssd$actual, .data$set_number), .data$point_id == max(.data$point_id)),
                                            "set_number", "score"))
        data_label <- left_join(data_label, group_by(ssd$actual, .data$team, .data$setter, .data$set_number) %>%
                                            dplyr::summarize(time = n()) %>%
                                            group_by(.data$team, .data$setter) %>% mutate(max_time = cumsum(.data$time)) %>% ungroup(),
                                by = "set_number") %>%
            left_join(ungroup(group_by(tbC, .data$team, .data$setter) %>% dplyr::summarize(maxPts = max(.data$pts))), by = c("team", "setter")) %>%
            left_join(ungroup(group_by(bbtrajqi, .data$team, .data$setter) %>% dplyr::summarize(maxPts_sim = max(.data$trajqi95))), by = c("team", "setter"))
        g <- g + geom_vline(data = group_by(ssd$actual, .data$team, .data$setter, .data$set_number) %>% dplyr::summarize(time = n()) %>%
                                group_by(.data$team, .data$setter) %>% mutate(max_time = cumsum(.data$time)),
                            aes_string(xintercept = "max_time"), col = "grey") +
            geom_label(data = data_label, aes_string(x = "max_time - time/2", y = "max(maxPts, maxPts_sim)+2", label = "score"), size = font_size/11*9*0.35278)
    }

    if (!is.null(ssd$filtered_simulations)) {
        bbtrajqi_f <- mutate(group_by(ssd$filtered_simulations, .data$sim_num, .data$team, .data$setter),
                             traj = cumsum(.data$reward))
        bbtrajqi_f <- ungroup(dplyr::summarize(group_by(bbtrajqi_f, .data$time, .data$team, .data$setter),
                                               trajqi05 = quantile(.data$traj, 0.05), trajqi95 = quantile(.data$traj, 0.95),
                                               trajqim = mean(.data$traj)))
        g <- g + geom_line(data = bbtrajqi_f, aes_string(x = "time", y = "trajqim", group = "setter"), col = "red") +
            geom_ribbon(data = bbtrajqi_f, aes_string(x = "time", ymin = "trajqi05", ymax = "trajqi95", group = "setter"), col = "white", fill = "red", alpha = 0.25)
    }
    g + theme_bw(base_size = font_size)
}


ssd_set_setter_labels <- function(ssd, label_setters_by = "id") {
    assert_that(is.string(label_setters_by))
    label_setters_by <- tolower(label_setters_by)
    label_setters_by <- match.arg(label_setters_by, c("id", "name"))
    if (label_setters_by == "name") {
        ## figure the player_id -> player_name mapping
        pnid <- na.omit(dplyr::select(ssd$raw_data$plays, .data$team, .data$player_id, .data$player_name))
        pnid <- distinct(dplyr::filter(pnid, .data$player_id %in% c(unique(ssd$simulations$setter), unique(ssd$actual$setter))))
        if (nrow(pnid) != nrow(distinct(dplyr::select(pnid, -"team")))) {
            warning("cannot find unique player name for each player id, using ids as labels")
        } else {
            try(ssd$simulations <- dplyr::rename(dplyr::select(left_join(ssd$simulations, pnid, by = c("team", setter = "player_id")), -"setter"), setter = "player_name"))
            try(ssd$actual <- dplyr::rename(dplyr::select(left_join(ssd$actual, pnid, by = c("team", setter = "player_id")), -"setter"), setter = "player_name"))
            try(ssd$conditional_simulations <- dplyr::rename(dplyr::select(left_join(ssd$conditional_simulations, pnid, by = c("team", setter = "player_id")), -"setter"), setter = "player_name"))
        }
    }
    ssd
}

#' Court plot of a real and simulated setter distribution
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param label_setters_by string: either "id" or "name"
#' @param font_size numeric: font size
#' @param title_wrap numeric: if non-`NA`, use [strwrap()] to break the title into lines of this width
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception", "Transition"),
#'                                           n_sim = 100, attack_by = "code")
#' ov_plot_distribution(setter)
#' @export
ov_plot_distribution <- function(ssd, label_setters_by = "id", font_size = 11, title_wrap = NA) {
    ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
    twrapf <- if (is.na(title_wrap)) function(z) z else function(z) paste0(strwrap(z, title_wrap), collapse = "\n")
    attack_by_var <- ssd$attack_by_var
    attack_zones_sim <- dplyr::summarize(group_by(ssd$simulations, .data$team, .data$setter, .data$setter_position, .data$attack_choice), n_attacks = n())
    attack_zones_sim <- ungroup(mutate(attack_zones_sim, rate = .data$n_attacks / sum(.data$n_attacks)))
    if (packageVersion("dplyr") >= "1.0.0") {
        attack_zones_actual <- dplyr::summarize(group_by(ssd$actual, .data$team, .data$setter, .data$setter_position, dplyr::across({{ attack_by_var }})), n_attacks = n())
    } else {
        attack_zones_actual <- dplyr::summarize(dplyr::group_by_at(ssd$actual, c("team", "setter", "setter_position", attack_by_var)), n_attacks = n())
    }
    attack_zones_actual <- ungroup(mutate(attack_zones_actual, rate = .data$n_attacks / sum(.data$n_attacks)))
    setter_team <- distinct(dplyr::select(attack_zones_actual, "team", "setter"))
    if (attack_by_var == "attack_code") {
        ## hmm, the meta$attacks data.frame can be a bit messy ... the attack location is either "X8" or "V8"
        atk_loc_var <- if ("X8" %in% names(ssd$raw_data$meta$attacks)) "X8" else "V8"
        if (!atk_loc_var %in% names(ssd$raw_data$meta$attacks)) stop("could not plot attack distribution, missing attack start location in meta$attacks table")
        attack_zones_actual <- left_join(attack_zones_actual, dplyr::select(ssd$raw_data$meta$attacks, "code", {{ atk_loc_var }}, "type"), by = c("attack_code" = "code"))
        attack_zones_actual <- mutate(cbind(attack_zones_actual, dv_index2xy(attack_zones_actual[[atk_loc_var]])),
                                      rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), c("4","3","2","5","6","1")))

        attack_zones_sim <- left_join(attack_zones_sim, dplyr::select(ssd$raw_data$meta$attacks, "code", {{ atk_loc_var }}, "type"), by = c("attack_choice" = "code"))
        attack_zones_sim <- mutate(cbind(attack_zones_sim, dv_index2xy(attack_zones_sim[[atk_loc_var]])),
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), c("4","3","2","5","6","1")))

        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy), aes_string(x = "x", y = "y - 0.5")) +
                geom_segment(aes_string(xend = "x", yend = "y - 0.1", size = "rate", col = "type"), arrow = arrow(length = unit(0.03, "npc"))) +
                geom_text(aes_string(y = "y - 0.6", label = "attack_code", col = "type"), show.legend = FALSE) +
                ggcourt(court = "lower", labels = "") +
                facet_wrap(~rotation) +
                scale_size(guide = "none") +
                ggtitle(twrapf(paste0("Actual distribution - ", yy, " (", xx, ")")))
        })

        gSim <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(dplyr::filter(attack_zones_sim, .data$team == xx & .data$setter == yy), aes_string(x = "x", y = "y - 0.5")) +
                geom_segment(aes_string(xend = "x", yend = "y - 0.1",  size = "rate", col = "type"), arrow = arrow(length = unit(0.03, "npc"))) +
                geom_text(aes_string(y = "y - 0.6", label = "attack_choice", col = "type"), show.legend = FALSE) +
                ggcourt(court = "lower", labels = "") + scale_size(guide = 'none') +
                scale_fill_gradient2(name = "Attack rate") + facet_wrap(~rotation) +
                ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
        })
    } else {
        attack_zones_sim <- mutate(cbind(attack_zones_sim, dv_xy(as.numeric(attack_zones_sim$attack_choice), end = "lower")),
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), c("4","3","2","5","6","1")))
        attack_zones_actual <- mutate(cbind(attack_zones_actual, dv_xy(attack_zones_actual$start_zone, end = "lower")),
                                      rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), c("4","3","2","5","6","1")))
        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")),
                   aes_string(x = "x", y = "y", fill = "rate")) + geom_tile() + ggcourt(court = "lower", labels = "", base_size = font_size) +
                scale_fill_gradient2(name = "Attack rate") + facet_wrap(~rotation) + theme(legend.position = "none") +
                geom_text(aes_string(x = "x", y ="y", label = "lab"), size = font_size/11*9.5*0.35278) + ggtitle(twrapf(paste0("Actual distribution - ", yy, " (", xx, ")")))
        })

        gSim <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_sim, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")),
                   aes_string(x = "x", y = "y", fill = "rate")) + geom_tile() + ggcourt(court = "lower", labels = "", base_size = font_size) +
                scale_fill_gradient2(name = "Attack rate") + facet_wrap(~rotation) +
                geom_text(aes_string(x = "x", y = "y", label = "lab"), size = font_size/11*9.5*0.35278) +
                theme(legend.position = "none") + ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
        })
    }
    wrap_plots(c(gActual, gSim), nrow = 2) + plot_layout(guides = "collect")
}


#' Plot a simulated setter distribution sequence
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param label_setters_by string: either "id" or "name"
#' @param font_size numeric: font size
#' @param title_wrap numeric: if non-`NA`, use [strwrap()] to break the title into lines of this width
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception", "Transition"),
#'                                        n_sim = 100, attack_by = "code")
#' ov_plot_sequence_distribution(ssd)
#'
#' @export
ov_plot_sequence_distribution <- function(ssd, label_setters_by = "id", font_size = 11, title_wrap = NA) {
    attack_by_var <- ssd$attack_by_var
    ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
    twrapf <- if (is.na(title_wrap)) function(z) z else function(z) paste0(strwrap(z, title_wrap), collapse = "\n")
    ## conditional distributions
    cd_bandit <- dplyr::select(ssd$conditional_simulations, dplyr::contains("Bandit choice"), "team", "setter", "set_number", "score", "point_id", "setter_position", "ts_pass_evaluation_code", {{ attack_by_var }})
    cd_bandit <- ungroup(mutate(group_by(cd_bandit, .data$team, .data$setter), time = row_number()))
    cd_bandit <- pivot_longer(cd_bandit, cols = dplyr::contains("Bandit choice"), names_to = "attack_choice_b", values_to = "Probability")
    cd_bandit <- mutate(cd_bandit, attack_choice_b = stringr::str_remove(.data$attack_choice_b, "Bandit choice "))
    cd_bandit$attack_choice <- as.factor(cd_bandit[[attack_by_var]])
    cd_setter <- ssd$conditional_simulations
    cd_setter$attack_choice <- as.factor(cd_setter[[attack_by_var]])
    cd_setter <- mutate(group_by(cd_setter, .data$team, .data$setter), time = row_number())
    cd_bandit <- ungroup(mutate(group_by(cd_bandit, .data$team, .data$setter, .data$time),
                                choice_bandit = case_when(.data$Probability == max(.data$Probability, na.rm=TRUE) ~ .data$attack_choice_b),
                                least_likely_choice = case_when(.data$Probability == min(.data$Probability, na.rm=TRUE) & .data$Probability < 1 ~ attack_choice_b)))
    setter_team <- distinct(dplyr::select(cd_setter, "team", "setter"))
    gCondDist <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
        ggplot(data = dplyr::filter(cd_setter, .data$team == xx, .data$setter == yy), aes_string(x = "time", y = "attack_choice")) +
            geom_tile(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy), aes_string(x = "time + 0.5", y = "attack_choice_b", fill = "Probability"), alpha = 0.75) +
            geom_step(group = 1) +
            geom_point(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy), aes_string(x = "time + 0.5", y = "choice_bandit"), col = "white", size = 1) +
            geom_point(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy & as.character(.data$attack_choice) == .data$least_likely_choice),
                       aes_string(x = "time + 0.5", y = "least_likely_choice"), col = "red", size = 1) +
            theme_bw(base_size = font_size) + scale_fill_continuous(na.value = NA, limits = c(0, 1)) +
            theme(legend.position = "none") + labs(x = "Game history", y = "Attack choice") +
            ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
    })
    wrap_plots(c(gCondDist)) + plot_layout(guides = "collect")
}


#' Create a prior table from a dvw or a directory of dvw files
#'
#' @param dvw string: path to one or more datavolley files, a list of one or more datavolley objects, or a directory containing datavolley files
#' @param play_phase character: one or both of "Reception", "Transition"
#' @param attack_by string: either "code" or "zone"
#' @param exclude_attacks character: vector of attack codes to exclude
#'
#' @return A list, currently with one component named "prior_table"
#'
#' @examples
#' ## use this file to create the priors
#' hist_dvw <- ovdata_example("PM06")
#' history_t <- ov_create_history_table(dvw = hist_dvw, attack_by = "zone")
#'
#' ## use it on this file
#' dvw <- ovdata_example("190301_kats_beds")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception", n_sim = 500,
#'                                   attack_by = "zone", attack_options = "use_history",
#'                                   history_table = history_t$prior_table, filter_sim = TRUE)
#'
#' ## plot the results
#' ov_plot_ssd(setter, overlay_set_number = TRUE)
#' ov_plot_distribution(setter)
#'
#' @export
ov_create_history_table <- function(dvw, play_phase = c("Reception", "Transition"), attack_by = "code", exclude_attacks = c("PR")) {
    attack_by <- match.arg(attack_by, c("code", "zone"))
    attack_by_var <- switch(attack_by,
                           "code" = "attack_code",
                           "zone" = "start_zone")

    if (is.character(dvw)) {
        if (is.string(dvw) && dir.exists(dvw)) {
            dvw <- dir(dvw, pattern = "\\.dvw$", ignore.case = TRUE, full.names = TRUE)
        } else if (is.character(dvw) && all(file.exists(dvw))) {
            ## one or more files, ok
        } else {
            stop("dvw is a character but does not appear to be a directory nor a vector of file names")
        }
        raw_data <- bind_rows(lapply(dvw, function(z) plays(dv_read(z))))
    } else if (is.data.frame(dvw) && (inherits(dvw, "datavolleyplays") || all(c("home_team", "visiting_team", "skill", "evaluation_code", "home_setter_position", "visiting_setter_position") %in% names(dvw)))) {
        ## use as-is
        raw_data <- dvw
    } else if (is.list(dvw)) {
        if (inherits(dvw, "datavolley")) {
            ## single datavolley
            dvw <- list(dvw)
        } else if (all(vapply(dvw, inherits, "datavolley", FUN.VALUE = TRUE))) {
            ## list of datavolley objects, ok
        } else {
            stop("dvw is a list but does not appear to be a datavolley object nor a list of such")
        }
        raw_data <- bind_rows(lapply(dvw, plays))
    } else {
        stop("unrecognized format of input parameter dvw")
    }

    data_game <- ov_augment_plays(raw_data, to_add = c("touch_summaries", "setters"))
    data_game <- mutate(data_game, setter_position = case_when(.data$team == .data$home_team ~ .data$home_setter_position,
                                                               .data$team == .data$visiting_team ~ .data$visiting_setter_position))
    data_game <- dplyr::filter(data_game, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase))

    prior_table <- if (packageVersion("dplyr") >= "1.0.0") {
                       group_by(data_game, .data$team, .data$setter_id, .data$setter_position, .data$ts_pass_evaluation_code, dplyr::across({{ attack_by_var }}))
                   } else {
                       dplyr::group_by_at(data_game, c("team", "setter_id", "setter_position", "ts_pass_evaluation_code", attack_by_var))
                   }
    prior_table <- ungroup(dplyr::summarize(prior_table, KR = mean(.data$evaluation_code == "#"), n = n()))
    list(prior_table = prior_table)
}
