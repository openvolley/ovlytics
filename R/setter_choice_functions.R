expand_rle <- function(lens) unlist(lapply(seq_along(lens), function(z) rep(z, lens[z])))

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
#' @param setter_position_by string: either "rotation" or "front_back"
#' @param history_table list: (only if `attack_options` is "use_history") the object returned by [ov_create_history_table()]
#' @param attack_by string: either "code", "zone", "tempo", "setter call"
#' @param exclude_attacks character: vector of attack codes to exclude
#' @param shiny_progress numeric: an optional two-element vector. If not `NULL` or `NA`, [shiny::setProgress()] calls will be made during simulation with `value`s in this range
#'
#' @seealso [ov_create_history_table()]
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' system.time({
#'   ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception",
#'                                          n_sim = 100, attack_by = "setter call", 
#'                                          setter_position_by = "front_back")
#' })
#' @export
ov_simulate_setter_distribution <- function(dvw, play_phase = c("Reception", "Transition"), n_sim = 500, priors = list(name = "beta", par1 = 1, par2 = 1),
                                            epsilon = 1, filter_sim = FALSE, attack_options = "use_data", setter_position_by = "rotation", history_table = NULL,
                                            attack_by = "code", exclude_attacks = c("PR"), shiny_progress = NULL) {
    ## TODO check input parms
    attack_by <- match.arg(attack_by, c("zone", "code", "tempo", "setter call"))
    attack_by_var <- switch(attack_by,
                            "code" = "attack_code",
                            "zone" = "start_zone", 
                            "tempo" = "skill_type", 
                            "setter call" = "set_code")
    setter_position_by <- match.arg(setter_position_by, c("rotation", "front_back"))
    setter_position_by_var <- switch(setter_position_by,
                            "rotation" = "setter_position",
                            "front_back" = "setter_front_back")
    
    
    # Check that ards matches with history as well!
    
    attack_options <- match.arg(attack_options, c("use_data", "use_history"))

    raw_data <- if (is.character(dvw)) dv_read(dvw) else dvw

    data <- ov_augment_plays(plays(raw_data), to_add = c("touch_summaries", "setters"))
    team_setter <- tidyr::drop_na(dplyr::distinct(dplyr::select(dplyr::filter(data, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), "team", "setter_id")))
    
    # If setter call we need to propagate the setter call to the subsequent attack
    if(attack_by == "setter call"){
        data <- dplyr::mutate(data, 
                                set_code = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ lag(.data$set_code), 
                                                         TRUE ~ .data$set_code), 
                                set_description = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ lag(.data$set_description), 
                                                            TRUE ~ .data$set_description)
                              )
    }
    
    
    data <- dplyr::filter(dplyr::filter(data, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), !is.na(.data[[attack_by_var]]))

    sim <- actual <- rates <- NULL

    if ((is.null(history_table$prior_table) || !is.data.frame(history_table$prior_table) || nrow(history_table$prior_table) < 1) && attack_options %eq% "use_history") stop("History table is missing or empty")

    if((!(setter_position_by_var %eq% history_table$setter_position_by_var) || !(attack_by_var %eq% history_table$attack_by_var)) && attack_options %eq% "use_history") stop("History and bandit option need to match")
        
    do_shiny_progress <- tryCatch(!is.null(shiny_progress) && length(shiny_progress) == 2 && !any(is.na(shiny_progress)) && requireNamespace("shiny", quietly = TRUE), error = function(e) FALSE)
    n_outer <- nrow(dplyr::distinct(team_setter[, c("team", "setter_id")]))
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
                              dplyr::group_by(data_game, dplyr::across({{ setter_position_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))
                          } else {
                              dplyr::group_by_at(data_game, c(setter_position_by_var, "ts_pass_quality", attack_by_var))
                          }
            tbleChoice <- dplyr::ungroup(dplyr::summarize(tbleChoice, alpha = sum(.data$evaluation %eq% "Winning attack"), beta = sum(!(.data$evaluation %eq% "Winning attack")), n = dplyr::n()))

            if (attack_options == "use_data") {
                this <- tbleChoice
            } else if (attack_options == "use_history") {
                this <- dplyr::bind_rows(
                    #dplyr::select(mutate(dplyr::filter(history_table, .data$team == iTeam & .data$setter_id == iSetter), n_s = .data$KR * .data$n), -"KR", -"team", -"setter_id"),
                    dplyr::select(dplyr::filter(history_table$prior_table, .data$team == iTeam & .data$setter_id == iSetter), -"team", -"setter_id"),
                    #dplyr::select(mutate(tbleChoice, n_s = .data$KR * .data$n), -"KR"))
                    tbleChoice)
                if (packageVersion("dplyr") >= "1.0.0") {
                    this <- ungroup(dplyr::summarize(group_by(this, dplyr::across({{ setter_position_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }})), alpha = sum(.data$alpha), beta = sum(.data$beta), n = sum(.data$n)))
                } else {
                    this <- ungroup(dplyr::summarize(dplyr::group_by_at(this, c(setter_position_by_var, "ts_pass_quality", attack_by_var)), alpha = sum(.data$alpha), beta = sum(.data$beta), n = sum(.data$n)))
                }
            }
            # Define the kill rate
            this <- dplyr::select(dplyr::mutate(this, KR = .data$alpha / (.data$alpha + .data$beta)), -"alpha", -"beta")
            
            probTable <- tidyr::pivot_wider(dplyr::select(this, -"n"), names_from = {{ attack_by_var }}, values_from = .data$KR)
            if (FALSE) {
                ## use team_id in plot
                tableBB <- dplyr::mutate(data_game, score = paste(.data$home_team_id, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team_id))
            } else {
                ## create a short team name automatically
                tableBB <- dplyr::mutate(data_game, score = paste(team_name_to_abbrev(.data$home_team), .data$home_team_score,
                                                           "-",
                                                           .data$visiting_team_score, team_name_to_abbrev(.data$visiting_team)))
            }
            tableBB <- dplyr::left_join(dplyr::select(tableBB, "set_number", "point_id", "score",{{ setter_position_by_var }}, "ts_pass_quality", {{ attack_by_var }}, "evaluation"), probTable, by = c({{ setter_position_by_var }}, "ts_pass_quality"))

            choice <- matrix(seq_len(ncol(tableBB) - 7), ncol = (ncol(tableBB) - 7), nrow = nrow(tableBB), byrow = TRUE)
            choice[which(is.na(as.matrix(tableBB[, seq(8, ncol(tableBB), by = 1)])))] <- NA

            probs <- as.matrix(tableBB[, seq(8, ncol(tableBB), by = 1)])
            probs[is.na(probs)] <- 0

            duration <- nrow(tableBB)

            ## preallocate vectors and assemble data frame outside of loop
            res_sim_num <- rep(seq_len(n_sim), each = duration)
            resn <- length(res_sim_num)
            res_ts_pass_quality <- rep(NA_character_, resn)
            res_setter_position <- rep(NA_integer_, resn)
            res_point_id <- rep(NA_integer_, resn)

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

                res_setter_position[res_sim_num == nS ] <- tableBB[,setter_position_by_var]
                res_ts_pass_quality[res_sim_num == nS] <- tableBB$ts_pass_quality
                res_point_id[res_sim_num == nS] <- tableBB$point_id

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
            ObservedSuccess <- as.numeric(tableBB$evaluation == "Winning attack")
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
                              chosen_arm = arm_seq, reward = reward_seq, ts_pass_quality = NA, setter_position = res_setter_position, 
                              point_id = res_point_id,
                              attack_choice = res_attack_choice,
                              team = iTeam, setter = iSetter, stringsAsFactors = FALSE)

            tableBB$team <- iTeam
            tableBB$setter <- iSetter
            tbleChoice$team <- iTeam
            tbleChoice$setter <- iSetter

            sim <- bind_rows(sim, res)
            actual <- bind_rows(actual, tableBB)
            tbleChoice <- dplyr::select(mutate(tbleChoice, KR = .data$alpha / (.data$alpha + .data$beta)), -"alpha", -"beta")
            rates <- bind_rows(rates, tbleChoice)
        }
    }

    sim_filter <- NULL

    if (isTRUE(filter_sim)) {
        tbleC <- ungroup(dplyr::summarize(group_by(sim, .data$attack_choice, .data$setter_position, .data$ts_pass_quality, .data$sim_num, .data$team, .data$setter),
                                          KR = mean(.data$reward), n = n()))
        tbleC <- left_join(tbleC, dplyr::rename(rates, true_KR = "KR", true_n = "n"), by = c("attack_choice" = {{ attack_by_var }}, 
                                                                                             "setter_position" = {{ setter_position_by_var }},
                                                                                             "ts_pass_quality", "team", "setter"))
        summaryTC <- mutate(ungroup(tbleC), trueKR_up = .data$true_KR + 1.64*sqrt(.data$true_KR * (1 - .data$true_KR) / .data$true_n),
                            trueKR_down = .data$true_KR - 1.64*sqrt(.data$true_KR * (1 - .data$true_KR) / .data$true_n),
                            keepSim = case_when(.data$KR <= .data$trueKR_up & .data$KR >= .data$trueKR_down ~ TRUE,
                                                TRUE ~ FALSE))
        summaryTC <- mutate(unique(dplyr::select(dplyr::filter(summaryTC, !.data$keepSim), "sim_num", "team", "setter")), keepSim = FALSE)

        sim_filter <- dplyr::filter(left_join(sim, summaryTC, by = c("setter", "team", "sim_num")), is.na(.data$keepSim))
    }
    list(simulations = sim, actual = actual, rates = rates, filtered_simulations = sim_filter, attack_by_var = attack_by_var, setter_position_by_var = setter_position_by_var, raw_data = raw_data, conditional_simulations = cond)
}

#' Plot a simulated setter distribution
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param overlay_set_number boolean: if `TRUE`, overlay set number and score in the plot
#' @param label_setters_by string: either "id" or "name"
#' @param font_size numeric: font size
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, 
#'                                           n_sim = 150, attack_by = "zone")
#' ov_plot_ssd(setter, overlay_set_number = TRUE)
#' @export
ov_plot_ssd <- function(ssd, overlay_set_number = FALSE, label_setters_by = "name", font_size = 11) {
    assert_that(is.flag(overlay_set_number), !is.na(overlay_set_number))
    ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
    bbtrajqi <- mutate(group_by(ssd$simulations, .data$sim_num, .data$team, .data$setter),
                       traj = cumsum(.data$reward))
    bbtrajqi <- ungroup(dplyr::summarize(group_by(bbtrajqi, .data$time,.data$point_id,  .data$team, .data$setter),
                                         trajqi05 = quantile(.data$traj, 0.05), trajqi95 = quantile(.data$traj, 0.95),
                                         trajqim = mean(.data$traj)))

    # Now, in case a setter comes in and out, we need to clean up the plot a bit. Let's define 
    # 'period_on_court' as a period of successive points played
    
    dataplays <- ov_augment_plays(plays(ssd$raw_data), to_add = c("touch_summaries", "setters"))
    temp <-  dplyr::select(dplyr::slice(dplyr::group_by(dplyr::filter(dataplays,!is.na(.data$skill) & !is.na(.data$home_setter_id) & !is.na(.data$visiting_setter_id)) ,.data$match_id, .data$point_id), 1L), "match_id", "point_id", "home_setter_id", "visiting_setter_id")
    temp$home_setter_period_on_court <- paste0(temp$home_setter_id, expand_rle(rle(temp$home_setter_id)$lengths))
    temp$visiting_setter_period_on_court <- paste0(temp$visiting_setter_id, expand_rle(rle(temp$visiting_setter_id)$lengths))
    
    bbtrajqi <- mutate(left_join(bbtrajqi,
        select(ungroup(temp), .data$point_id, .data$home_setter_period_on_court, .data$visiting_setter_period_on_court), by = "point_id"),
        period_on_court = case_when(.data$team %eq% datavolley::home_team(ssd$raw_data) ~ .data$home_setter_period_on_court, 
                                           .data$team %eq% datavolley::visiting_team(ssd$raw_data) ~ .data$visiting_setter_period_on_court))
    
    tbC <- mutate(group_by(ssd$actual, .data$team, .data$setter),
                  pts = cumsum(.data$evaluation == "Winning attack"), time = row_number())
    
    tbC <- mutate(left_join(tbC,
                            select(ungroup(temp), .data$point_id, .data$home_setter_period_on_court, .data$visiting_setter_period_on_court), by = "point_id"),
                  period_on_court = case_when(.data$team %eq% datavolley::home_team(ssd$raw_data) ~ .data$home_setter_period_on_court, 
                                              .data$team %eq% datavolley::visiting_team(ssd$raw_data) ~ .data$visiting_setter_period_on_court))
    g <- ggplot(tbC) +
        geom_line(data = bbtrajqi, aes_string(x = "point_id", y = "trajqim", group = "period_on_court"), col = "orange") +
        geom_ribbon(data = bbtrajqi, aes_string(x = "point_id", ymin = "trajqi05", ymax = "trajqi95", group = "period_on_court"), col = "white", fill = "orange", alpha = 0.25) +
        geom_line(aes_string(x = "point_id", y = "pts", group = "period_on_court", linetype = "setter")) +
        labs(x = "Point ID", y = "Cumulative points scored") + facet_wrap("team", scales = "free")

    if (overlay_set_number) {
        data_label <- dplyr::select(mutate(ssd$raw_data$meta$result, set_number = row_number(),
                                           home_team_abbrv = team_name_to_abbrev(datavolley::home_team(ssd$raw_data)),
                                           visiting_team_abbrv = team_name_to_abbrev(datavolley::visiting_team(ssd$raw_data)),
                                           score = paste(.data$home_team_abbrv, .data$score, .data$visiting_team_abbrv)), "set_number", "score")

        data_label <- left_join(data_label, group_by(ssd$actual, .data$team, .data$set_number) %>%
                                    dplyr::summarize(x_label = max(.data$point_id)/2 + min(.data$point_id)/2) %>% ungroup(),
                                by = "set_number") %>%
            left_join(ungroup(group_by(tbC, .data$team) %>% dplyr::summarize(maxPts = max(.data$pts))), by = c("team")) %>%
            left_join(ungroup(group_by(bbtrajqi, .data$team) %>% dplyr::summarize(maxPts_sim = max(.data$trajqi95))), by = c("team"))

        data_sets <- group_by(ssd$actual, .data$team, .data$set_number) %>% dplyr::summarize(x_line = max(.data$point_id))

        g <- g + geom_vline(data = data_sets, aes_string(xintercept = "x_line"), col = "grey") +
            geom_label(data = data_label, aes_string(x = "x_label", y = "max(maxPts, maxPts_sim)+2", label = "score"), size = font_size/11*9*0.35278)
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
    g + theme_bw(base_size = font_size) + theme(legend.position = 'bottom')
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
    setter_position_by_var <- ssd$setter_position_by_var
    
    attack_zones_sim <- dplyr::summarize(group_by(ssd$simulations, .data$team, .data$setter, .data$setter_position, .data$attack_choice), n_attacks = n())
    attack_zones_sim <- ungroup(mutate(attack_zones_sim, rate = .data$n_attacks / sum(.data$n_attacks)))
    if (packageVersion("dplyr") >= "1.0.0") {
        attack_zones_actual <- dplyr::summarize(group_by(ssd$actual, .data$team, .data$setter, dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }})), n_attacks = n())
    } else {
        attack_zones_actual <- dplyr::summarize(dplyr::group_by_at(ssd$actual, c("team", "setter", setter_position_by_var, attack_by_var)), n_attacks = n())
    }
    attack_zones_actual <- ungroup(mutate(attack_zones_actual, rate = .data$n_attacks / sum(.data$n_attacks)))
    setter_team <- distinct(dplyr::select(attack_zones_actual, "team", "setter"))
    
    setter_rotation_levels = switch(setter_position_by_var,
                                    "setter_position" = c("4","3","2","5","6","1"),
                                    "setter_front_back" = c("front", "back"))
    
    if (attack_by_var == "attack_code") {
       
        ## hmm, the meta$attacks data.frame can be a bit messy ... the attack location is either "X8" or "V8"
        atk_loc_var <- if ("X8" %in% names(ssd$raw_data$meta$attacks)) "X8" else "V8"
        if (!atk_loc_var %in% names(ssd$raw_data$meta$attacks)) stop("could not plot attack distribution, missing attack start location in meta$attacks table")
        attack_zones_actual <- left_join(attack_zones_actual, dplyr::select(ssd$raw_data$meta$attacks, "code", {{ atk_loc_var }}, "type"), by = c("attack_code" = "code"))
        attack_zones_actual <-cbind(attack_zones_actual, dv_index2xy(attack_zones_actual[[atk_loc_var]]))

        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)
        
        attack_zones_sim <- left_join(attack_zones_sim, dplyr::select(ssd$raw_data$meta$attacks, "code", {{ atk_loc_var }}, "type"), by = c("attack_choice" = "code"))
        attack_zones_sim <- mutate(cbind(attack_zones_sim, dv_index2xy(attack_zones_sim[[atk_loc_var]])),
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), setter_rotation_levels))

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
    } 
    if (attack_by_var == "start_zone") {
        attack_zones_sim <- mutate(cbind(attack_zones_sim, dv_xy(as.numeric(attack_zones_sim$attack_choice), end = "lower")),
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), setter_rotation_levels))
        attack_zones_actual <- cbind(attack_zones_actual, dv_xy(attack_zones_actual$start_zone, end = "lower"))
        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)
        
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
    if (attack_by_var == "skill_type") {
        
        attack_zones_sim <- mutate(attack_zones_sim, 
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), setter_rotation_levels),
                                   attack_choice = forcats::fct_relevel(as.factor(as.character(.data$attack_choice)), c("Quick ball attack","Half ball attack","Head ball attack","High ball attack")))
        attack_zones_actual <- mutate(attack_zones_actual,
                                      skill_type = forcats::fct_relevel(as.factor(as.character(.data$skill_type)), c("Quick ball attack","Half ball attack","Head ball attack","High ball attack")))
        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)
        
        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")), aes_string(x = "skill_type"))  + 
                geom_col(aes_string(y= "rate")) + facet_wrap(~rotation) +
                scale_y_continuous(labels=scales::percent) + theme_bw()+
                theme(axis.text.x = element_text(size = 9.5*0.75278, angle = 30, hjust = 1)) + 
                ylab("Percent")+ xlab("")+
                ggtitle(twrapf(paste0("Actual distribution - ", yy, " (", xx, ")")))
        })
        gSim <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_sim, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")), aes_string(x = "attack_choice"))  + 
                geom_col(aes_string(y= "rate")) + facet_wrap(~rotation) +
                scale_y_continuous(labels=scales::percent) + theme_bw()+
                theme(axis.text.x = element_text(size = 9.5*0.75278, angle = 30, hjust = 1)) + 
                ylab("Percent")+ xlab("")+
                ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
        })
    }
    if (attack_by_var == "set_code") {
        
        attack_zones_sim <- mutate(attack_zones_sim, 
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), setter_rotation_levels),
                                   attack_choice = forcats::fct_relevel(as.factor(as.character(.data$attack_choice)), levels(.data$attack_choice)))
        attack_zones_actual <- mutate(attack_zones_actual,
                                      skill_type = forcats::fct_relevel(as.factor(as.character(.data$set_code)), levels(.data$set_code)))
        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)
        
        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")), aes_string(x = "set_code"))  + 
                geom_col(aes_string(y= "rate")) + facet_wrap(~rotation) +
                scale_y_continuous(labels=scales::percent) + theme_bw()+
                theme(axis.text.x = element_text(size = 9.5*0.75278, angle = 30, hjust = 1)) + 
                ylab("Percent")+ xlab("")+
                ggtitle(twrapf(paste0("Actual distribution - ", yy, " (", xx, ")")))
        })
        gSim <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_sim, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")), aes_string(x = "attack_choice"))  + 
                geom_col(aes_string(y= "rate")) + facet_wrap(~rotation) +
                scale_y_continuous(labels=scales::percent) + theme_bw()+
                theme(axis.text.x = element_text(size = 9.5*0.75278, angle = 30, hjust = 1)) + 
                ylab("Percent")+ xlab("")+
                ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
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
#' @param split_set boolean: if `TRUE`, separate the distribution sequence by set
#'
#' @examples
#' dvw <- ovdata_example("190301_kats_beds")
#' ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception"),
#'                                        n_sim = 100, attack_by = "zone")
#' ov_plot_sequence_distribution(ssd)
#'
#' @export
ov_plot_sequence_distribution <- function(ssd, label_setters_by = "id", font_size = 11, title_wrap = NA, split_set = FALSE) {
    attack_by_var <- ssd$attack_by_var
    setter_position_by_var <- ssd$setter_position_by_var
    
    ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
    twrapf <- if (is.na(title_wrap)) function(z) z else function(z) paste0(strwrap(z, title_wrap), collapse = "\n")
    ## conditional distributions
    cd_bandit <- dplyr::select(ssd$conditional_simulations, dplyr::contains("Bandit choice"), "team", "setter", "set_number", "score", "point_id", {{ setter_position_by_var }}, "ts_pass_quality", {{ attack_by_var }})
    cd_bandit <- ungroup(mutate(group_by(cd_bandit, .data$team, .data$setter), time = row_number()))
    cd_bandit <- pivot_longer(cd_bandit, cols = dplyr::contains("Bandit choice"), names_to = "attack_choice_b", values_to = "Probability")
    cd_bandit <- mutate(cd_bandit, attack_choice_b = stringr::str_remove(.data$attack_choice_b, "Bandit choice "))
    cd_bandit$attack_choice <- as.factor(cd_bandit[[attack_by_var]])
    
    cd_setter <- ssd$conditional_simulations
    cd_setter$attack_choice <- as.factor(cd_setter[[attack_by_var]])
    cd_setter <- mutate(group_by(cd_setter, .data$team, .data$setter), time = row_number())
    
    cd_setter$ts_pass_quality <- forcats::fct_relevel(as.factor(as.character(unlist(cd_setter$ts_pass_quality))), c("Poor", "OK", "Good", "Perfect"))
    
    cd_bandit <- ungroup(mutate(group_by(cd_bandit, .data$team, .data$setter, .data$time, .data$point_id),
                                choice_bandit = case_when(.data$Probability == max(.data$Probability, na.rm=TRUE) ~ .data$attack_choice_b),
                                least_likely_choice = case_when(.data$Probability == min(.data$Probability, na.rm=TRUE) & .data$Probability < 1 ~ attack_choice_b)))
    
    cd_bandit$ts_pass_quality <- forcats::fct_relevel(as.factor(as.character(unlist(cd_bandit$ts_pass_quality))), c("Poor", "OK", "Good", "Perfect"))
    
    setter_team <- distinct(dplyr::select(cd_setter, "team", "setter"))
    gCondDist <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
        g1 <- ggplot(data = dplyr::filter(cd_setter, .data$team == xx, .data$setter == yy), aes_string(x = "time", y = "attack_choice")) +
            geom_tile(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy), aes_string(x = "time + 0.5", y = "attack_choice_b", fill = "Probability"), alpha = 0.75) +
            geom_step(group = 1) +
            geom_point(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy), aes_string(x = "time + 0.5", y = "choice_bandit"), col = "white", size = 1) +
            geom_point(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy & as.character(.data$attack_choice) == .data$least_likely_choice),
                       aes_string(x = "time + 0.5", y = "least_likely_choice"), col = "red", size = 1) +
            theme_bw(base_size = font_size) + scale_fill_continuous(na.value = NA, limits = c(0, 1)) +
            theme(legend.position = "none") + labs(x = "Game history", y = "Attack choice") +
            ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")"))) 

        g2 <- ggplot(data = dplyr::filter(cd_setter, .data$team == xx, .data$setter == yy), aes_string(x = "time")) +
                geom_tile(aes_string(fill = "ts_pass_quality", y ="1")) + 
            geom_text(aes_string(y="1", label = "setter_position"), size = 2)+
            scale_fill_brewer()+
            theme_void() + theme(legend.position = 'none')

            if(split_set) g <- (g1  + facet_wrap(~set_number, scales = "free_x")) / (g2 + facet_wrap(~set_number, scales = "free_x")) else g <- g1 / g2 + patchwork::plot_layout(heights = c(8, 1))
            g
    })
    wrap_plots(c(gCondDist)) + plot_layout(guides = "collect")
}


#' Create a prior table from a dvw or a directory of dvw files
#'
#' @param dvw string: path to one or more datavolley files, a list of one or more datavolley objects, or a directory containing datavolley files
#' @param play_phase character: one or both of "Reception", "Transition"
#' @param attack_by string: either "code", "zone", "tempo" or "setter call"
#' @param setter_position_by string: either "rotation", or "front_back"
#' @param exclude_attacks character: vector of attack codes to exclude
#' @param normalize_parameters logical: reduce the prior parameter values
#'
#' @return A list, currently with one component named "prior_table"
#'
#' @examples
#' ## use this file to create the priors
#' hist_dvw <- ovdata_example("190301_kats_beds")
#' history_table <- ov_create_history_table(dvw = hist_dvw, attack_by = "setter call", 
#'                                    setter_position_by = "front_back")
#'
#' ## use it on another file (here, the same file for demo purposes)
#' ## usually the history would be from a reference set of previous matches
#'
#' dvw <- ovdata_example("190301_kats_beds")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception", n_sim = 100,
#'                                   attack_by = "setter call", attack_options = "use_history",
#'                                   setter_position_by = "front_back",
#'                                   history_table = history_table, filter_sim = TRUE)
#'
#' ## plot the results
#' ov_plot_ssd(setter, overlay_set_number = TRUE)
#' ov_plot_distribution(setter)
#'
#' @export
ov_create_history_table <- function(dvw, play_phase = c("Reception", "Transition"), attack_by = "code", setter_position_by = "rotation", 
                                    exclude_attacks = c("PR"), normalize_parameters = TRUE) {
    attack_by <- match.arg(attack_by, c("code", "zone", "tempo", "setter call"))
    attack_by_var <- switch(attack_by,
                           "code" = "attack_code",
                           "zone" = "start_zone", 
                           "tempo" = "skill_type", 
                           "setter call" = "set_code")
    setter_position_by <- match.arg(setter_position_by, c("rotation", "front_back"))
    setter_position_by_var <- switch(setter_position_by,
                                     "rotation" = "setter_position",
                                     "front_back" = "setter_front_back")
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
    
    if(attack_by == "setter call"){
        data_game <- dplyr::mutate(data_game, 
                              set_code = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ lag(.data$set_code), 
                                                   TRUE ~ .data$set_code), 
                              set_description = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ lag(.data$set_description), 
                                                          TRUE ~ .data$set_description)
        )
    }
    
    
    data_game <- dplyr::filter(dplyr::filter(data_game, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), !is.na(.data[[attack_by_var]]))

    # prior_table <- if (packageVersion("dplyr") >= "1.0.0") {
    #                    group_by(data_game, .data$team, .data$setter_id, .data$setter_position, .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))
    #                } else {
    #                    dplyr::group_by_at(data_game, c("team", "setter_id", "setter_position", "ts_pass_quality", attack_by_var))
    #                }
    
    prior_table <- if (packageVersion("dplyr") >= "1.0.0") {
        group_by(data_game,.data$match_id, .data$team, .data$setter_id, dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))
    } else {
        dplyr::group_by_at(data_game, c("match_id","team", "setter_id", setter_position_by_var, "ts_pass_quality", attack_by_var))
    }
    
    prior_table <- ungroup(dplyr::summarize(prior_table, alpha = sum(.data$evaluation %eq% "Winning attack"), beta = sum(!(.data$evaluation %eq% "Winning attack")), n = n()))
    
    prior_table <- if (packageVersion("dplyr") >= "1.0.0") {
        group_by(prior_table,.data$team, .data$setter_id, dplyr::across({{ setter_position_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))
    } else {
        dplyr::group_by_at(prior_table, c("team", "setter_id", setter_position_by_var, "ts_pass_quality", attack_by_var))
    }
    
    prior_table <- drop_na(ungroup(dplyr::summarize(prior_table, alpha = sum(.data$alpha), beta = sum(.data$beta), n = sum(n), n_matches = n())))
    
    ## Normalize priors
    if(normalize_parameters){
        prior_table <- mutate(prior_table, alpha = .data$alpha / .data$n * .data$n_matches, beta = .data$beta / .data$n* .data$n_matches)
    }
    
    list(prior_table = prior_table,  attack_by_var = attack_by_var, setter_position_by_var = setter_position_by_var)
}

#' Plot the prior table 
#' 
#' @param history_table data.frame: the `prior_table` component of the object returned by [ov_create_history_table()]
#' @param team string: team name
#' @param setter_id string: setter_id
#' 
#' @examples 
#' hist_dvw <- ovdata_example("190301_kats_beds")
#' history_table <- ov_create_history_table(dvw = hist_dvw, attack_by = "tempo", 
#'                                                     setter_position_by = "front_back", 
#'                                                     normalize_parameters = FALSE)
#' team = unique(history_table$prior_table$team)[1]
#' setter_id = unique(history_table$prior_table$setter_id)[4]
#' ov_plot_history_table(history_table, team, setter_id)
#' @export
ov_plot_history_table <- function(history_table, team, setter_id){
    if ((is.null(history_table$prior_table) || !is.data.frame(history_table$prior_table) || nrow(history_table$prior_table) < 1)) stop("History table is missing or empty")
    
    team_select <- team
    setter_select <- setter_id
    
    attack_by_var <- history_table$attack_by_var
    setter_position_by_var <- history_table$setter_position_by_var
    
    setter_rotation_levels = switch(setter_position_by_var,
                                    "setter_position" = c("4","3","2","5","6","1"),
                                    "setter_front_back" = c("front", "back"))
    
    df <- dplyr::tibble(thetaBounds = c(0,1))
    if (packageVersion("dplyr") >= "1.0.0") {
        ht_tmp <- mutate(history_table$prior_table, 
                         dplyr::across(dplyr::matches("start_zone"), factor), 
                         dplyr::across(dplyr::matches("attack_code"), factor), 
                         dplyr::across(dplyr::matches("set_code"), factor), 
                         dplyr::across(dplyr::matches("skill_type"), factor), 
                         dplyr::across(dplyr::matches("setter_position"), factor, levels = setter_rotation_levels),
                         dplyr::across(dplyr::matches("setter_front_back"), factor, levels = setter_rotation_levels),
                         dplyr::across(dplyr::matches("ts_pass_quality"), factor, levels = c("Perfect", "Good", "OK", "Poor")))
    } else {
        cls <- intersect(names(history_table$prior_table), c("start_zone", "attack_code", "set_code", "skill_type"))
        if (length(cls) > 0) ht_tmp <- dplyr::mutate_at(history_table$prior_table, cls, factor)
        if ("setter_position" %in% names(ht_tmp)) ht_tmp$setter_position <- factor(ht_tmp$setter_position, levels = setter_rotation_levels)
        if ("setter_front_back" %in% names(ht_tmp)) ht_tmp$setter_front_back <- factor(ht_tmp$setter_front_back, levels = setter_rotation_levels)
        if ("ts_pass_quality" %in% names(ht_tmp)) ht_tmp$ts_pass_quality <- factor(ht_tmp$ts_pass_quality, levels = c("Perfect", "Good", "OK", "Poor"))
    }

    ht <-  if (packageVersion("dplyr") >= "1.0.0") {
               mutate(tidyr::nest(dplyr::group_by(dplyr::filter(ht_tmp, .data$team %eq% team_select, .data$setter_id %eq% setter_select),
                                                  dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }}))),
                      plot = purrr::map(.data$data, ~  
                                                        ggplot(df, aes_string(x="thetaBounds")) + xlab("") + ylab("")+ylim(c(0,6)) +
                                                        apply(.x, 1, function(y) geom_area(aes(fill = y['ts_pass_quality'], col = y['ts_pass_quality']), stat = "function", fun = dbeta, alpha = 0.5, 
                                                                                           args = list(shape1 = as.numeric(y['alpha']), shape2 = as.numeric(y['beta'])))) + theme_bw(base_size = 11) + 
                                                        theme(legend.position = 'none',plot.margin = unit(c(1,0,0,0.1), "pt"))))
           } else {
               mutate(tidyr::nest(dplyr::group_by_at(dplyr::filter(ht_tmp, .data$team %eq% team_select, .data$setter_id %eq% setter_select),
                                                  setter_position_by_var, attack_by_var)),
                      plot = purrr::map(.data$data, ~  
                                                        ggplot(df, aes_string(x="thetaBounds")) + xlab("") + ylab("")+ylim(c(0,6)) +
                                                        apply(.x, 1, function(y) geom_area(aes(fill = y['ts_pass_quality'], col = y['ts_pass_quality']), stat = "function", fun = dbeta, alpha = 0.5, 
                                                                                           args = list(shape1 = as.numeric(y['alpha']), shape2 = as.numeric(y['beta'])))) + theme_bw(base_size = 11) + 
                                                        theme(legend.position = 'none',plot.margin = unit(c(1,0,0,0.1), "pt"))))
           }

    all_combs <- tidyr::complete(distinct(
        dplyr::select(ht_tmp, dplyr::matches("setter_front_back"), dplyr::matches("setter_position"), dplyr::matches("start_zone"), dplyr::matches("attack_code"), dplyr::matches("set_code"), dplyr::matches("skill_type"))), .data[[{{ setter_position_by_var }}]], .data[[{{ attack_by_var }}]])
    
    ht <- if (packageVersion("dplyr") >= "1.0.0") {
              dplyr::arrange(full_join(ht, all_combs), dplyr::across({{ setter_position_by_var }}), across({{ attack_by_var }}))
          } else {
              dplyr::arrange(full_join(ht, all_combs), .data[[setter_position_by_var]], .data[[attack_by_var]])
          }

    ht <- mutate(ht, plot = ifelse(.data$plot == "NULL", 
                                  list(ggplot() + theme_void()), .data$plot))
    
    labels = dplyr::pull(tidyr::unite(ht, 'label', {{ setter_position_by_var }}, {{ attack_by_var }}, remove = FALSE, sep = " "), .data$label)
    
    cowplot::plot_grid(plotlist = ht$plot, labels = labels, nrow = length(setter_rotation_levels),  label_x = -0.04, label_y = 1.01)
    
    }

#' Print the prior table 
#' 
#' @param history_table data.frame: the `prior_table` component of the object returned by [ov_create_history_table()]
#' @param team string: team name
#' @param setter_id string: setter_id
#' 
#' @examples 
#' hist_dvw <- ovdata_example("190301_kats_beds")
#' history_table <- ov_create_history_table(dvw = hist_dvw, attack_by = "zone")
#' team = history_table$prior_table$team[1]
#' setter_id = history_table$prior_table$setter_id[1]
#' ov_print_history_table(history_table, team, setter_id)
#' @export
ov_print_history_table <- function(history_table, team, setter_id){
    
    team_select = team
    setter_select = setter_id
    my_pal <- scales::col_numeric(
        paletteer::paletteer_d(
            palette = "ggsci::orange_material"
        ) %>% as.character(),
        domain = NULL,
        na.color = "white"
    )
    if ((is.null(history_table$prior_table) || !is.data.frame(history_table$prior_table) || nrow(history_table$prior_table) < 1)) stop("History table is missing or empty")
    
    
    attack_by_var <- history_table$attack_by_var
    setter_position_by_var <- history_table$setter_position_by_var
    
    setter_rotation_levels = switch(setter_position_by_var,
                                    "setter_position" = c("4","3","2","5","6","1"),
                                    "setter_front_back" = c("front", "back"))
    
    if (packageVersion("dplyr") >= "1.0.0") {
        ht_tmp <- mutate(history_table$prior_table, 
                         dplyr::across(dplyr::matches("start_zone"), factor), 
                         dplyr::across(dplyr::matches("attack_code"), factor), 
                         dplyr::across(dplyr::matches("set_code"), factor), 
                         dplyr::across(dplyr::matches("skill_type"), factor), 
                         dplyr::across(dplyr::matches("setter_position"), factor, levels = setter_rotation_levels),
                         dplyr::across(dplyr::matches("setter_front_back"), factor, levels = setter_rotation_levels),
                         dplyr::across(dplyr::matches("ts_pass_quality"), factor, levels = c("Perfect", "Good", "OK", "Poor")),
                         kr = round(.data$alpha / (.data$alpha + .data$beta),2))
    } else {
        cls <- intersect(names(history_table$prior_table), c("start_zone", "attack_code", "set_code", "skill_type"))
        if (length(cls) > 0) ht_tmp <- dplyr::mutate_at(history_table$prior_table, cls, factor)
        if ("setter_position" %in% names(ht_tmp)) ht_tmp$setter_position <- factor(ht_tmp$setter_position, levels = setter_rotation_levels)
        if ("setter_front_back" %in% names(ht_tmp)) ht_tmp$setter_front_back <- factor(ht_tmp$setter_front_back, levels = setter_rotation_levels)
        if ("ts_pass_quality" %in% names(ht_tmp)) ht_tmp$ts_pass_quality <- factor(ht_tmp$ts_pass_quality, levels = c("Perfect", "Good", "OK", "Poor"))
        ht_tmp <- mutate(ht_tmp, kr = round(.data$alpha / (.data$alpha + .data$beta),2))
    }
    
    ht <-select(dplyr::filter(ht_tmp, .data$team %eq% team_select, .data$setter_id %eq% setter_select), dplyr::matches("setter_front_back"), dplyr::matches("setter_position"), .data$ts_pass_quality, .data$kr, dplyr::matches("start_zone"), dplyr::matches("set_code"), dplyr::matches("attack_code"), dplyr::matches("skill_type"))
    
    ht <- if (packageVersion("dplyr") >= "1.0.0") {
              group_by(arrange(pivot_wider(ht, names_from = {{ attack_by_var }}, values_from = .data$kr), {{ setter_position_by_var }}, .data$ts_pass_quality), dplyr::across({{ setter_position_by_var }}))
          } else {
              dplyr::group_by_at(arrange(pivot_wider(ht, names_from = {{ attack_by_var }}, values_from = .data$kr), {{ setter_position_by_var }}, .data$ts_pass_quality), setter_position_by_var)
          }
    gt::gt(ht,
           rowname_col = "ts_pass_quality")  %>% 
        gt::fmt_missing(columns = 1:ncol(ht),
                        missing_text = ".") %>% 
        gt::data_color(
            columns = 1:ncol(ht),
            colors = my_pal
        ) %>%
        gt::cols_align(align = "center",
                   columns = 1:ncol(ht)) 
    
}
