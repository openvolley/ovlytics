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
#' @param killRate_grouping string: Default to NULL, it will use 'attack by' grouping variables. Otherwise a set of additional grouping variables to calculate the kill rate. 
#' @param setter_position_by string: either "rotation" or "front_back"
#' @param history_table list: (only if `attack_options` is "use_history") the object returned by [ov_create_history_table()]
#' @param attack_by string: either "code", "zone", "tempo", "setter call", "attacker_name", "player_role"
#' @param exclude_attacks character: vector of attack codes to exclude
#' @param rotation string: (only relevant when `attack_by` is "player_role") either "SHM" (assume a setter-hitter-middle rotation order), or "SMH" (setter-middle-hitter)
#' @param shiny_progress numeric: an optional two-element vector. If not `NULL` or `NA`, [shiny::setProgress()] calls will be made during simulation with `value`s in this range
#'
#' @seealso [ov_create_history_table()]
#'
#' @examples
#' dvw <- ovdata_example("NCA-CUB")
#' system.time({
#'   ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception",
#'                                          n_sim = 100, attack_by = "setter call",
#'                                          setter_position_by = "front_back", filter_sim = TRUE)
#' })
#' @export
ov_simulate_setter_distribution <- function(dvw, play_phase = c("Reception", "Transition"), n_sim = 500, priors = list(name = "beta", par1 = 1, par2 = 1),
                                            epsilon = 1, filter_sim = FALSE, attack_options = "use_data", 
                                            killRate_grouping = NULL,
                                            setter_position_by = "rotation", history_table = NULL,
                                            attack_by = "code", exclude_attacks = c("PR"), rotation = "SHM", shiny_progress = NULL) {
    ## TODO check input parms
    attack_by <- match.arg(attack_by, c("zone", "code", "tempo", "setter call", "attacker_name", "player_role"))
    attack_by_var <- switch(attack_by,
                            "code" = "attack_code",
                            "zone" = "start_zone",
                            "tempo" = "skill_type",
                            "setter call" = "set_code",
                            "attacker_name" = "attacker_name",
                            "player_role" = "player_role")

    setter_position_by <- match.arg(setter_position_by, c("rotation", "front_back"))
    setter_position_by_var <- switch(setter_position_by,
                                     "rotation" = "setter_position",
                                     "front_back" = "setter_front_back")

    # Check that ards matches with history as well!
    attack_options <- match.arg(attack_options, c("use_data", "use_history"))
    rotation <- match.arg(rotation, c("SHM", "SMH"))
    raw_data <- if (is.character(dvw)) dv_read(dvw) else dvw

    data <- ov_augment_plays(plays(raw_data), to_add = c("touch_summaries", "setters", "player_role"), rotation = rotation)
    data <- mutate(data, attacker_name = case_when(.data$skill == "Attack" ~ .data$player_name))
    team_setter <- drop_na(dplyr::distinct(dplyr::select(dplyr::filter(data, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), "team", "setter_id")))

    # If setter call we need to propagate the setter call to the subsequent attack
    if (attack_by == "setter call"){
        data <- dplyr::mutate(data,
                              set_code = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ paste0(lag(.data$set_code), lag(.data$set_type)),
                                                   TRUE ~ .data$set_code),
                              set_description = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ lag(.data$set_description),
                                                          TRUE ~ .data$set_description)
        )
    }

    data <- dplyr::filter(dplyr::filter(data, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), !is.na(.data[[attack_by_var]]))

    sim <- actual <- rates <- rates_beta <- NULL

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
            tbleChoice <- dplyr::group_by(data_game, dplyr::across({{ setter_position_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))
            tbleChoice <- dplyr::ungroup(dplyr::summarize(tbleChoice, alpha = sum(.data$evaluation %eq% "Winning attack"), beta = sum(!(.data$evaluation %eq% "Winning attack")), n = dplyr::n()))
        
            
            
            if (attack_options == "use_data") {
                this <- tbleChoice
            } else if (attack_options == "use_history") {
                this <- dplyr::bind_rows(
                    #dplyr::select(mutate(dplyr::filter(history_table, .data$team == iTeam & .data$setter_id == iSetter), n_s = .data$KR * .data$n), -"KR", -"team", -"setter_id"),
                    dplyr::select(dplyr::filter(history_table$prior_table, .data$team == iTeam & .data$setter_id == iSetter), -"team", -"setter_id"),
                    #dplyr::select(mutate(tbleChoice, n_s = .data$KR * .data$n), -"KR"))
                    tbleChoice)
                this <- ungroup(dplyr::summarize(group_by(this, dplyr::across({{ setter_position_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }})), alpha = sum(.data$alpha), beta = sum(.data$beta), n = sum(.data$n)))
            }
            
            # Define the kill rate
            
            # We define the grouping for the KR - depending on the grouping chosen as arg into the function
            
            if(is.null(killRate_grouping)){
                killRate_grouping = attack_by_var
            }
            this_tmp <- this %>% group_by(dplyr::across({{ killRate_grouping }})) %>%
                dplyr::summarise(alpha = sum(.data$alpha), beta = sum(.data$beta), n = sum(.data$n)) %>%
                ungroup()

            thisKR <- dplyr::select(dplyr::mutate(this_tmp, KR = .data$alpha / (.data$alpha + .data$beta)), -"alpha", -"beta")
            pT_tmp <- left_join(dplyr::select(dplyr::mutate(this, play = .data$n>0), -"alpha", -"beta", -"n"), thisKR, by =  killRate_grouping )
            probTable <- pivot_wider(dplyr::select(pT_tmp, -"n", -"play"), names_from = {{ attack_by_var }}, values_from = "KR")
            
            if (FALSE) {
                ## use team_id in plot
                tableBB <- dplyr::mutate(data_game, score = paste(.data$home_team_id, .data$home_team_score, "-", .data$visiting_team_score, .data$visiting_team_id))
            } else {
                ## create a short team name automatically
                tableBB <- dplyr::mutate(data_game, score = paste(team_name_to_abbrev(.data$home_team), .data$home_team_score,
                                                                  "-",
                                                                  .data$visiting_team_score, team_name_to_abbrev(.data$visiting_team)))
            }

            tableBB <- dplyr::left_join(dplyr::select(tableBB, "set_number", "point_id", "score", {{ setter_position_by_var }}, "ts_pass_quality", {{ attack_by_var }}, "evaluation", "video_time"), 
                                        probTable, by = c(setter_position_by_var, "ts_pass_quality"))

            choice <- matrix(seq_len(ncol(tableBB) - 8), ncol = (ncol(tableBB) - 8), nrow = nrow(tableBB), byrow = TRUE)
            choice[which(is.na(as.matrix(tableBB[, seq(9, ncol(tableBB), by = 1)])))] <- NA
        
            probs <- as.matrix(tableBB[, seq(9, ncol(tableBB), by = 1)])
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
            
                #res_setter_position[res_sim_num == nS ] <- unlist(tableBB[,setter_position_by_var])
                #res_ts_pass_quality[res_sim_num == nS] <- tableBB$ts_pass_quality
                #res_point_id[res_sim_num == nS] <- tableBB$point_id
            
                res_setter_position[res_sim_num == nS ] <- tableBB[[setter_position_by_var]]
                res_ts_pass_quality[res_sim_num == nS] <- tableBB[["ts_pass_quality"]]
                res_point_id[res_sim_num == nS] <- tableBB[["point_id"]]

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
        
            this_tmp$team <- iTeam
            this_tmp$setter <- iSetter
            
            thisKR$team <- iTeam
            thisKR$setter <- iSetter
            
            sim <- bind_rows(sim, res)
            actual <- bind_rows(actual, tableBB)
            rates_beta <- bind_rows(rates_beta, this_tmp)
            tbleChoice <- dplyr::select(mutate(tbleChoice, KR = .data$alpha / (.data$alpha + .data$beta)), -"alpha", -"beta")
            rates <- bind_rows(rates, thisKR)
        }
    }

    sim_filter <- NULL
    
    if (isTRUE(filter_sim)) {
        
        tbleC <- ungroup(dplyr::summarize(group_by(sim, .data$attack_choice, .data$setter_position, .data$ts_pass_quality, .data$sim_num, .data$team, .data$setter),
                                          KR = mean(.data$reward), n = n()))
        join_vars  = c("team", "setter", "attack_choice" = {{ attack_by_var }})
        if(setter_position_by_var %in% colnames(rates)) join_vars  = c(join_vars, "setter_position" = {{ setter_position_by_var }})
        if("ts_pass_quality" %in% colnames(rates)) join_vars  = c(join_vars, "ts_pass_quality")
        tbleC <- left_join(tbleC, dplyr::rename(rates, true_KR = "KR", true_n = "n"), by = join_vars)
        
        summaryTC <- mutate(ungroup(tbleC), trueKR_up = .data$true_KR + 1.64*sqrt(.data$true_KR * (1 - .data$true_KR) / .data$true_n),
                            trueKR_down = .data$true_KR - 1.64*sqrt(.data$true_KR * (1 - .data$true_KR) / .data$true_n),
                            keepSim = case_when(.data$KR <= .data$trueKR_up & .data$KR >= .data$trueKR_down ~ TRUE,
                                                TRUE ~ FALSE))
        summaryTC <- mutate(unique(dplyr::select(dplyr::filter(summaryTC, !.data$keepSim), "sim_num", "team", "setter")), keepSim = FALSE)

        sim_filter <- dplyr::filter(left_join(sim, summaryTC, by = c("setter", "team", "sim_num")), is.na(.data$keepSim))
    }
    list(simulations = sim, actual = actual, rates = rates, rates_beta = rates_beta, filtered_simulations = sim_filter, attack_by_var = attack_by_var, setter_position_by_var = setter_position_by_var, raw_data = raw_data, conditional_simulations = cond)
}

#' Simulate a Bayesian Bandit choice for a given set of probabilities and a number of points for multiple games
#'
#' @param list_dv list: list of datavolley object as returned by [datavolley::dv_read()] 
#' @param play_phase character: one or both of "Reception", "Transition"
#' @param n_sim integer: number of simulations
#' @param priors numeric: prior distribution of the kill rate for the different attacking options
#' @param epsilon numeric: reward size
#' @param filter_sim logical:
#' @param attack_options string: either "use_data" or "use_history"
#' @param killRate_grouping string: Default to NULL, it will use 'attack by' grouping variables. Otherwise a set of additional grouping variables to calculate the kill rate. 
#' @param setter_position_by string: either "rotation" or "front_back"
#' @param history_table list: (only if `attack_options` is "use_history") the object returned by [ov_create_history_table()]
#' @param attack_by string: either "code", "zone", "tempo", "setter call", "attacker_name", "player_role"
#' @param exclude_attacks character: vector of attack codes to exclude
#' @param rotation string: (only relevant when `attack_by` is "player_role") either "SHM" (assume a setter-hitter-middle rotation order), or "SMH" (setter-middle-hitter)
#' @param shiny_progress numeric: an optional two-element vector. If not `NULL` or `NA`, [shiny::setProgress()] calls will be made during simulation with `value`s in this range
#'
#' @seealso [ov_simulate_setter_distribution()]
#'
#' @examples
#' list_dv <- list(dv_read(ovdata_example("NCA-CUB")), dv_read(ovdata_example("NCA-CUB")))
#' system.time({
#'   mssd <- ov_simulate_multiple_setter_distribution(list_dv = list_dv, play_phase = "Reception",
#'                n_sim = 100, setter_position_by = "front_back")
#' })
#' @export
ov_simulate_multiple_setter_distribution <- function(list_dv, play_phase = c("Reception", "Transition"), n_sim = 500, priors = list(name = "beta", par1 = 1, par2 = 1),
                                                     epsilon = 1, filter_sim = FALSE, attack_options = "use_data", 
                                                     killRate_grouping = NULL,
                                                     setter_position_by = "rotation", history_table = NULL,
                                                     attack_by = "code", exclude_attacks = c("PR"), rotation = "SHM", shiny_progress = NULL) {
    n_dv = length(list_dv)
    lapply(seq_len(n_dv), function(x) ov_simulate_setter_distribution(list_dv[[x]], play_phase = play_phase, n_sim = n_sim, priors = priors, epsilon = epsilon, filter_sim = filter_sim,
                                                                attack_options = attack_options, setter_position_by = setter_position_by, history_table = history_table,
                                                                attack_by = attack_by, exclude_attacks = exclude_attacks, rotation = rotation, shiny_progress = (shiny_progress / n_dv + x/n_dv)))
}

#' Plot a simulated setter distribution
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param overlay_set_number boolean: if `TRUE`, overlay set number and score in the plot
#' @param label_setters_by string: either "id" or "name"
#' @param font_size numeric: font size
#'
#' @examples
#' dvw <- ovdata_example("NCA-CUB")
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
    # "period_on_court" as a period of successive points played

    dataplays <- ov_augment_plays(plays(ssd$raw_data), to_add = c("touch_summaries", "setters"))
    temp <-  dplyr::select(dplyr::slice(dplyr::group_by(dplyr::filter(dataplays,!is.na(.data$skill) & !is.na(.data$home_setter_id) | !is.na(.data$visiting_setter_id)) ,.data$match_id, .data$point_id), 1L), "match_id", "point_id", "home_setter_id", "visiting_setter_id")
    temp$home_setter_period_on_court <- paste0(temp$home_setter_id, expand_rle(rle(temp$home_setter_id)$lengths))
    temp$visiting_setter_period_on_court <- paste0(temp$visiting_setter_id, expand_rle(rle(temp$visiting_setter_id)$lengths))

    bbtrajqi <- mutate(left_join(bbtrajqi,
        dplyr::select(ungroup(temp), "point_id", "home_setter_period_on_court", "visiting_setter_period_on_court"), by = "point_id"),
        period_on_court = case_when(.data$team %eq% datavolley::home_team(ssd$raw_data) ~ .data$home_setter_period_on_court,
                                           .data$team %eq% datavolley::visiting_team(ssd$raw_data) ~ .data$visiting_setter_period_on_court))

    tbC <- mutate(group_by(ssd$actual, .data$team, .data$setter),
                  pts = cumsum(.data$evaluation == "Winning attack"), time = row_number())

    tbC <- mutate(left_join(tbC,
                            dplyr::select(ungroup(temp), "point_id", "home_setter_period_on_court", "visiting_setter_period_on_court"), by = "point_id"),
                  period_on_court = case_when(.data$team %eq% datavolley::home_team(ssd$raw_data) ~ .data$home_setter_period_on_court,
                                              .data$team %eq% datavolley::visiting_team(ssd$raw_data) ~ .data$visiting_setter_period_on_court))
    
    pts_id_score <- tbC %>% ungroup() %>% dplyr::select("point_id", "score") %>% distinct() %>% dplyr::arrange(.data$point_id)
    label_breaks = round(seq(2, to = nrow(pts_id_score), length.out = max(tbC$set_number)*4))
    g <- 
        ggplot(tbC) +
        geom_line(data = bbtrajqi, aes_string(x = "point_id", y = "trajqim", group = "period_on_court"), col = "orange") +
        geom_ribbon(data = bbtrajqi, aes_string(x = "point_id", ymin = "trajqi05", ymax = "trajqi95", group = "period_on_court"), col = "white", fill = "orange", alpha = 0.25) +
        geom_line(aes_string(x = "point_id", y = "pts", linetype = "setter")) + ## previously also group = "period_on_court", but this causes problems with varying linetype and isn't needed anyway?
        labs(x = "", y = "Cumulative points scored") + facet_wrap("team", scales = "free") + 
            ggplot2::scale_x_continuous(breaks=pts_id_score$point_id[label_breaks],labels=pts_id_score$score[label_breaks], 
                                        guide = ggplot2::guide_axis(n.dodge = 2))

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
    g + theme_bw(base_size = font_size) + theme(legend.position = "bottom")
}


ssd_set_setter_labels <- function(ssd, label_setters_by = "id") {
    assert_that(is.string(label_setters_by))
    label_setters_by <- tolower(label_setters_by)
    label_setters_by <- match.arg(label_setters_by, c("id", "name"))
    if (label_setters_by == "name") {
        ## figure the player_id -> player_name mapping
        pnid <- na.omit(dplyr::select(ssd$raw_data$plays, "team", "player_id", "player_name"))
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
#' @param output string: either "plot" or "list"
#'
#' @examples
#' dvw <- ovdata_example("NCA-CUB")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception", "Transition"),
#'                                           n_sim = 100, attack_by = "code")
#' ov_plot_distribution(setter)
#' @export
ov_plot_distribution <- function(ssd, label_setters_by = "id", font_size = 11, title_wrap = NA, output = "plot") {
    output <- match.arg(output, c("list", "plot"))
    ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
    twrapf <- if (is.na(title_wrap)) function(z) z else function(z) paste0(strwrap(z, title_wrap), collapse = "\n")
    attack_by_var <- ssd$attack_by_var
    setter_position_by_var <- ssd$setter_position_by_var

    attack_zones_sim <- dplyr::summarize(group_by(ssd$simulations, .data$team, .data$setter, .data$setter_position, .data$attack_choice), n_attacks = n())
    attack_zones_sim <- ungroup(mutate(attack_zones_sim, rate = .data$n_attacks / sum(.data$n_attacks)))
    attack_zones_actual <- dplyr::summarize(group_by(ssd$actual, .data$team, .data$setter, dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }})), n_attacks = n())
    attack_zones_actual <- ungroup(mutate(attack_zones_actual, rate = .data$n_attacks / sum(.data$n_attacks)))
    setter_team <- distinct(dplyr::select(attack_zones_actual, "team", "setter"))

    setter_rotation_levels = switch(setter_position_by_var,
                                    "setter_position" = c("4","3","2","5","6","1"),
                                    "setter_front_back" = c("front", "back"))

    if (attack_by_var == "attack_code") {
        ## hmm, the meta$attacks data.frame can be a bit messy ... the attack location is either "X8" or "V8" or "start_coordinate"
        atk_loc_var <- head(intersect(names(ssd$raw_data$meta$attacks), c("X8", "V8", "start_coordinate")), 1)
        if (length(atk_loc_var) < 1 || !atk_loc_var %in% names(ssd$raw_data$meta$attacks)) stop("could not plot attack distribution, missing attack start location in meta$attacks table")
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
                ggcourt(court = "lower", labels = "") + scale_size(guide = "none") +
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
                                   attack_choice = forcats::fct_relevel(as.factor(as.character(.data$attack_choice)), levels(.data$attack_choice)),
                                   start_zone = case_when(stringr::str_ends(.data$attack_choice, "F") ~ 4,
                                                          stringr::str_ends(.data$attack_choice, "C") ~ 3,
                                                          stringr::str_ends(.data$attack_choice, "P") ~ 8,
                                                          stringr::str_ends(.data$attack_choice, "B") & .data$rotation %in% c("1", "6", "5", "back") ~ 2,
                                                          stringr::str_ends(.data$attack_choice, "B") & .data$rotation %in% c("2", "3", "4", "front")  ~ 9),
                                   setter_call = stringr::str_trunc(as.character(.data$attack_choice), 2, side = "right", ellipsis = ""),
                                   setter_call = case_when(.data$setter_call == "NA" ~ "No Call",
                                                           TRUE ~ .data$setter_call))

        attack_zones_sim <- mutate(cbind(attack_zones_sim, dv_xy(as.numeric(attack_zones_sim$start_zone), end = "lower")))

        if(setter_position_by_var == "setter_position"){
            attack_zones_actual <- mutate(attack_zones_actual, start_zone = case_when(stringr::str_ends(.data$set_code, "F") ~ 4,
                                                                                      stringr::str_ends(.data$set_code, "C") ~ 3,
                                                                                      stringr::str_ends(.data$set_code, "P") ~ 8,
                                                                                      stringr::str_ends(.data$set_code, "B") & .data$setter_position %in% c("1", "6", "5", "back") ~ 2,
                                                                                      stringr::str_ends(.data$set_code, "B") & .data$setter_position %in% c("2", "3", "4", "front")  ~ 9),
                                          setter_call = stringr::str_trunc(as.character(.data$set_code), 2, side = "right", ellipsis = ""),
                                          setter_call = case_when(.data$setter_call == "NA" ~ "No Call",
                                                                  TRUE ~ .data$setter_call))
        }
        if(setter_position_by_var == "setter_front_back"){
            attack_zones_actual <- mutate(attack_zones_actual, start_zone = case_when(stringr::str_ends(.data$set_code, "F") ~ 4,
                                                                                      stringr::str_ends(.data$set_code, "C") ~ 3,
                                                                                      stringr::str_ends(.data$set_code, "P") ~ 8,
                                                                                      stringr::str_ends(.data$set_code, "B") & .data$setter_front_back %in% c("1", "6", "5", "back") ~ 2,
                                                                                      stringr::str_ends(.data$set_code, "B") & .data$setter_front_back %in% c("2", "3", "4", "front")  ~ 9),
                                          setter_call = stringr::str_trunc(as.character(.data$set_code), 2, side = "right", ellipsis = ""),
                                          setter_call = case_when(.data$setter_call == "NA" ~ "No Call",
                                                                  TRUE ~ .data$setter_call))
        }
        attack_zones_actual <- cbind(attack_zones_actual, dv_xy(attack_zones_actual$start_zone, end = "lower"))

        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)

        calls_arrows <- dplyr::filter(ssd$raw_data$meta$sets, .data$start_coordinate > 0)
        calls_arrows <- cbind(calls_arrows,
                             datavolley::dv_index2xy(calls_arrows$start_coordinate),
                             datavolley::dv_index2xy(calls_arrows$mid_coordinate),
                             datavolley::dv_index2xy(calls_arrows$end_coordinate))

        colnames(calls_arrows)[(ncol(calls_arrows)-5):ncol(calls_arrows)] <- c("start_x", "start_y","mid_x","mid_y", "end_x", "end_y")

        calls_arrows <- calls_arrows %>% dplyr::select("code", "start_x", "start_y",  "mid_x", "mid_y", "end_x", "end_y") %>% dplyr::rename(setter_call = "code")

        calls_arrows_f <- NULL
        for(srl in setter_rotation_levels){
            calls_arrows_tmp <- mutate(calls_arrows, rotation = srl)
            calls_arrows_f <- bind_rows(calls_arrows_f, calls_arrows_tmp)
        }
        suppressWarnings(calls_arrows_f$rotation <- forcats::fct_relevel(as.factor(calls_arrows_f$rotation), setter_rotation_levels))

        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")),
                   aes_string(x = "x", y = "y")) + geom_tile(aes_string(fill = "rate")) + ggcourt(court = "lower", labels = "", base_size = font_size) +
                scale_fill_gradient2(name = "Attack rate") + facet_grid(rotation~setter_call) + theme(legend.position = "none") +
                geom_text(aes_string(x = "x", y ="y", label = "lab"), size = font_size/11*9.5*0.35278) +
                geom_segment(data=calls_arrows_f, aes_string(x = "mid_x", y = "mid_y", xend = "mid_x", yend = "mid_y"))+
                geom_segment(data=calls_arrows_f, aes_string(x = "mid_x", y = "mid_y", xend = "end_x", yend = "end_y"),
                             arrow = arrow(length = unit(0.03, "npc"))) +
                ggtitle(twrapf(paste0("Actual distribution - ", yy, " (", xx, ")")))
        })

        gSim <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_sim, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")),
                   aes_string(x = "x", y = "y")) + geom_tile(aes_string(fill = "rate")) + ggcourt(court = "lower", labels = "", base_size = font_size) +
                scale_fill_gradient2(name = "Attack rate") + facet_grid(rotation~setter_call) +
                geom_text(aes_string(x = "x", y = "y", label = "lab"), size = font_size/11*9.5*0.35278) +
                geom_segment(data=calls_arrows_f, aes_string(x = "mid_x", y = "mid_y", xend = "mid_x", yend = "mid_y"))+
                geom_segment(data=calls_arrows_f, aes_string(x = "mid_x", y = "mid_y", xend = "end_x", yend = "end_y"),
                             arrow = arrow(length = unit(0.03, "npc"))) +
                theme(legend.position = "none") + ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
        })

    }
    if (attack_by_var == "player_role") {
        
        attack_zones_sim <- mutate(attack_zones_sim,
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), setter_rotation_levels))
        attack_zones_actual <- mutate(attack_zones_actual)
        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)
        
        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")), aes_string(x = "player_role"))  +
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
    if (attack_by_var == "attacker_name") {
        
        attack_zones_sim <- mutate(attack_zones_sim,
                                   rotation = forcats::fct_relevel(as.factor(as.character(.data$setter_position)), setter_rotation_levels))
        #attack_zones_actual <- mutate(attack_zones_actual)
        attack_zones_actual$rotation <- forcats::fct_relevel(as.factor(as.character(unlist(attack_zones_actual[,setter_position_by_var]))), setter_rotation_levels)
        
        gActual <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
            ggplot(mutate(dplyr::filter(attack_zones_actual, .data$team == xx & .data$setter == yy),
                          lab = paste0(round(.data$rate, 2) * 100, "%")), aes_string(x = "attacker_name"))  +
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
    if (output == "plot") {
        wrap_plots(c(gActual, gSim), nrow = 2) + plot_layout(guides = "collect")
    } else {
        list(actual = gActual, sim = gSim)
    }
}


#' Plot a simulated setter distribution sequence
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param label_setters_by string: either "id" or "name"
#' @param font_size numeric: font size
#' @param title_wrap numeric: if non-`NA`, use [strwrap()] to break the title into lines of this width
#' @param split_set boolean: if `TRUE`, separate the distribution sequence by set
#' @param output string: either "plot" or "list"
#'
#' @examples
#' dvw <- ovdata_example("NCA-CUB")
#' ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception"),
#'                                        n_sim = 100, attack_by = "zone",
#'                                        setter_position_by = "front_back")
#' ov_plot_sequence_distribution(ssd)
#'
#' @export
ov_plot_sequence_distribution <- function(ssd, label_setters_by = "id", font_size = 11, title_wrap = NA, split_set = FALSE, output = "plot") {
    output <- match.arg(output, c("list", "plot"))
    attack_by_var <- ssd$attack_by_var
    setter_position_by_var <- ssd$setter_position_by_var
    ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
    twrapf <- if (is.na(title_wrap)) function(z) z else function(z) paste0(strwrap(z, title_wrap), collapse = "\n")
    ## conditional distributions
    cd_bandit <- dplyr::select(ssd$conditional_simulations, dplyr::contains("Bandit choice"), "team", "setter", "set_number", "score", "point_id", {{ setter_position_by_var }}, "ts_pass_quality", {{ attack_by_var }})
    cd_bandit <- ungroup(mutate(group_by(cd_bandit, .data$team, .data$setter), time = row_number()))
    cd_bandit <- pivot_longer(cd_bandit, cols = dplyr::contains("Bandit choice"), names_to = "attack_choice_b", values_to = "Probability") %>% drop_na()
    cd_bandit <- mutate(cd_bandit, attack_choice_b = stringr::str_remove(.data$attack_choice_b, "Bandit choice "))
    cd_bandit$attack_choice <- as.factor(cd_bandit[[attack_by_var]])

    cd_setter <- ssd$conditional_simulations
    cd_setter$attack_choice <- as.factor(cd_setter[[attack_by_var]])
    cd_setter <- mutate(group_by(cd_setter, .data$team, .data$setter), time = row_number())

    cd_setter$ts_pass_quality <- forcats::fct_relevel(as.factor(as.character(unlist(cd_setter$ts_pass_quality))), c("Poor", "OK", "Good", "Perfect"))

    cd_bandit <- ungroup(mutate(group_by(cd_bandit, .data$team, .data$setter, .data$time, .data$point_id),
                                choice_bandit = case_when(.data$Probability == max(.data$Probability, na.rm=TRUE) ~ .data$attack_choice_b),
                                least_likely_choice = case_when(.data$Probability == min(.data$Probability, na.rm=TRUE) & .data$Probability < 1 ~ .data$attack_choice_b)))

    cd_bandit$ts_pass_quality <- forcats::fct_relevel(as.factor(as.character(unlist(cd_bandit$ts_pass_quality))), c("Poor", "OK", "Good", "Perfect"))

    cd_bandit$Probability <- ggplot2::cut_interval(cd_bandit$Probability, 4)

    setter_team <- dplyr::arrange(distinct(dplyr::select(cd_setter, "team", "setter")), .data$team, .data$setter)

    gCondDist <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
    
        cd_quality <- bind_rows(dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy) %>% droplevels() %>%
                                   dplyr::select("time", "choice_bandit") %>% dplyr::rename(attack_choice = "choice_bandit") %>% mutate(choice_quality = "most likely") %>% drop_na(.data$attack_choice),
                               dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy & as.character(.data$attack_choice) == .data$least_likely_choice) %>% 
                                   droplevels() %>%
                                   dplyr::select("time", "least_likely_choice") %>% dplyr::rename(attack_choice = "least_likely_choice") %>% mutate(choice_quality = "least likely") %>% drop_na(.data$attack_choice))
    
        g1 <- 
            ggplot(data = dplyr::filter(cd_setter, .data$team == xx, .data$setter == yy) %>% droplevels(), aes_string(x = "time", y = "attack_choice")) +
            geom_tile(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy) %>% droplevels(), aes_string(x = "time + 0.5", y = "attack_choice_b", fill ="Probability"),
                      alpha = 0.75, show.legend = FALSE) +
            geom_step(group = 1) +
            geom_point(data = dplyr::filter(cd_bandit, .data$team == xx & .data$setter == yy) %>% droplevels(), aes_string(x = "time + 0.5", y = "choice_bandit"), col = "white", size = 1) +
            geom_point(data = cd_quality, aes_string(x = "time + 0.5", y = "attack_choice", col = "choice_quality"), size = 1) +
            theme_bw(base_size = font_size) +
            #scale_fill_continuous(na.value = NA, limits = c(0, 1)) +
            theme(legend.position = "bottom") + labs(x = "Game history", y = "Attack choice") +
            scale_colour_manual("Option", values = c("blue", "darkred")) + scale_fill_brewer(palette = "OrRd") +
            ggtitle(twrapf(paste0("Bandit distribution - ", yy, " (", xx, ")")))
    
        cd_setter[[setter_position_by_var]] <- stringr::str_trunc(cd_setter[[setter_position_by_var]],1 ,side = "right", ellipsis = "")
        g2 <- ggplot(data = dplyr::filter(cd_setter, .data$team == xx, .data$setter == yy)%>% droplevels(), aes_string(x = "time")) +
            geom_tile(aes_string(fill = "ts_pass_quality", y ="1")) +
            geom_text(aes_string(y="1", label = setter_position_by_var), size = 2)+
            scale_fill_brewer()+
            theme_void() + theme(legend.position = "none")

        if (output == "list") {
            list(g1, g2)
        } else {
            if (split_set) {
                (g1  + facet_wrap(~set_number, scales = "free_x")) / (g2 + facet_wrap(~set_number, scales = "free_x"))
            } else {
                g1 / g2 + patchwork::plot_layout(heights = c(8, 1))
            }
        }
    })

    dfCondDist <- purrr::map2(setter_team$team, setter_team$setter, function(xx, yy) {
        dplyr::filter(cd_setter, .data$team == xx, .data$setter == yy) %>% droplevels()
    })

    if(output == "list"){return(list(plots = gCondDist, data = dfCondDist))}
    if(output == "plot"){return(wrap_plots(c(gCondDist)) + plot_layout(guides = "collect"))}
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
#' hist_dvw <- ovdata_example("NCA-CUB")
#' history_table <- ov_create_history_table(dvw = hist_dvw, attack_by = "attacker_name",
#'                                    setter_position_by = "front_back")
#'
#' ## use it on another file (here, the same file for demo purposes)
#' ## usually the history would be from a reference set of previous matches
#'
#' dvw <- ovdata_example("NCA-CUB")
#' setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception", n_sim = 100,
#'                                   attack_by = "attacker_name", attack_options = "use_history",
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
    attack_by <- match.arg(attack_by, c("zone", "code", "tempo", "setter call", "attacker_name", "player_role"))
    attack_by_var <- switch(attack_by,
                            "code" = "attack_code",
                            "zone" = "start_zone",
                            "tempo" = "skill_type",
                            "setter call" = "set_code",
                            "attacker_name" = "attacker_name",
                            "player_role" = "player_role")
    
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
    data_game <- mutate(data_game, attacker_name = case_when(.data$skill == "Attack" ~ .data$player_name))

    if(attack_by == "setter call"){
        data_game <- dplyr::mutate(data_game,
                                   set_code = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ paste0(lag(.data$set_code), lag(.data$set_type)),
                                                        TRUE ~ .data$set_code),
                                   set_description = case_when(.data$skill == "Attack" & lag(.data$skill) == "Set" ~ lag(.data$set_description),
                                                               TRUE ~ .data$set_description)
        )
    }


    data_game <- dplyr::filter(dplyr::filter(data_game, .data$skill == "Attack" & !.data$attack_code %in% exclude_attacks & tolower(.data$phase) %in% tolower(play_phase)), !is.na(.data[[attack_by_var]]))

    ## prior_table <- group_by(data_game, .data$team, .data$setter_id, .data$setter_position, .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))
    prior_table <- group_by(data_game,.data$match_id, .data$team, .data$setter_id, dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))

    prior_table <- ungroup(dplyr::summarize(prior_table, alpha = sum(.data$evaluation %eq% "Winning attack"), beta = sum(!(.data$evaluation %eq% "Winning attack")), n = n()))

    prior_table <- group_by(prior_table,.data$team, .data$setter_id, dplyr::across({{ setter_position_by_var }}), .data$ts_pass_quality, dplyr::across({{ attack_by_var }}))

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
#' hist_dvw <- ovdata_example("NCA-CUB")
#' history_table <- ov_create_history_table(dvw = hist_dvw, setter_position_by = "front_back",
#'                                                     normalize_parameters = FALSE)
#' team = unique(history_table$prior_table$team)[1]
#' setter_id = unique(history_table$prior_table$setter_id)[1]
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
    ht_tmp <- mutate(history_table$prior_table,
                     dplyr::across(dplyr::matches("start_zone"), factor),
                     dplyr::across(dplyr::matches("attack_code"), factor),
                     dplyr::across(dplyr::matches("set_code"), factor),
                     dplyr::across(dplyr::matches("skill_type"), factor),
                     dplyr::across(dplyr::matches("setter_position"), factor, levels = setter_rotation_levels),
                     dplyr::across(dplyr::matches("setter_front_back"), factor, levels = setter_rotation_levels),
                     dplyr::across(dplyr::matches("ts_pass_quality"), factor, levels = c("Perfect", "Good", "OK", "Poor")))

    ht <-  mutate(nest(dplyr::group_by(dplyr::filter(ht_tmp, .data$team %eq% team_select, .data$setter_id %eq% setter_select),
                                              dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }}))),
                  plot = purrr::map(.data$data, ~
                                                    ggplot(df, aes_string(x = "thetaBounds")) + xlab("") + ylab("") + ylim(c(0, 6)) +
                                                    apply(.x, 1, function(y) geom_area(aes(fill = y["ts_pass_quality"], col = y["ts_pass_quality"]), stat = "function", fun = dbeta, alpha = 0.5,
                                                                                       args = list(shape1 = as.numeric(y["alpha"]), shape2 = as.numeric(y["beta"])))) + theme_bw(base_size = 11) +
                                                    theme(legend.position = "none", plot.margin = unit(c(1, 0, 0, 0.1), "pt"))))

    all_combs <- complete(distinct(
        dplyr::select(ht_tmp, dplyr::matches("setter_front_back"), dplyr::matches("setter_position"), dplyr::matches("start_zone"), dplyr::matches("attack_code"), dplyr::matches("set_code"), dplyr::matches("skill_type"))), .data[[{{ setter_position_by_var }}]], .data[[{{ attack_by_var }}]])

    ht <- dplyr::arrange(full_join(ht, all_combs), dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }}))

    ht <- mutate(ht, plot = ifelse(.data$plot == "NULL",
                                  list(ggplot() + theme_void()), .data$plot))

    labels = dplyr::pull(unite(ht, "label", {{ setter_position_by_var }}, {{ attack_by_var }}, remove = FALSE, sep = " "), .data$label)

    cowplot::plot_grid(plotlist = ht$plot, labels = labels, nrow = length(setter_rotation_levels),  label_x = -0.04, label_y = 1.01)

    }

#' Print the prior table
#'
#' @param history_table data.frame: the `prior_table` component of the object returned by [ov_create_history_table()]
#' @param team string: team name
#' @param setter_id string: setter_id
#'
#' @examples
#' hist_dvw <- ovdata_example("NCA-CUB")
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

    ht_tmp <- mutate(history_table$prior_table,
                     dplyr::across(dplyr::matches("start_zone"), factor),
                     dplyr::across(dplyr::matches("attack_code"), factor),
                     dplyr::across(dplyr::matches("set_code"), factor),
                     dplyr::across(dplyr::matches("skill_type"), factor),
                     dplyr::across(dplyr::matches("setter_position"), factor, levels = setter_rotation_levels),
                     dplyr::across(dplyr::matches("setter_front_back"), factor, levels = setter_rotation_levels),
                     dplyr::across(dplyr::matches("ts_pass_quality"), factor, levels = c("Perfect", "Good", "OK", "Poor")),
                     kr = round(.data$alpha / (.data$alpha + .data$beta), 2))

    ht <- dplyr::filter(ht_tmp, .data$team %eq% team_select, .data$setter_id %eq% setter_select) %>% dplyr::select(dplyr::matches("setter_front_back"), dplyr::matches("setter_position"), "ts_pass_quality", "kr", dplyr::matches("start_zone"), dplyr::matches("set_code"), dplyr::matches("attack_code"), dplyr::matches("skill_type"))

    ht <- group_by(dplyr::arrange(pivot_wider(ht, names_from = {{ attack_by_var }}, values_from = "kr"), {{ setter_position_by_var }}, .data$ts_pass_quality), dplyr::across({{ setter_position_by_var }}))
    gt::gt(ht, rowname_col = "ts_pass_quality")  %>%
        gt::fmt_missing(columns = seq_len(ncol(ht)), missing_text = ".") %>%
        gt::data_color(columns = seq_len(ncol(ht)), colors = my_pal) %>%
        gt::cols_align(align = "center", columns = seq_len(ncol(ht)))
}


#' Print the rate table
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param team string: team name
#' @param setter_id string: setter_id
#'
#' @examples
#' dvw <- ovdata_example("NCA-CUB")
#' system.time({
#'   ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception",
#'                                          n_sim = 100, setter_position_by = "front_back")
#'   team <- ssd$raw_data$meta$teams$team[1]
#'   setter_id <- ssd$raw_data$meta$players_h$player_id[which(ssd$raw_data$meta$players_h$role == "setter")][2]
#'   ov_print_rate_table(ssd, team, setter_id)
#' })
#' @export
ov_print_rate_table <- function(ssd, team, setter_id){
    team_name <- team
    table_df <- dplyr::filter(ssd$rates, .data$team == team_name, .data$setter == setter_id) %>%
        dplyr::select(-"n", -"team", -"setter") %>% drop_na()  %>%
        pivot_wider(names_from = ssd$attack_by_var, values_from = "KR")
    if ("ts_pass_quality" %in% names(table_df)) table_df <- table_df %>% dplyr::rename("Pass quality" = "ts_pass_quality")
    if ("setter_front_back" %in% names(table_df)) table_df <- table_df %>% dplyr::rename("Front/Back" = "setter_front_back")
    reactable::reactable(table_df, pagination = FALSE,
                         defaultColDef = reactable::colDef(cell = reactablefmtr::data_bars(table_df, text_position = "above", fill_color = viridisLite::viridis(5),
                                                                                           background = "transparent", round_edges = TRUE, number_fmt = scales::percent)))
}


#' Plot the rates
#'
#' @param ssd simulated setter distribution output as returned by [ov_simulate_setter_distribution()]
#' @param team string: team name
#' @param setter_id string: setter_id
#' @param range vector of maximum and minimum quantile value description
#'
#' @examples
#' dvw <- ovdata_example("NCA-CUB")
#' system.time({
#'   ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception", "Transition"),
#'                                          n_sim = 150, setter_position_by = "front_back", 
#'                                           attack_by = "zone")
#'   team <- ssd$raw_data$meta$teams$team[1]
#'   setter_id <- ssd$raw_data$meta$players_h$player_id[which(ssd$raw_data$meta$players_h$role == "setter")][2]
#'   ov_plot_rate(ssd, team, setter_id)
#' })
#' @export
ov_plot_rate <- function(ssd, team, setter_id, range = c(0.05, 0.95)){
    team_name <- team
    table_df <- dplyr::filter(ssd$rates_beta, .data$team == team_name, .data$setter == setter_id) %>%
        dplyr::select(-"n", -"team", -"setter") %>% drop_na()  %>% 
        mutate(qlow = qbeta(range[1], shape1 = .data$alpha, shape2  = .data$beta), 
               mean = .data$alpha / (.data$alpha + .data$beta), 
               qup = qbeta(range[2], shape1 = .data$alpha, shape2  = .data$beta))
    if ("ts_pass_quality" %in% names(table_df)){
        table_df <- table_df %>% 
        mutate(ts_pass_quality = factor(.data$ts_pass_quality, c("Perfect", "Good", "OK", "Poor"))) %>% 
        dplyr::rename("Pass quality" = "ts_pass_quality")
    g <- ggplot(data = table_df %>% mutate(x_axis = as.factor(!!rlang::sym(ssd$attack_by_var)))) + 
        ggplot2::geom_pointrange(aes_string(ymin = "qlow", ymax = "qup",y = "mean",
                                           x = "x_axis", group = "`Pass quality`", col = "`Pass quality`"), 
                     position = ggplot2::position_dodge(1), linewidth = 1.5, size = 0.75) + 
        ggplot2::scale_color_brewer(direction = -1)+ scale_y_continuous(labels = scales::percent) +
        {if(ssd$setter_position_by_var %in% names(table_df)) facet_wrap(dplyr::sym(ssd$setter_position_by_var))}+
        theme_bw() + xlab(ssd$attack_by_var) + ylab("Attack kill rate range")+ 
        ggtitle(paste0("Kill rate: ", setter_id,"(",team_name,")"))
    } else {
        g <- ggplot(data = table_df %>% mutate(x_axis = as.factor(!!rlang::sym(ssd$attack_by_var)))) + 
            ggplot2::geom_pointrange(aes_string(ymin = "qlow", ymax = "qup",y = "mean",  x = "x_axis"), 
                                     position = ggplot2::position_dodge(1), linewidth = 1.5, size = 0.75) + 
            ggplot2::scale_color_brewer(direction = -1)+ scale_y_continuous(labels = scales::percent) +
           {if(ssd$setter_position_by_var %in% names(table_df)) facet_wrap(dplyr::sym(ssd$setter_position_by_var))}+
            theme_bw() + xlab(ssd$attack_by_var) + ylab("Attack kill rate range") + 
            ggtitle(paste0("Kill rate: ", setter_id," (",team_name,")"))
    }
    g
}

#' Table of a simulated multi-game setter distribution sequence
#'
#' @param mssd simulated multi-game setter distribution output as returned by [ov_simulate_multiple_setter_distribution()]
#' @param label_setters_by string: either "id" or "name"
#' @param team NULL or string: if non-NULL, show sequence just for this team name
#' @param nrows integer: number of rows per page in the table
#' @param groupBy boolean: if TRUE, will group the rows by Opponent
#'
#' @examples
#' \dontrun{
#'  list_dv <- list(dv_read(ovdata_example("NCA-CUB"))) # would normally be multiple games
#'  mssd <- ov_simulate_multiple_setter_distribution(list_dv = list_dv,
#'              play_phase = c("Reception", "Transition"), attack_by = "attacker_name",
#'              n_sim = 100, setter_position_by = "front_back")
#'
#'  res <- ov_table_mssd(mssd, team = "NICARAGUA")
#' }
#' @export
ov_table_mssd <- function(mssd, label_setters_by = "name", team = NULL, nrows = 50, groupBy = TRUE) {
    team_select <- team

    rating_column <- function(maxWidth = 55, ...) reactable::colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)

    make_color_pal <- function(colors, bias = 1) {
        get_color <- colorRamp(colors, bias = bias)
        function(x) rgb(get_color(x), maxColorValue = 255)
    }

    temppal_cold <- c("#36a1d6", "#76b8de", "#a0bfd9", "#ffffff")
    temppal_hot <- c("#ffffff", "#d88359", "#d65440", "#c62c34")
    temppal_balanced <- c("#ffffff", "lightgreen", "forestgreen", "darkgreen")
    #temppal_hot_cold <- c("#36a1d6", "#76b8de", "#a0bfd9","#ffffff", "#d88359", "#d65440", "#c62c34")
    temppal_hot_cold <-rev(colorRampPalette(colors = c("#c62c34", "white", "#36a1d6"))(9))
    choices_rating_color <- make_color_pal(temppal_hot_cold, bias = 1)
    exploration_rating_color <- make_color_pal(rev(temppal_cold), bias = 1)
    exploitation_rating_color <- make_color_pal(temppal_hot, bias = 1)
    balanced_rating_color <- make_color_pal(temppal_balanced, bias = 1)
    full_dd <- NULL
    for (dvo in seq_along(mssd)) {
        ssd <- mssd[[dvo]]
        ssd <- ssd_set_setter_labels(ssd, label_setters_by = label_setters_by)
        bbtrajqi <- mutate(group_by(ssd$simulations, .data$sim_num, .data$team, .data$setter),
                           traj = cumsum(.data$reward))
        bbtrajqi <- ungroup(dplyr::summarize(group_by(bbtrajqi, .data$time,.data$point_id,  .data$team, .data$setter),
                                             trajqi05 = quantile(.data$traj, 0.05), trajqi95 = quantile(.data$traj, 0.95),
                                             trajqim = mean(.data$traj)))

        ## Now, in case a setter comes in and out, we need to clean up the plot a bit. Let's define
        ## "period_on_court" as a period of successive points played

        dataplays <- ov_augment_plays(plays(ssd$raw_data), to_add = c("touch_summaries", "setters"))
        temp <-  dplyr::select(dplyr::slice(dplyr::group_by(dplyr::filter(dataplays, !is.na(.data$skill) & !is.na(.data$home_setter_id) & !is.na(.data$visiting_setter_id)) ,.data$match_id, .data$point_id), 1L), "match_id", "point_id", "home_setter_id", "visiting_setter_id")
        temp$home_setter_period_on_court <- paste0(temp$home_setter_id, expand_rle(rle(temp$home_setter_id)$lengths))
        temp$visiting_setter_period_on_court <- paste0(temp$visiting_setter_id, expand_rle(rle(temp$visiting_setter_id)$lengths))

        bbtrajqi <- mutate(left_join(bbtrajqi,
                                     dplyr::select(ungroup(temp), "point_id", "home_setter_period_on_court", "visiting_setter_period_on_court"), by = "point_id"),
                           period_on_court = case_when(.data$team %eq% datavolley::home_team(ssd$raw_data) ~ .data$home_setter_period_on_court,
                                                       .data$team %eq% datavolley::visiting_team(ssd$raw_data) ~ .data$visiting_setter_period_on_court))

        tbC <- mutate(group_by(ssd$actual, .data$team, .data$setter),
                      pts = cumsum(.data$evaluation == "Winning attack"), time = row_number())

        tbC <- mutate(left_join(tbC,
                                dplyr::select(ungroup(temp), "point_id", "home_setter_period_on_court", "visiting_setter_period_on_court"), by = "point_id"),
                      period_on_court = case_when(.data$team %eq% datavolley::home_team(ssd$raw_data) ~ .data$home_setter_period_on_court,
                                                  .data$team %eq% datavolley::visiting_team(ssd$raw_data) ~ .data$visiting_setter_period_on_court))
        dd <- left_join(tbC, dplyr::select(bbtrajqi, "team", "point_id", "time", "setter", "trajqi05", "trajqi95","trajqim"),
                        by = c("team", "point_id", "time", "setter")) %>% group_by(.data$setter, .data$team) %>%
            dplyr::summarize(pct_below = mean(.data$pts < .data$trajqi05),pct_above = mean(.data$pts > .data$trajqi95), pct_between = 1 - .data$pct_above - .data$pct_below) %>% ungroup() %>%
            dplyr::select(team, "setter", "pct_below", "pct_between","pct_above")

        attack_by_var <- ssd$attack_by_var
        setter_position_by_var <- ssd$setter_position_by_var

        attack_zones_sim <- dplyr::summarize(group_by(ssd$simulations, .data$team, .data$setter, .data$setter_position, .data$attack_choice), n_attacks = n())
        attack_zones_sim <- ungroup(mutate(attack_zones_sim, rate = .data$n_attacks / sum(.data$n_attacks)))
        attack_zones_actual <- dplyr::summarize(group_by(ssd$actual, .data$team, .data$setter, dplyr::across({{ setter_position_by_var }}), dplyr::across({{ attack_by_var }})), n_attacks = n())
        attack_zones_actual <- ungroup(mutate(attack_zones_actual, rate = .data$n_attacks / sum(.data$n_attacks)))
        setter_team <- distinct(dplyr::select(attack_zones_actual, "team", "setter"))

        attack_zones_actual <- dplyr::rename(attack_zones_actual, attack_choice = {{ attack_by_var }}, setter_position = {{ setter_position_by_var}})

        dd_att <- full_join(dplyr::rename(attack_zones_actual, setter_name = "setter"),
                            dplyr::rename(attack_zones_sim, exp_rate = "rate", ns_attacks = "n_attacks", setter_name = "setter")) %>% ## TO CHECK: needs by = c(something)
            mutate(diff_rate = .data$rate - .data$exp_rate) %>% dplyr::select(-"exp_rate",-"rate",-"ns_attacks", -"n_attacks") %>%
            pivot_wider(names_from = "attack_choice", values_from = c("diff_rate"))

        full_dd <- bind_rows(full_dd, mutate(left_join(dplyr::rename(dd, setter_name = "setter"), dd_att), ## TO CHECK: needs by = c(something)
                                             home_team = ssd$raw_data$plays$home_team[1],
                                             away_team = ssd$raw_data$plays$visiting_team[1],
                                             date = ssd$raw_data$meta$match$date))
    }
    
    # Maybe a pb with opponent here
    
    full_dd_table <- full_dd %>% ungroup() %>%
        mutate(Opponent = case_when(.data$team == .data$away_team ~ paste0("@", .data$home_team),
                                    .data$team == .data$home_team ~ .data$away_team),
               empty_space = "") %>%
        dplyr::select("date", "Opponent", "setter_position", "pct_below", "setter_position", "pct_between","pct_above", "empty_space", everything())

    if (!is.null(team_select)) full_dd_table <- full_dd_table %>% dplyr::filter(.data$team %in% team_select)

    full_dd_table <- full_dd_table %>% mutate(setter_position = as.character(.data$setter_position))
    
    bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
        bar <- htmltools::div(style = list(background = fill, width = width, height = height))
        chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
        htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    if(groupBy){
    resT <- full_dd_table %>% group_by(.data$team, .data$setter_name) %>%
        dplyr::group_map(~{
            df <- .x %>% dplyr::select(-"team", -"setter_name", -"home_team", -"away_team")
            df <- df[, colSums(is.na(df)) < nrow(df)]
            df <- df %>% unite("Opponent", date:Opponent, sep = ": ")
            
    reactable::reactable(df, 
                         groupBy = "Opponent",
                         theme = reactablefmtr::fivethirtyeight(),
              columnGroups = list(
                  reactable::colGroup(name = "Overall", columns = c("pct_below", "pct_between","pct_above"))
              ),
              defaultColDef = rating_column(
                  maxWidth = 90,
                  ##align = "center",
                  aggregate = "mean",
                  cell = function(value) {
                      value_o <- value
                      #excess <- data$[index]
                      if(!is.numeric(value) || is.na(value) || is.nan(value)) value <- 0
                      scaled <- round((value + 1) / 2, 3)
                      bgcolor <- choices_rating_color(scaled)
                      bdcolor <- "grey"
                      ##value <- format(round(value, 2)*100, nsmall = 1)
                      value <- scales::label_percent()(value)
                      if (!is.numeric(value_o) || is.na(value_o)) { ## TO CHECK: should be value NOT value_o?
                          bdcolor <- "white"
                          value <- if (is.na(value_o)) "" else value_o
                      }
                      tags$div(class = "", style = list(background=bgcolor, borderStyle = bdcolor), value)
                  },
                  format = list(aggregated = reactable::colFormat(percent = TRUE, digits = 1))
              ),
              columns = list(
                  date = reactable::colDef(maxWidth = 180, align = "left"),
                  Opponent = reactable::colDef(maxWidth = 250, align = "left",
                                               aggregate = "unique",
                                               style = list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)",
                                                            borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")),
                  setter_position = reactable::colDef(name = "Setter", aggregate = "unique", maxWidth = 100),
                  empty_space = reactable::colDef(name = ""),
                  pct_below = reactable::colDef(name = "More explorative", maxWidth = 210,align = "left",
                                                cell = function(value) {
                                                    width <- paste0(value*100, "%")
                                                    if(!is.numeric(value) || is.na(value)) value <- 0
                                                    scaled <- round(value, 3)
                                                    bgcolor <- exploration_rating_color(scaled)
                                                    bar_chart(round(value*100), width = width, fill = bgcolor, background = "#e1e1e1")
                                                }
                  ),
                  pct_between = reactable::colDef(name = "More balanced", maxWidth = 210,align = "left",
                                                  cell = function(value) {
                                                      width <- paste0(value*100, "%")
                                                      if(!is.numeric(value) || is.na(value)) value <- 0
                                                      scaled <- round(value, 3)
                                                      bgcolor <- balanced_rating_color(scaled)
                                                      bar_chart(round(value*100), width = width, fill = bgcolor, background = "#e1e1e1")
                                                  }
                  ),
                  pct_above = reactable::colDef(name = "More exploitative", maxWidth = 210,align = "left",
                                                cell = function(value) {
                                                    width <- paste0(value*100, "%")
                                                    if(!is.numeric(value) || is.na(value)) value <- 0
                                                    scaled <- round(value, 3)
                                                    bgcolor <- exploitation_rating_color(scaled)
                                                    bar_chart(round(value*100), width = width, fill = bgcolor, background = "#e1e1e1")
                                                }
                                                )
              ), defaultPageSize = nrows) %>% reactablefmtr::add_title(paste0(.x$setter_name[1], " (", .x$team[1], ")"))
        }, .keep = TRUE)
    } else {
        resT <- full_dd_table %>% group_by(.data$team, .data$setter_name) %>%
            dplyr::group_map(~{
                df <- .x %>% dplyr::select(-"team", -"setter_name", -"home_team", -"away_team", -"empty_space")
                df <- df[, colSums(is.na(df)) < nrow(df)]
                df <- df %>% unite("Opponent", date:Opponent, sep = ": ")
                df <- df %>% dplyr::select("Opponent", "pct_below", "pct_between", "pct_above", everything())
                reactable::reactable(df, 
                                     theme = reactablefmtr::fivethirtyeight(),
                                     columnGroups = list(
                                         reactable::colGroup(name = "Overall", columns = c("pct_below", "pct_between","pct_above"))
                                     ),
                                     defaultColDef = rating_column(
                                         maxWidth = 90,
                                         ##align = "center",
                                         aggregate = "mean",
                                         cell = function(value) {
                                             value_o <- value
                                             #excess <- data$[index]
                                             if(!is.numeric(value) || is.na(value) || is.nan(value)) value <- 0
                                             scaled <- round((value + 1) / 2, 3)
                                             bgcolor <- choices_rating_color(scaled)
                                             bdcolor <- "grey"
                                             ##value <- format(round(value, 2)*100, nsmall = 1)
                                             value <- scales::label_percent()(value)
                                             if (!is.numeric(value_o) || is.na(value_o)) { ## TO CHECK: should be value NOT value_o?
                                                 bdcolor <- "white"
                                                 value <- if (is.na(value_o)) "" else value_o
                                             }
                                             tags$div(class = "", style = list(background=bgcolor, borderStyle = bdcolor), value)
                                         },
                                         format = list(aggregated = reactable::colFormat(percent = TRUE, digits = 1))
                                     ),
                                     columns = list(
                                         Opponent = reactable::colDef(
                                             maxWidth = 180,
                                             align = "left",
                                             style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Opponent') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Opponent'] === prevRow['Opponent']) {
            return { visibility: 'hidden' }
          }
        }
      }")
                                         ),
                                         setter_position = reactable::colDef(name = "Setter", aggregate = "unique", maxWidth = 100, 
                                                                             style = list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)",
                                                                                          borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")),
                                         empty_space = reactable::colDef(name = ""),
                                         pct_below = reactable::colDef(name = "More explorative", maxWidth = 210,
                                                                       align = "left",
                                                                       cell = function(value) {
                                                                           width <- paste0(value*100, "%")
                                                                           if(!is.numeric(value) || is.na(value)) value <- 0
                                                                           scaled <- round(value, 3)
                                                                           bgcolor <- exploration_rating_color(scaled)
                                                                           bar_chart(round(value*100), width = width, fill = bgcolor, background = "#e1e1e1")
                                                                       },
                                                                       style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Opponent') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Opponent'] === prevRow['Opponent']) {
            return { visibility: 'hidden' }
          }
        }
      }")
                                         ),
                                         pct_between = reactable::colDef(name = "More balanced", maxWidth = 210,align = "center",
                                                                         cell = function(value) {
                                                                             width <- paste0(value*100, "%")
                                                                             if(!is.numeric(value) || is.na(value)) value <- 0
                                                                             scaled <- round(value, 3)
                                                                             bgcolor <- balanced_rating_color(scaled)
                                                                             bar_chart(round(value*100), width = width, fill = bgcolor, background = "#e1e1e1")
                                                                         },
                                                                         style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Opponent') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Opponent'] === prevRow['Opponent']) {
            return { visibility: 'hidden' }
          }
        }
      }")
                                         ),
                                         pct_above = reactable::colDef(name = "More exploitative", maxWidth = 210,align = "right",
                                                                       cell = function(value) {
                                                                           width <- paste0(value*100, "%")
                                                                           if(!is.numeric(value) || is.na(value)) value <- 0
                                                                           scaled <- round(value, 3)
                                                                           bgcolor <- exploitation_rating_color(scaled)
                                                                           bar_chart(round(value*100), width = width, fill = bgcolor, background = "#e1e1e1")
                                                                       },
                                                                       style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by school
        if (!firstSorted || firstSorted.id === 'Opponent') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Opponent'] === prevRow['Opponent']) {
            return { visibility: 'hidden' }
          }
        }
      }")
                                         )
                                     ), defaultPageSize = nrows) %>% reactablefmtr::add_title(paste0(.x$setter_name[1], " (", .x$team[1], ")"))
            }, .keep = TRUE)
    }
    resT
}
