context("attack evenness")
test_that("attack evenness behaves as expected", {
    px <- plays(dv_read(ovdata_example()))

    ## general error checks
    expect_error(ov_aev(px, team = "GKS Katowice"), "argument \"rotation\" is missing, with no default")
    expect_error(ov_aev(px, rotation = "player_role", team = "GKS Katowice"), "there is no 'player_role' column in x")
    expect_error(ov_aev(px, team = "GKS KatowiceXYZ", rotation = "SHM"), "does not appear in x")

    ## single team
    this <- ov_aev(px, team = "GKS Katowice", rotation = "SHM")
    expect_equal(nrow(this), 1)
    expect_equal(this$N_attacks, 99)

    ## all teams
    this2 <- ov_aev(px, rotation = "SHM")
    expect_equal(nrow(this2), 2)
    expect_equal(this, this2[1, ])

    ## all teams, with extra detail
    this2 <- ov_aev(px, rotation = "SHM", detail = TRUE)
    expect_true(all(c("team", "aev_sd", "aev", "rally_win_rate", "kill_rate", "rec_eff", "N_attacks", "N_rallies") %in% names(this2)))

    ## for a single team, calculated by set number but aggregate results when reporting
    this3 <- ov_aev(px, team = "GKS Katowice", rotation = "SHM", calculate_by = "set_number")
    expect_equal(nrow(this3), 1)
    expect_equal(this3$N_attacks, this$N_attacks)

    ## for a single team, calculated and reported by set number
    this3 <- ov_aev(px, team = "GKS Katowice", rotation = "SHM", calculate_by = "set_number", report_by = "set_number")
    expect_equal(nrow(this3), 5)
    expect_equal(sum(this3$N_attacks), this$N_attacks)
})
