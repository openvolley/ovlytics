context("performance indicators")

test_that("augmentation works", {
    x <- rbind(ovdata_example("190301_kats_beds", as = "parsed")$plays,
               ovdata_example("mlafin_braslovce_nkbm", as = "parsed")$plays)
    expect_identical(ov_augment_plays(x, "all"), ov_augment_plays(x, c("receiving_team", "touch_summaries", "setters", "followed", "winners", "player_role")))

    expect_setequal(setdiff(names(ov_augment_plays(x, "receiving_team")), names(x)), c("receiving_team", "receiving_team_id"))
    expect_setequal(setdiff(names(ov_augment_plays(x, "winners")), names(x)), c("set_won_by", "set_won_by_id", "match_won_by", "match_won_by_id", "team_won_set", "team_won_match", "home_sets_won", "visiting_sets_won"))
    expect_setequal(setdiff(names(ov_augment_plays(x, "touch_summaries")), names(x)), c("freeball_over", "ts_pass_quality", "ts_pass_evaluation_code", "ts_set_error"))
    expect_setequal(setdiff(names(ov_augment_plays(x, "setters")), names(x)), c("home_setter_id", "visiting_setter_id", "setter_id", "setter_position", "setter_front_back"))
    expect_setequal(setdiff(names(ov_augment_plays(x, "followed")), names(x)), c("followed_timeout", "followed_technical_timeout", "followed_sub"))
    expect_setequal(setdiff(names(ov_augment_plays(x, "player_role")), names(x)), c("player_role"))
    expect_setequal(setdiff(names(ov_augment_plays(x, "all")), names(x)), c("receiving_team", "receiving_team_id", "set_won_by", "set_won_by_id", "match_won_by", "match_won_by_id", "team_won_set", "team_won_match", "home_sets_won", "visiting_sets_won", "freeball_over", "ts_pass_quality", "ts_pass_evaluation_code", "ts_set_error", "home_setter_id", "visiting_setter_id", "setter_id", "setter_position", "setter_front_back", "followed_timeout", "followed_technical_timeout", "followed_sub", "player_role"))

    x <- ov_augment_plays(x, "all")

    expect_equal(x$match_won_by[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], "GKS Katowice")
    expect_equal(x$home_sets_won[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], 3)
    expect_equal(x$visiting_sets_won[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], 2)

    expect_equal(x$home_sets_won[which(x$match_id == "89cf04e6a5037c16fe7ab49094a91b77")][1], 3)
    expect_equal(x$visiting_sets_won[which(x$match_id == "89cf04e6a5037c16fe7ab49094a91b77")][1], 0)

    temp <- sort(table(x$player_role[which(x$skill == "Set")]), decreasing = TRUE)
    expect_equal(names(temp)[1], "setter")
    temp <- sort(table(x$player_role[which(x$skill == "Attack" & x$start_zone %in% c(4, 8))]), decreasing = TRUE)
    expect_equal(names(temp)[1], "outside")
    temp <- sort(table(x$player_role[which(x$skill == "Attack" & x$start_zone %in% c(2, 9))]), decreasing = TRUE)
    expect_equal(names(temp)[1], "opposite")
    temp <- sort(table(x$player_role[which(x$skill == "Attack" & x$start_zone == 3)]), decreasing = TRUE)
    expect_equal(names(temp)[1], "middle")

    x0 <- x
    x$setter_position <- -1L
    expect_identical(x0, ov_augment_plays(x, "all", use_existing = FALSE)) ## regenerate all cols
    expect_false(identical(x0, ov_augment_plays(x, "all", use_existing = TRUE)))
})
