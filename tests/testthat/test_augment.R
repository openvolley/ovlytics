context("performance indicators")

test_that("augmentation works", {
    x <- rbind(ovdata_example("190301_kats_beds", as = "parsed")$plays,
               ovdata_example("mlafin_braslovce_nkbm", as = "parsed")$plays)
    expect_identical(ov_augment_plays(x, "all"), ov_augment_plays(x, c("receiving_team", "touch_summaries", "setters", "followed", "winners", "player_role")))
    x <- ov_augment_plays(x, "all")

    expect_equal(x$match_won_by[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], "GKS Katowice")
    expect_equal(x$home_sets_won[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], 3)
    expect_equal(x$visiting_sets_won[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], 2)

    expect_equal(x$home_sets_won[which(x$match_id == "8c1b4e8d534ec040829c35f31b03209b")][1], 3)
    expect_equal(x$visiting_sets_won[which(x$match_id == "8c1b4e8d534ec040829c35f31b03209b")][1], 0)

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
