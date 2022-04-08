context("performance indicators")

test_that("augmentation works", {
    x <- rbind(ovdata_example("190301_kats_beds", as = "parsed")$plays,
               ovdata_example("mlafin_braslovce_nkbm", as = "parsed")$plays)
    expect_identical(ov_augment_plays(x, "all"), ov_augment_plays(x, c("receiving_team", "touch_summaries", "setters", "followed", "winners")))
    x <- ov_augment_plays(x, "all")

    expect_equal(x$match_won_by[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], "GKS Katowice")
    expect_equal(x$home_sets_won[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], 3)
    expect_equal(x$visiting_sets_won[which(x$match_id == "9d209c4d499f8eb78e4e9992fc1c00d0")][1], 2)

    expect_equal(x$home_sets_won[which(x$match_id == "89cf04e6a5037c16fe7ab49094a91b77")][1], 3)
    expect_equal(x$visiting_sets_won[which(x$match_id == "89cf04e6a5037c16fe7ab49094a91b77")][1], 0)

    x0 <- x
    x$setter_position <- -1L
    expect_identical(x0, ov_augment_plays(x, "all", use_existing = FALSE)) ## regenerate all cols
    expect_false(identical(x0, ov_augment_plays(x, "all", use_existing = TRUE)))
})
