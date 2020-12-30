context("inferring player roles and rotations")

test_that("player roles", {
    ## when meta is missing, roles should fall back to data
    x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
    rx <- ov_infer_player_roles(x, target_team = "Nova KBM Branik", method = "meta")
    rx2 <- ov_infer_player_roles(x, target_team = "Nova KBM Branik", method = "data")
    expect_identical(rx, rx2)
})
