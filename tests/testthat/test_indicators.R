context("performance indicators")

test_that("efficiencies", {
    x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
    expect_error(attack_eff(1:10), "not a character vector")
    eff1 <- attack_eff(x$plays$evaluation[x$plays$skill == "Attack"])
    expect_lt(abs(eff1 - 0.1762), 0.001)
    eff2 <- attack_eff(x$plays$evaluation, x$plays$skill)
    expect_identical(eff1, eff2)

    expect_lt(abs(serve_eff(x$plays$evaluation, x$plays$skill) - 0.2344), 0.001)

    expect_lt(abs(reception_eff(x$plays$evaluation, x$plays$skill) - 0.0818), 0.001)
})
