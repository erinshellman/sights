context("testing Z")

load("inglese_results.rda")
load("inglese.rda")

Z_fn <- data.frame(normZ(dataMatrix = inglese, dataCols = 3:44))
Z_sights <- data.frame(normSights(normMethod = "Z", dataMatrix = inglese, dataCols = 3:44))
test_that("results match", {
    expect_equivalent(inglese_results[, 131:172], Z_fn)
})

test_that("wrapper match", {
    expect_equal(Z_fn, Z_sights)
})

test_that("message", {
    expect_message(object = normSights(normMethod = "Z", dataMatrix = inglese[, 33:35]), regexp = "Completed Z score normalization")
}) 
