context("testing Loess")

load("inglese_results.rda")
load("inglese.rda")

Loess_fn <- data.frame(normLoess(dataMatrix = inglese, dataCols = 3:44, plateRows = 32, plateCols = 40))
Loess_sights <- data.frame(normSights(normMethod = "Loess", dataMatrix = inglese, dataCols = 3:44, 
    plateRows = 32, plateCols = 40))
test_that("results match", {
    expect_equivalent(inglese_results[, 215:256], Loess_fn)
})

test_that("wrapper match", {
    expect_equal(Loess_fn, Loess_sights)
})

test_that("message", {
    expect_message(object = normSights(normMethod = "Loess", dataMatrix = inglese[, 33:35], plateRows = 32, 
        plateCols = 40), regexp = "Completed Loess normalization")
}) 
