context("testing R")

load("inglese.rda")

R_fn <- data.frame(normR(dataMatrix = inglese, dataCols = 3:44, plateRows = 32, plateCols = 40))
R_sights <- data.frame(normSights(normMethod = "R", dataMatrix = inglese, dataCols = 3:44, plateRows = 32, 
    plateCols = 40))
# test_that('results match', { expect_equivalent(inglese_results[, 47:88], R_fn) }) not equivalent
# because JBS results were calculated replicate by replicate, whereas SIGHTS calculates R score
# plate by plate.

test_that("wrapper match", {
    expect_equal(R_fn, R_sights)
})

test_that("message", {
    expect_message(object = normSights(normMethod = "R", dataMatrix = inglese[, 33:35], plateRows = 32, 
        plateCols = 40), regexp = "Completed Robust Regression model normalization")
}) 
