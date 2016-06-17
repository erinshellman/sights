context("testing SPAWN")

load("inglese_results.rda")
load("inglese.rda")

SPAWN_fn <- data.frame(normSPAWN(dataMatrix = inglese, dataCols = 3:44, plateRows = 32, plateCols = 40, 
    trimFactor = 0.2, biasCols = 1:18, wellCorrection = TRUE))
SPAWN_sights <- data.frame(normSights(normMethod = "SPAWN", dataMatrix = inglese, dataCols = 3:44, 
    plateRows = 32, plateCols = 40, trimFactor = 0.2, biasMatrix = inglese[, 3:20], wellCorrection = TRUE))
test_that("results match", {
    expect_equivalent(inglese_results[, 5:46], SPAWN_fn)
})

test_that("wrapper match", {
    expect_equal(SPAWN_fn, SPAWN_sights)
})

test_that("message", {
    expect_message(object = normSights(normMethod = "SPAWN", dataMatrix = inglese[, 33:35], plateRows = 32, 
        plateCols = 40, trimFactor = 0.3, wellCorrection = FALSE, biasMatrix = inglese[, 3:20]), regexp = "Completed Spatial Polish normalization, trim 0.3 without well correction")
}) 
