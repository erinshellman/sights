context("testing Median Filter")

load("inglese_results.rda")
load("inglese.rda")

MedFil_fn <- data.frame(normMedFil(dataMatrix = inglese, dataCols = 3:44, plateRows = 32, plateCols = 40))
MedFil_sights <- data.frame(normSights(normMethod = "MedFil", dataMatrix = inglese, dataCols = 3:44, 
    plateRows = 32, plateCols = 40))
test_that("results match", {
    expect_equivalent(inglese_results[, 89:130], MedFil_fn)
})

test_that("wrapper match", {
    expect_equal(MedFil_fn, MedFil_sights)
})

test_that("message", {
    expect_message(object = normSights(normMethod = "MedFil", dataMatrix = inglese[, 33:35], plateRows = 32, 
        plateCols = 40, seqFilter = FALSE), regexp = "Completed Median Filter normalization")
}) 
