context("testing errors")

load("inglese.rda")

norms <- c("Z", "RobZ", "SPAWN", "MedFil", "Loess", "R")
test_that("norm", {
    expect_error(normSights(normMethod = tolower(sample(norms, 1)), dataMatrix = inglese), "not a valid method name")
    expect_error(normSights(normMethod = sample(norms[1:2], 1), dataMatrix = inglese, dataRows = 1280:1289),
        "exceed the number of plate wells")
    expect_error(normSights(normMethod = sample(norms[1:2], 1), dataMatrix = inglese, dataCols = 40:49),
        "exceed the number of plates")
})

stats <- c("T", "RVM")
test_that("stat", {
    expect_error(statSights(statMethod = tolower(sample(stats, 1)), normMatrix = inglese), "not a valid method name")
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese[, 1:3], repIndex = c(1,
        1, 1), normRows = 1280:1289), "exceed the number of plate wells")
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese[, 1:3], repIndex = c(1,
        1, 1), normCols = 40:49), "exceed the number of plates")
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese[, 1:3], repIndex = c(1,
        1, 1), testSide = "fg"), "testSide can only take one of these values: 'less', 'greater' or 'two.sided'")
})
test_that("fdr", {
    expect_error(statFDR(testMatrix = inglese), "p-values should be between 0 and 1")
    expect_error(statFDR(testMatrix = inglese[, -1]), "testMatrix should consist of 5 columns each for 1 sample, where the 5th column should contain p-values")
    expect_error(statFDR(testMatrix = matrix(sample(1:100)/100, ncol = 5), ctrlMethod = 4), "Please provide a valid method name")
    expect_error(statFDR(testMatrix = matrix(sample(1:100)/100, ncol = 5), ctrlMethod = "smo"), "is not a valid method name")
})
plots <- c("3d", "Autoco", "Box", "Heatmap", "Hist", "IGFit", "Scatter")
test_that("plot", {
    expect_error(plotSights(plotMethod = tolower(sample(plots[-1], 1)), plotMatrix = inglese), "not a valid method name")
    expect_error(plotSights(plotMethod = "Hist", plotMatrix = inglese, plotRows = 1280:1289), "exceed the number of plate wells")
    expect_error(plotSights(plotMethod = "Hist", plotMatrix = inglese, plotCols = 40:49), "exceed the number of plates")
    expect_error(plotSights(plotMethod = "Hist", plotMatrix = inglese[, 1:2], repIndex = c(1, 1), colNames = "gh"),
        "Cols is not equal to the number of sample names in the colNames")
})
test_that("missing", {
    expect_error(normSights(normMethod = sample(norms[-c(1:2)], 1), dataMatrix = inglese), "is missing")
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese), "is missing")
    expect_error(plotSights(plotMethod = sample(plots[-c(3,5)], 1), plotMatrix = inglese), "is missing")
})
test_that("plates", {
    expect_error(normSights(normMethod = sample(norms[-c(1:2)], 1), dataMatrix = inglese, dataRows = 1:79,
        plateRows = 32, plateCols = 40), "is not equal to the product of")
    expect_error(plotSights(plotMethod = sample(plots[c(1, 2, 4)], 1), plotMatrix = inglese, plotRows = 1:79,
        plateRows = 32, plateCols = 40), "is not equal to the product of")
})
test_that("spawn", {
    expect_error(normSights(normMethod = "SPAWN", dataMatrix = inglese, plateRows = 32, plateCols = 40,
        biasMatrix = inglese[1:78, ]), "dataRows is not equal to the number of plate wells in the biasMatrix")
    expect_error(normSights(normMethod = "SPAWN", dataMatrix = inglese, dataCols = 1:9, plateRows = 32,
        plateCols = 40, biasCols = 1:10), "biasCols exceed the number of plates in the dataMatrix")
    expect_error(normSights(normMethod = "SPAWN", dataMatrix = inglese, plateRows = 32, plateCols = 40,
        trimFactor = "a"), "trimFactor is not a number between 0 and 0.5")
    expect_error(normSights(normMethod = "SPAWN", dataMatrix = inglese, plateRows = 32, plateCols = 40,
        trimFactor = -1), "trimFactor is not a number between 0 and 0.5")
    expect_error(normSights(normMethod = "SPAWN", dataMatrix = inglese, plateRows = 32, plateCols = 40,
        trimFactor = 1), "trimFactor is not a number between 0 and 0.5")
})
test_that("replicate", {
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese, repIndex = c(1, 1,
        1)), "is not equal to the number of plates in the repIndex")
    expect_error(plotSights(plotMethod = sample(plots[c(3, 6, 7)], 1), plotMatrix = inglese, plateRows = 32,
        plateCols = 40, repIndex = c(1, 1, 1)), "is not equal to the number of plates in the repIndex")
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese, repIndex = 1:45),
        "Please provide at least two replicates per sample")
    expect_error(plotSights(plotMethod = sample(plots[c(3, 6, 7)], 1), plotMatrix = inglese, plateRows = 32,
        plateCols = 40, repIndex = 1:45), "Please provide at least two replicates per sample")
    expect_error(statSights(statMethod = sample(stats, 1), normMatrix = inglese, repIndex = c(1:15,
        1:20, 1:10)), "Please provide the same number of replicates per sample")
    expect_error(plotSights(plotMethod = sample(plots[c(3, 6, 7)], 1), plotMatrix = inglese, plateRows = 32,
        plateCols = 40, repIndex = c(1:15, 1:20, 1:10)), "Please provide the same number of replicates per sample")
})
