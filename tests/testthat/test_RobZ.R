context("testing RobZ score")

load("inglese.rda")

RobZ_fn <- data.frame(normRobZ(dataMatrix = inglese, dataCols = 3:44))
RobZ_sights <- data.frame(normSights(normMethod = "RobZ", dataMatrix = inglese, dataCols = 3:44))

# JBS results do not have Rob Z

test_that("wrapper match", {
    expect_equal(RobZ_fn, RobZ_sights)
})

test_that("message", {
    expect_message(object = normSights(normMethod = "RobZ", dataMatrix = inglese[, 3:5]), regexp = "Completed Robust Z score normalization")
}) 
