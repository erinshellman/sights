context("testing statSights")

load("inglese.rda")

norm_fn <- data.frame(normSPAWN(dataMatrix = inglese, dataCols = 33:38, plateRows = 32, plateCols = 40))
t_fn <- data.frame(statT(normMatrix = norm_fn, repIndex = c(1, 1, 1, 2, 2, 2)))
t_sights <- data.frame(statSights(statMethod = "T", normMatrix = norm_fn, repIndex = c(1, 1, 1, 2, 
    2, 2)))
rvm_fn <- data.frame(statRVM(normMatrix = norm_fn, repIndex = c(1, 1, 1, 2, 2, 2)))
rvm_sights <- data.frame(statSights(statMethod = "RVM", normMatrix = norm_fn, repIndex = c(1, 1, 1, 
    2, 2, 2)))
fdr_fn <- data.frame(statFDR(testMatrix = t_fn, ctrlMethod = "smoother"))
fdr_sights <- data.frame(statSights(statMethod = "T", normMatrix = norm_fn, repIndex = c(1, 1, 1, 2, 
    2, 2), ctrlMethod = "smoother"))

test_that("wrapper match", {
    expect_equal(t_fn, t_sights)
    expect_equal(rvm_fn, rvm_sights)
    expect_equal(fdr_fn, fdr_sights)
})

test_that("message", {
    expect_message(object = statSights(statMethod = "T", normMatrix = norm_fn, repIndex = c(1, 1, 1, 
        2, 2, 2), testSide = "greater"), regexp = "Completed t-test")
    expect_message(object = statSights(statMethod = "RVM", normMatrix = norm_fn, repIndex = c(1, 1, 
        1, 2, 2, 2), testSide = "less"), regexp = "Completed RVM test")
    expect_message(object = statFDR(testMatrix = t_fn, ctrlMethod = "bootstrap"), regexp = "Completed FDR with bootstrap estimation")
}) 
