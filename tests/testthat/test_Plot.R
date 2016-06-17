context("testing plotSights")

load("inglese.rda")

norm_fn <- data.frame(normSPAWN(dataMatrix = inglese, dataCols = 33:35, plateRows = 32, plateCols = 40))
stat_fn <- data.frame(statRVM(normMatrix = norm_fn, repIndex = c(1, 1, 1)))

box_fn <- plotBox(plotMatrix = norm_fn)
box_sights <- plotSights(plotMethod = "Box", plotMatrix = norm_fn)
autoco_fn <- plotAutoco(plotMatrix = norm_fn, plateRows = 32, plateCols = 40)
autoco_sights <- plotSights(plotMethod = "Autoco", plotMatrix = norm_fn, plateRows = 32, plateCols = 40)
ddd_fn <- plot3d(plotMatrix = norm_fn, plateRows = 32, plateCols = 40)
ddd_sights <- plotSights(plotMethod = "3d", plotMatrix = norm_fn, plateRows = 32, plateCols = 40)
heatmap_fn <- plotHeatmap(plotMatrix = norm_fn, plateRows = 32, plateCols = 40)
heatmap_sights <- plotSights(plotMethod = "Heatmap", plotMatrix = norm_fn, plateRows = 32, plateCols = 40)
ig_fn <- plotIGFit(plotMatrix = norm_fn, repIndex = c(1, 1, 1))
ig_sights <- plotSights(plotMethod = "IGFit", plotMatrix = norm_fn, repIndex = c(1, 1, 1))
scatter_fn <- plotScatter(plotMatrix = norm_fn, repIndex = c(1, 1, 1))
scatter_sights <- plotSights(plotMethod = "Scatter", plotMatrix = norm_fn, repIndex = c(1, 1, 1))
hist_fn <- plotHist(plotMatrix = stat_fn, plotCols = 5)
hist_sights <- plotSights(plotMethod = "Hist", plotMatrix = stat_fn, plotCols = 5)


test_that("wrapper match", {
    expect_equal(box_fn, box_sights)
    expect_equal(autoco_fn, autoco_sights)
    expect_equal(ddd_fn, ddd_sights)
    expect_equal(heatmap_fn, heatmap_sights)
    expect_equal(ig_fn, ig_sights)
    expect_equal(scatter_fn, scatter_sights)
    expect_equal(hist_fn, hist_sights)
})

test_that("object type", {
    expect_is(box_fn, "ggplot")
    expect_is(plotAutoco(plotMatrix = norm_fn, plateRows = 32, plateCols = 40, plotSep = FALSE), "ggplot")
    expect_is(plotHeatmap(plotMatrix = norm_fn, plateRows = 32, plateCols = 40, plotSep = FALSE), "ggplot")
    expect_is(ig_fn, "ggplot")
    expect_is(plotHist(plotMatrix = stat_fn, plotCols = 5, plotAll = TRUE, plotSep = FALSE), "ggplot")
}) 
