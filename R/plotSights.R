#' @title Graphical devices
#'
#' @description Apply any of the available SIGHTS graphical devices
#'
#' @param plotMethod Plotting method name from SIGHTS ('3d', 'Autoco', 'Box', 'Heatmap', 'Hist', 'IGFit', or 'Scatter').
#' @param plotMatrix Data frame or numeric matrix. Columns are plates, and rows are plate wells. For plotMethod 'Hist', this is a p-value matrix with each column a single sample.
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param plotName Optional. Name of plotMatrix for plot title.
#' @param plateRows,plateCols Number of rows/columns in plate. Applies to plotMethods '3d', 'Autoco' and 'Heatmap'.
#' @param plotAll Optional logical. Should all p-values be plotted together? Default is FALSE. Applies to plotMethod 'Hist'.
#' @param plotSep Optional logical. Should plots be presented in separate windows? Default is TRUE. Applies to plotMethods 'Autoco', 'Box', 'Hist' and 'Heatmap'. For 'Box', each replicate group is presented in a separate window and it only applies if repIndex is not NULL.
#' @param colNames Optional. Names of plotCols for plot title. Applies to plotMethod 'Hist'.
#' @param repIndex Vector of labels indicating replicate group. Each index in the vector matches the corresponding column of plotMatrix. Applies to plotMethods 'Box',  'Scatter' and 'IGFit'.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{ggplot}} functions.
#'
#' @family SIGHTS functions
#'
#' @details One of the following SIGHTS graphical devices may be chosen: \code{\link{plot3d}}, \code{\link{plotAutoco}}, \code{\link{plotBox}}, \code{\link{plotHeatmap}}, \code{\link{plotHist}}, \code{\link{plotIGFit}}, or \code{\link{plotScatter}}. See their individual help pages for more details.
#'
#' @include internal.R plot3d.R plotAutoco.R plotBox.R plotHeatmap.R plotHist.R plotIGFit.R plotScatter.R
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/25190066}{Murie et al. (2015). Improving detection of rare biological events in high-throughput screens. Journal of Biomolecular Screening, 20(2), 230-241.}
#'
#' @return List of lattice objects for 'plot3d'. Modifiable ggplot2 object or list of objects for all others.
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normSights(normMethod = 'RobZ', dataMatrix = ex_dataMatrix,
#' dataCols = 5:10, wellCorrection = TRUE)
#' ## choose a graphical device and provide relevant information
#' plotSights(plotMethod = 'Autoco', plotMatrix = ex_normMatrix,
#' plotName = 'Example', plateRows = 8, plateCols = 10)
#'
#' @export
plotSights <- function(plotMethod, plotMatrix, plateRows, plateCols, repIndex = NULL, plotRows = NULL, plotCols = NULL, 
    plotName = NULL, plotSep = TRUE, plotAll = FALSE, colNames = NULL, ...) {
    if (plotMethod == "3d" | plotMethod == "plot3d") {
        pl <- plot3d(plotMatrix, plateRows, plateCols, plotRows, plotCols, plotName)
    } else if (plotMethod == "Autoco" | plotMethod == "plotAutoco") {
        pl <- plotAutoco(plotMatrix, plateRows, plateCols, plotRows, plotCols, plotName, plotSep, ...)
    } else if (plotMethod == "Box" | plotMethod == "plotBox") {
        pl <- plotBox(plotMatrix, plotRows, plotCols, plotName, repIndex, plotSep, ...)
    } else if (plotMethod == "Heatmap" | plotMethod == "plotHeatmap") {
        pl <- plotHeatmap(plotMatrix, plateRows, plateCols, plotRows, plotCols, plotName, plotSep, ...)
    } else if (plotMethod == "Hist" | plotMethod == "plotHist") {
        pl <- plotHist(plotMatrix, plotRows, plotCols, plotAll, plotSep, plotName, colNames, ...)
    } else if (plotMethod == "IGFit" | plotMethod == "plotIGFit") {
        pl <- plotIGFit(plotMatrix, repIndex, plotRows, plotCols, plotName, ...)
    } else if (plotMethod == "Scatter" | plotMethod == "plotScatter") {
        pl <- plotScatter(plotMatrix, repIndex, plotRows, plotCols, plotName, ...)
    } else {
        stop(paste(plotMethod, "is not a valid method name. Note: names are case-sensitive."))
    }
    return(pl)
}
