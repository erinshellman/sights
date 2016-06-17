#' @title 3D plot
#'
#' @description Plot a three-dimensional plot for each plate
#'
#' @inheritParams plotBox
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param plateRows,plateCols Number of rows/columns in plate.
#'
#' @family graphical devices
#' @include internal.R
#'
#' @importFrom lattice wireframe
#'
#' @return List of lattice objects
#'
#' @details 3d plots can be used to assess the existence of spatial bias on a plate  by plate basis. Spatial bias can be visually  subtle, however, and sometimes difficult to detect with 3d plots. Auto-correlation  plots (\code{\link{plotAutoco}}) can circumvent this problem.
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## plot raw data
#' plot3d(plotMatrix = ex_dataMatrix, plotCols = 5:10,
#' plotName = 'Example', plateRows = 8, plateCols = 10)
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#' ## plot normalized data
#' plot3d(plotMatrix = ex_normMatrix, plotName = 'Example',
#' plateRows = 8, plateCols = 10)
#'
#' @export
plot3d <- function(plotMatrix, plateRows, plateCols, plotRows = NULL, plotCols = NULL, plotName = NULL) {
    
    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }
    
    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, plateRows, plateCols)
    len <- dim(plotMatrix)[[2]]
    sclist <- list()
    for (i in 1:len) {
        sclist[[i]] <- matrix(plotMatrix[, i], nrow = plateRows, ncol = plateCols)
    }
    lims <- sort(unlist(sclist))
    length(lims)
    gl <- list()
    for (i in 1:len) {
        score <- sclist[[i]]
        gl[[i]] <- lattice::wireframe(score, drape = TRUE, col.regions = grDevices::heat.colors(length(lims)), 
            zlab = list("score", rot = 90), xlab = list("row", rot = 45), ylab = list("column", rot = 315), 
            main = list(label = paste("3D Plot", plotName, "Plate", i), font = 1), par.settings = list(axis.line = list(col = 0), 
                fontsize = list(text = 10)))
        
        
    }
    message("Number of plots = ", i)
    message("Number of plates = ", ncol(plotMatrix))
    message("Number of plate wells = ", nrow(plotMatrix))
    return(gl)
}
