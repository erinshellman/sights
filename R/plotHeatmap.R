#' @title Heat map
#'
#' @description Plot heat map for each plate
#'
#' @inheritParams plotAutoco
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param plateRows,plateCols Number of rows/columns in plate.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{geom_tile}}.
#'
#' @details Heat maps can be used to assess the existence of spatial bias on a plate by plate basis.  Spatial bias can be visually subtle, however, and sometimes difficult to detect with heat maps. Auto-correlation plots (\code{\link{plotAutoco}}) can circumvent this problem.
#'
#' @family graphical devices
#' @include internal.R
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#'
#' @return Modifiable ggplot2 object or list of objects
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## plot raw data with graphs separated
#' plotHeatmap(plotMatrix = ex_dataMatrix, plotCols = 5:10,
#' plotName = 'Example', plateRows = 8, plateCols = 10)
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#' ## plot normalized data with graphs together
#' plotHeatmap(plotMatrix = ex_normMatrix, plotName = 'Example',
#' plateRows = 8, plateCols = 10, plotSep = FALSE)
#'
#' @export
plotHeatmap <- function(plotMatrix, plateRows, plateCols, plotRows = NULL, plotCols = NULL, plotName = NULL,
    plotSep = TRUE, ...) {

    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }

    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, plateRows, plateCols)

    len <- dim(plotMatrix)[[2]]


    revInd <- plateRows:1
    tmp <- matrix(plotMatrix, nrow = plateRows, byrow = FALSE)
    tmp1 <- tmp[revInd, ]
    heatmat <- data.frame(matrix(tmp1, nrow = plateRows, byrow = FALSE))
    heatmelt <- suppressMessages(reshape2::melt(heatmat))
    heatmelt$Rows <- as.factor(rep(1:plateRows))
    heatmelt$Columns <- as.factor(rep(1:plateCols, each = plateRows))
    heatmelt$Plate <- as.factor(rep(1:len, each = plateRows * plateCols))

    if (plateCols > plateRows) {
        lpos = "bottom"
    } else {
        lpos = "right"
    }

    if (plotSep) {
        gl <- list()
        for (i in 1:len) {
            heatmelti <- heatmelt[heatmelt$Plate == i, ]
            heatmelti <- heatmelti[stats::complete.cases(heatmelti),]
            bk <- pretty(heatmelti$value, nchar(min(heatmelti$value)))
            gl[[i]] <- ggplot2::ggplot(heatmelti) + ggplot2::geom_tile(ggplot2::aes_string(y = "Rows", x = "Columns",
                fill = "value"), ...) + ggplot2::labs(title = paste("Heatmap", plotName, "Plate", i)) + ggplot2::theme_bw() +
                ggplot2::theme(legend.position = lpos, title = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 7),
                  axis.text.x = ggplot2::element_text(angle = 270, vjust = 0.2), legend.text = ggplot2::element_text(size = 7)) +
                ggplot2::scale_fill_gradient(low = "slategray1", high = "steelblue4", breaks = bk[c(2, length(bk) -
                  1)])

        }
        message("Number of plots = ", i)
    } else {
        heatmelt$Plate <- paste("Plate", heatmelt$Plate)
        heatmelt$Plate <- factor(heatmelt$Plate, levels = unique(heatmelt$Plate))
        heatmelt <- heatmelt[stats::complete.cases(heatmelt),]
        bk <- pretty(heatmelt$value, nchar(min(heatmelt$value)))
        gl <- ggplot2::ggplot(heatmelt) + ggplot2::geom_tile(ggplot2::aes_string(y = "Rows", x = "Columns",
            fill = "value"), ...) + ggplot2::labs(title = paste("Heatmap", plotName)) + ggplot2::theme_bw() +
            ggplot2::theme(title = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 7),
                axis.text.x = ggplot2::element_text(angle = 270, vjust = 0.2), legend.text = ggplot2::element_text(size = 7)) +
            ggplot2::facet_wrap(~Plate) + ggplot2::scale_fill_gradient(low = "slategray1", high = "steelblue4",
            breaks = bk[c(2, length(bk) - 1)])
        message("Number of plots = 1")
    }
    message("Number of plates = ", ncol(plotMatrix))
    message("Number of plate wells = ", nrow(plotMatrix))
    return(gl)
}
