#' @title Auto-correlation
#'
#' @description Plot auto-correlation for each plate
#'
#' @inheritParams plot3d
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param plateRows,plateCols Number of rows/columns in plate.
#' @param plotSep Optional logical. Should plots be presented in separate windows? Default is TRUE.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{geom_path}}.
#'
#' @family graphical devices
#' @include internal.R
#'
#' @details Auto-correlation plots can be used to identify spatial bias.  Non-zero auto-correlations indicate within-plate bias, namely that proximal wells within-plates are correlated and that the measured intensity of a feature depends partially on its well location in the plate.  Cyclical patterns of auto-correlation, in particular indicate within-plate spatial bias.  Normalization methods that produce auto-correlations close to zero indicate the removal of spatial bias.
#'
#' @import ggplot2
#'
#' @return Modifiable ggplot2 object or list of objects
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## plot raw data
#' plotAutoco(plotMatrix = ex_dataMatrix, plateRows = 8, plateCols = 10,
#' plotCols = 5:10, plotName = 'Example')
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#' ## plot normalized data
#' plotAutoco(plotMatrix = ex_normMatrix, plotName = 'Example',
#' plateRows = 8, plateCols = 10, plotSep = FALSE)
#'
#' @export
plotAutoco <- function(plotMatrix, plateRows, plateCols, plotRows = NULL, plotCols = NULL, plotName = NULL, 
    plotSep = TRUE, ...) {
    
    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }
    
    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, plateRows, plateCols)
    
    rowlag <- 3 * plateRows
    collag <- 3 * plateCols
    # rowInd <- rep(1:plateRows, plateCols) rowSort <- order(rowInd)
    colInd <- rep(1:plateCols, plateRows)
    colSort <- order(colInd)
    
    
    len <- dim(plotMatrix)[[2]]
    colDat <- as.matrix(plotMatrix[colSort, ], ncol = len, byrow = FALSE)
    
    
    rowMat <- colMat <- NULL
    for (i in 1:len) {
        rowMat <- cbind(rowMat, stats::acf(colDat[, i], lag.max = rowlag, plot = FALSE, na.action = stats::na.pass)$acf[-1])
        colMat <- cbind(colMat, stats::acf(plotMatrix[, i], lag.max = collag, plot = FALSE, na.action = stats::na.pass)$acf[-1])
    }
    
    acmat <- data.frame(rbind(reshape2::melt(colMat), reshape2::melt(rowMat)))
    names(acmat)[1:2] <- c("RCInd", "Plate")
    acmat$Effects <- as.factor(rep(c("Along row", "Along column"), c(nrow(colMat) * ncol(colMat), nrow(rowMat) * 
        ncol(rowMat))))
    acmat$Plate <- as.factor(acmat$Plate)
    vline.data <- data.frame(z = c(plateCols, plateRows * 2, plateCols * 3, plateRows, plateCols * 2, plateRows * 
        3), Effects = c("Along row", "Along column"))
    # mf_labeller <- function(var, value) { value <- as.character(value) if (var == 'Effects') { value[value ==
    # 'row'] <- 'Along column' value[value == 'column'] <- 'Along row' } return(value) }
    
    if (plotSep) {
        gl <- list()
        for (i in 1:len) {
            acmati <- acmat[acmat$Plate == i, ]
            
            gl[[i]] <- ggplot2::ggplot(acmati, ggplot2::aes_string(x = "RCInd", y = "value", col = "Effects", 
                group = "Effects")) + ggplot2::geom_path(...) + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), 
                axis.text = ggplot2::element_text(size = 7)) + ggplot2::facet_grid(~Effects, scales = "free_x") + 
                ggplot2::labs(title = paste("Auto-Correlation Plot", plotName, "Plate", i), x = "Lag", y = "Auto-Correlation") + 
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + ggplot2::geom_vline(ggplot2::aes_string(xintercept = "z", 
                color = "Effects"), vline.data, linetype = "dashed", alpha = 0.5) + ggplot2::theme(legend.position = "none")
            
            
        }
        message("Number of plots = ", i)
    } else {
        acmat$Plate <- paste("Plate", acmat$Plate)
        acmat$Plate <- factor(acmat$Plate, levels = unique(acmat$Plate))
        gl <- ggplot2::ggplot(acmat, ggplot2::aes_string(x = "RCInd", y = "value", col = "Effects", group = "Effects")) + 
            ggplot2::geom_path(...) + ggplot2::facet_grid(Plate ~ Effects, scales = "free_x") + ggplot2::theme_bw() + 
            ggplot2::theme(title = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 7)) + 
            ggplot2::labs(title = paste("Auto-Correlation Plot", plotName), x = "Lag", y = "Auto-Correlation") + 
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + ggplot2::geom_vline(ggplot2::aes_string(xintercept = "z", 
            color = "Effects"), vline.data, linetype = "dashed", alpha = 0.5) + ggplot2::theme(legend.position = "none")
        message("Number of plots = 1")
    }
    message("Number of plates = ", ncol(plotMatrix))
    message("Number of plate wells = ", nrow(plotMatrix))
    return(gl)
}
