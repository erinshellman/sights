#' @title Histogram
#'
#' @description Plot histogram of p-values or q-values for each plate or all plates together
#'
#' @param plotMatrix Data frame or numeric matrix consisting only of p-values or q-values. Columns are samples, and rows are plate wells.
#' @inheritParams plotBox
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param colNames Optional. If plotAll is FALSE, names of plotCols for plot titles.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{geom_histogram}}.
#' @param plotAll Optional logical. Should all p-values or q-values be plotted together? Default is FALSE.
#' @param plotSep Optional logical. If plotAll is FALSE, should plots be presented in separate windows? Default is TRUE.
#'
#' @family graphical devices
#' @include internal.R
#'
#' @details Histograms can be used to compare actual to expected p-value distributions obtained from statistical tests of replicated features. In the presence of rare biological events, the p-value distribution should be approximately uniformly distributed with somewhat more small p-values. Deviations from these patterns indicate that the activity measurements are incorrect and/or that the statistical model is incorrectly specified.
#'
#' @import ggplot2
#'
#' @return Modifiable ggplot2 object or list of objects
#'
#' @note If using output from \code{\link{statT}}, \code{\link{statRVM}}, \code{\link{statFDR}} or \code{\link{statSights}}, please only select the plotCols corresponding to p-value and/or q-value columns, i.e., every 5th and/or 6th column in that output. Also, the x-axis label is derived from these column names indicating either 'p-values' or 'q-values'.
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#' ## apply any test to normalized data and store in new variable
#' ex_testMatrix <- statRVM(normMatrix = ex_normMatrix,
#' repIndex = c(1,1,1,2,2,2))
#' ## plot p-value data by selecting the p-value columns from test result matrix
#' plotHist(plotMatrix = ex_testMatrix, plotCols = c(5,10), plotName = 'Example',
#' colNames = c('Set_A', 'Set_B'))
#'
#' @export
plotHist <- function(plotMatrix, plotRows = NULL, plotCols = NULL, plotAll = FALSE, plotSep = TRUE, plotName = NULL, 
    colNames = NULL, ...) {
    
    
    
    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }
    
    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, cn = colNames)
    
    suppressWarnings(if (!grepl("val", colnames(plotMatrix), ignore.case = TRUE)) {
        plotValues = "values"
    } else {
        if (grepl("p", colnames(plotMatrix), ignore.case = TRUE)) {
            plotValues = "p-values"
        } else if (grepl("q", colnames(plotMatrix), ignore.case = TRUE)) {
            plotValues = "q-values"
        }
    })
    
    len <- dim(plotMatrix)[[2]]
    
    
    for (i in 1:len) {
        if (is.null(colNames)) {
            colnames(plotMatrix)[i] <- paste("Sample", i)
        } else {
            colnames(plotMatrix)[i] <- colNames[i]
        }
    }
    
    
    allMatrix <- as.data.frame(plotMatrix)
    remat <- suppressMessages(reshape2::melt(allMatrix))
    message("Number of samples = ", ncol(plotMatrix))
    if (plotAll) {
        gl <- ggplot2::ggplot(remat, ggplot2::aes_string(x = "value")) + ggplot2::geom_histogram(position = "dodge", 
            breaks = seq(0, 1, 0.05), ...) + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), 
            axis.text = ggplot2::element_text(size = 7)) + ggplot2::labs(title = paste("Histogram", plotName), 
            x = plotValues, y = "Frequency") + ggplot2::xlim(0, 1)
        message("Number of plots = 1")
    } else {
        
        if (plotSep) {
            gl <- list()
            for (i in 1:len) {
                ploda <- data.frame(plotMatrix[, i])
                names(ploda) <- "iax"
                gl[[i]] <- ggplot2::ggplot(ploda, ggplot2::aes_string(x = "iax")) + ggplot2::geom_histogram(position = "dodge", 
                  breaks = seq(0, 1, 0.05), ...) + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), 
                  axis.text = ggplot2::element_text(size = 7)) + ggplot2::labs(title = paste("Histogram", plotName, 
                  colnames(plotMatrix)[i]), x = plotValues, y = "Frequency") + ggplot2::xlim(0, 1)
            }
            message("Number of plots = ", i)
        } else {
            remat$variable <- as.factor(remat$variable)
            gl <- ggplot2::ggplot(remat, ggplot2::aes_string(x = "value")) + ggplot2::geom_histogram(position = "dodge", 
                breaks = seq(0, 1, 0.05), ...) + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), 
                axis.text = ggplot2::element_text(size = 7)) + ggplot2::labs(title = paste("Histogram", plotName), 
                x = plotValues, y = "Frequency") + ggplot2::xlim(0, 1) + ggplot2::facet_wrap(~variable)
            message("Number of plots = 1")
        }
        
    }
    
    message("Number of plate wells = ", nrow(plotMatrix))
    
    if (max(stats::na.omit(plotMatrix)) > 1 || min(stats::na.omit(plotMatrix)) < 0) 
        warning("p-values or q-values should be between 0 and 1")
    return(gl)
}
