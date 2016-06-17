#' @title Boxplot
#'
#' @description Construct an ordered boxplot for each plate
#'
#' @param plotMatrix Data frame or numeric matrix. Columns are plates, and rows are plate wells.
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param plotName Optional. Name of plotMatrix for plot title.
#' @param repIndex Optional. Vector of labels indicating replicate group. Each index in the vector matches the corresponding column of plotMatrix. If NULL then all plates are plotted together without grouping.
#' @param plotSep Optional logical. Should plots of different replicate groups be presented in separate windows? Default is TRUE. Does not apply if repIndex is NULL.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{geom_boxplot}}.
#'
#' @family graphical devices
#' @include internal.R
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#'
#' @return Modifiable ggplot2 object or list of objects
#'
#' @details Box plots can be used to identify scaling shifts among replicates and view the general distribution of data among all plates.
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## plot raw data
#' plotBox(plotMatrix = ex_dataMatrix, repIndex = c(1,1,1,2,2,2), plotCols = 5:10,
#' plotName = 'Example')
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#' ## plot normalized data
#' plotBox(plotMatrix = ex_normMatrix, repIndex = c(1,1,1,2,2,2), plotName = 'Example')
#'
#' @export
plotBox <- function(plotMatrix, plotRows = NULL, plotCols = NULL, plotName = NULL, repIndex = NULL, plotSep = TRUE, 
    ...) {
    
    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }
    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, ri = repIndex)
    
    len <- dim(plotMatrix)[[2]]
    
    meltMatrix <- reshape2::melt(plotMatrix)
    meltMatrix$Plate <- as.factor(rep(1:len, each = nrow(meltMatrix)/len))
    if (is.null(repIndex) == FALSE) {
        meltMatrix$Sample <- as.factor(rep(repIndex, each = dim(plotMatrix)[[1]]))
        message("Number of samples = ", length(unique(repIndex)))
        if (plotSep) {
            gl <- list()
            for (i in unique(repIndex)) {
                melti <- meltMatrix[meltMatrix$Sample == i, ]
                gl[[i]] <- ggplot2::ggplot(data = melti, ggplot2::aes_string(x = "Plate", y = "value")) + ggplot2::geom_boxplot(...) + 
                  ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 7)) + 
                  ggplot2::labs(title = paste("Boxplot", plotName, "Sample", i))
            }
            message("Number of plots = ", i)
        } else {
            meltMatrix$Sample <- as.factor(paste("Sample", rep(repIndex, each = dim(plotMatrix)[[1]])))
            gl <- ggplot2::ggplot(data = meltMatrix, ggplot2::aes_string(x = "Plate", y = "value")) + ggplot2::geom_boxplot(...) + 
                ggplot2::facet_wrap(~Sample, scales = "free_x") + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), 
                axis.text = ggplot2::element_text(size = 7)) + ggplot2::labs(title = paste("Boxplot", plotName))
            
            message("Number of plots = 1")
        }
        
    } else {
        
        gl <- ggplot2::ggplot(data = meltMatrix, ggplot2::aes_string(x = "Plate", y = "value")) + ggplot2::geom_boxplot(...) + 
            ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 7)) + 
            ggplot2::labs(title = paste("Boxplot", plotName))
        
        message("Number of plots = 1")
    }
    
    message("Number of plates = ", ncol(plotMatrix))
    message("Number of plate wells = ", nrow(plotMatrix))
    return(gl)
}
