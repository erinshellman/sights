#' @title Scatter plot
#'
#' @description Construct a scatter plot of all pairwise combinations of replicates
#'
#' @inheritParams plotBox
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{geom_point}}.
#'
#' @family graphical devices
#' @include internal.R
#'
#' @details Scatter plots with robust regression lines of replicate plates can reveal a kind of bias which acts independently of within-plate biases and which cannot be detected by heat maps (\code{\link{plotHeatmap}}) or auto-correlation plots (\code{\link{plotAutoco}}). A mixture of active and inactive features should produce a zero-correlation flat regression line within most of the range and a positively sloped line within the active range(s) at the extreme(s) of the distribution.
#'
#' @import ggplot2
# @importFrom epiR epi.ccc
#'
#' @return List of modifiable ggplot2 objects
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## plot raw data
#' plotScatter(plotMatrix = ex_dataMatrix, repIndex = c(1,1,1), plotCols = 5:7,
#' plotName = 'Example')
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#' ## plot normalized data
#' plotScatter(plotMatrix = ex_normMatrix, repIndex = c(1,1,1), plotCols = 1:3,
#' plotName = 'Example')
#'
#' @export
plotScatter <- function(plotMatrix, repIndex, plotRows = NULL, plotCols = NULL, plotName = NULL, ...) {
    if (missing(repIndex) | is.null(repIndex)) {
        stop(paste("argument", " \"", "repIndex", "\" ", "is missing, with no default", sep = ""))
    }
    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }
    
    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, ri = repIndex)
    
    le <- dim(plotMatrix)[[2]]
    we <- dim(plotMatrix)[[1]]
    
    dat <- NULL
    names(plotMatrix) = NULL
    for (i in unique(repIndex)) {
        dat <- rbind(dat, as.matrix(plotMatrix[, repIndex == i]))
    }
    dat <- data.frame(dat)
    cn = utils::combn(1:ncol(dat), 2)
    n = dim(cn)[2]
    dat$sample_index <- rep(unique(repIndex), each = we)
    message("Number of samples = ", length(unique(repIndex)))
    m <- matrix(1:le, nrow = max(repIndex), byrow = TRUE)
    gl <- list()
    lg = 1
    for (l in unique(repIndex)) {
        for (k in 1:n) {
            i = cn[1, k]
            j = cn[2, k]
            ploda <- data.frame(dat[dat$sample_index == l, c(i, j)])
            names(ploda) <- c("xax", "yax")
            pear <- stats::cor(x = ploda$xax, y = ploda$yax, use = "complete.obs", method = "pearson")
            # ccc <- unlist(epiR::epi.ccc(x = ploda$xax, y = ploda$yax)[[1]][1])
            ploxmin <- min(ploda[stats::complete.cases(ploda$xax), 1])
            ploxmax <- max(ploda[stats::complete.cases(ploda$xax), 1])
            ploymin <- min(ploda[stats::complete.cases(ploda$yax), 2])
            ploymax <- max(ploda[stats::complete.cases(ploda$yax), 2])
            # ploxm <- mean(ploda[complete.cases(ploda$xax),1]) ploym <- mean(ploda[complete.cases(ploda$yax),2])
            gl[[lg]] <- ggplot2::ggplot(ploda, ggplot2::aes_string(x = "xax", y = "yax")) + ggplot2::geom_point(...) + 
                ggplot2::geom_smooth(method = "loess", color = "blue", alpha = 0.2) + ggplot2::labs(title = paste("Scatter Plot ", 
                plotName, " Plates ", m[l, i], "-", m[l, j], sep = ""), x = paste("Replicate", i), y = paste("Replicate", 
                j)) + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), axis.text = ggplot2::element_text(size = 7)) + 
                ggplot2::coord_fixed(ratio = 1) + ggplot2::lims(x = c(min(ploxmin, ploymin), max(ploxmax, ploymax)), 
                y = c(min(ploxmin, ploymin), max(ploxmax, ploymax))) + ggplot2::geom_abline(linetype = 2, intercept = 0, 
                slope = 1)
            # + ggplot2::annotate(geom = 'label', x = Inf, y = ploymin, hjust = 1, vjust = 1, alpha = 0.2, label =
            # paste('r =', round(pear, 3), 'ccc =', round(ccc, 3)))
            lg = lg + 1
        }
        
    }
    message("Number of plots = ", lg - 1)
    message("Number of plates = ", ncol(plotMatrix))
    message("Number of plate wells = ", nrow(plotMatrix))
    # return(c(paste('Pearson's correlation coefficient =', pear), paste('Concordance correlation coefficient
    # =', ccc)))
    return(gl)
}
