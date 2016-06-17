#' @title Inverse gamma
#'
#' @description Plot an inverse gamma fit plot for all plates together
#'
#' @inheritParams plotScatter
#' @param plotRows,plotCols Optional integer vector. Indicate which row/column numbers from the plotMatrix should be plotted. If NULL then all rows/columns from the plotMatrix are used.
#' @param ... Optional. Additional parameters passed to \code{\link[ggplot2]{geom_step}}.
#'
#' @family graphical devices
#'
#' @details Inverse gamma fit plot can be used to check if RVM test (\code{\link{statRVM}}) assumptions are valid and it can be applied to the data.
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#'
#' @include internal.R statRVM.R plotScatter.R
#'
#' @return Modifiable ggplot2 object
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normSights(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' normMethod = 'normZ')
#' ## plot normalized data
#' plotIGFit(plotMatrix = ex_normMatrix, repIndex = c(1,1,1,2,2,2),
#' plotName = 'Example')
#'
#' @export
plotIGFit <- function(plotMatrix, repIndex, plotRows = NULL, plotCols = NULL, plotName = NULL, ...) {
    if (missing(repIndex) | is.null(repIndex)) {
        stop(paste("argument", " \"", "repIndex", "\" ", "is missing, with no default", sep = ""))
    }
    if (is.null(plotName)) {
        plotName <- ""
    } else {
        plotName <- paste(":", plotName)
    }
    
    plotMatrix <- sightsCheck(plotMatrix, "plot", plotRows, plotCols, ri = repIndex)
    
    len <- dim(plotMatrix)[[2]]
    
    dat <- NULL
    for (i in unique(repIndex)) {
        dat <- rbind(dat, plotMatrix[, repIndex == i])
    }
    
    dat <- dat[stats::complete.cases(dat), ]
    
    degFreedom1 <- dim(dat)[[2]] - 1
    vars <- rowVars(dat, na.rm = TRUE)
    ab <- getab(vars, rep(degFreedom1, length(vars)))
    adj <- ab[1] * ab[2]
    adjVars <- vars * adj[1]
    scum <- myca(data = adjVars, q = 1)
    probF <- stats::pf(scum[, 1], degFreedom1, ab[1] * 2)
    ploda <- data.frame(scum, f = probF)
    plods <- reshape2::melt(data = ploda, id.vars = "x", variable.name = "y")
    message("Number of plots = 1")
    message("Number of plates = ", ncol(plotMatrix))
    message("Number of plate wells = ", nrow(plotMatrix))
    
    ggplot2::ggplot(data = plods, ggplot2::aes_string(x = "x", y = "value", col = "y")) + ggplot2::geom_step(...) + 
        ggplot2::theme_bw() + ggplot2::labs(title = paste("RVM Inverse Gamma Fit Plot", plotName), x = "Variable", 
        y = "Cumulative Distribution Function") + ggplot2::scale_colour_manual(values = c(p = "red", f = "blue"), 
        name = "", labels = c("Empirical", "Theoretical"), guide = "legend") + ggplot2::theme_bw() + ggplot2::theme(title = ggplot2::element_text(size = 9), 
        axis.text = ggplot2::element_text(size = 7), legend.text = ggplot2::element_text(size = 7), legend.justification = c(1, 
            0), legend.position = c(1, 0))
    
}
