#' @title t-test
#'
#' @description Apply one-sample t-test separately to each plate
#'
#' @inheritParams statRVM
#' @param normRows,normCols Optional integer vector. Indicate which row/column numbers from the normMatrix should be tested. If NULL then all rows/columns from the normMatrix are used.
#' @param trueMean Optional. Number indicating true value of mean. Default is 0.
#'
#' @family statistical methods
#' @include internal.R
#'
#' @details Standard one-sample t-test is applied to the normalized data.
#'
#' @return A matrix of parameters for each replicate group is returned:
#' \item{T-statistic}{Value of the t-statistic.}
#' \item{Mean_Difference}{Difference between the calculated and the true mean.}
#' \item{Standard_Error}{Standard error of the difference between means.}
#' \item{Degrees_Of_Freedom}{Degrees of freedom for the t-statistic.}
#' \item{P-value}{P-value for the t-test.}
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normSights(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' normMethod = 'normZ')
#' ## apply T test to normalized data matrix and get the p-values
#' ex_testMatrix <- statT(normMatrix = ex_normMatrix, trueMean = 0,
#' repIndex = c(1,1,1,2,2,2))
#'
#' @export
statT <- function(normMatrix, repIndex, normRows = NULL, normCols = NULL, testSide = "two.sided", trueMean = 0) {
    if (missing(repIndex) | is.null(repIndex)) {
        stop(paste("argument", " \"", "repIndex", "\" ", "is missing, with no default", sep = ""))
    }
    normMatrix <- sightsCheck(normMatrix, "norm", normRows, normCols, ri = repIndex, ts = testSide)
    
    
    outMat <- NULL
    for (i in unique(repIndex)) {
        tmpDatt <- normMatrix[, repIndex == i]
        lenn <- dim(tmpDatt)[[2]]
        colNames <- paste(i, c(":T-statistic", ":Mean_Difference", ":Standard_Error", ":Degrees_Of_Freedom", 
            ":P-value"), sep = "")
        tmpRes <- t(apply(tmpDatt, 1, myT, testSide, trueMean))
        dimnames(tmpRes)[[2]] <- colNames
        outMat <- cbind(outMat, tmpRes)
    }
    
    # dimnames(outMat)[[1]] <- dimnames(normMatrix)[[1]]
    
    if (!is.null(dimnames(normMatrix)[[1]])) {
        dimnames(outMat)[[1]] <- dimnames(normMatrix)[[1]]
    }
    
    message("Completed t-test [get p-value columns: seq(", length(unique(repIndex)), ")*5 ]")
    message("Number of samples = ", length(unique(repIndex)))
    message("Number of plates = ", ncol(normMatrix))
    message("Number of plate wells = ", nrow(normMatrix))
    return(outMat)
}
