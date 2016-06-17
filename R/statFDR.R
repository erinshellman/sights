#' @title FDR control
#'
#' @description Apply Storey's FDR control to p-values
#'
#' @param testMatrix Data frame or numeric matrix consisting of output from \code{\link{statT}} or \code{\link{statRVM}} functions. P-value columns from this matrix are automatically selected for FDR calculation. Columns are samples, and rows are plate wells.
#' @param ctrlMethod Optional. Method to use either 'smoother' or 'bootstrap' to estimate null. Default is 'smoother'.
#' @param ... Optional. Additional parameters passed to \code{\link[qvalue]{qvalue}} function.
#'
#' @family statistical methods
#' @include internal.R
#'
#' @details False Discovery Rate procedure is used to control the proportion of false positives in the results. This is an implementation of the positive false discovery (pFDR) procedure of the \code{\link[qvalue]{qvalue}} function.
#'
#' @return A matrix of parameters for each replicate group is returned:
#' \item{T-statistic or RVM T-statistic}{Value of the t-statistic.}
#' \item{Mean_Difference}{Difference between the calculated and the true mean.}
#' \item{Standard_Error}{Standard error of the difference between means.}
#' \item{Degrees_Of_Freedom}{Degrees of freedom for the t-statistic.}
#' \item{P-value}{P-value for the t-test.}
#' \item{q-value}{FDR q-value for the P-value.}
#'
#' @importFrom qvalue qvalue
#'
#' @references \href{http://onlinelibrary.wiley.com/doi/10.1111/1467-9868.00346/pdf}{Storey (2002). A direct approach to false discovery rates. Journal of the Royal Statistical Society: Series B, 64, 479-498.}
#'
#' @note Please install the package 'qvalue' from Bioconductor, if not already installed.
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normSights(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' normMethod = 'normZ')
#' ## test normalized data matrix using either the RVM or T test and store in new variable
#' ex_testMatrix <- statT(normMatrix = ex_normMatrix, trueMean = 0,
#' repIndex = c(1,1,1,2,2,2))
#' ## apply FDR control to test matrix with bootstrap control method
#' ex_ctrlMatrix <- statFDR(testMatrix = ex_testMatrix,
#' ctrlMethod = 'bootstrap')
#'
#' @export
statFDR <- function(testMatrix, ctrlMethod = "smoother", ...) {
    testMatrix <- sightsCheck(testMatrix, "fdr", cm = ctrlMethod)
    
    # myQ <- function(x) { return(qvalue::qvalue(x)$qvalues) }
    pvalInd <- seq(5, dim(testMatrix)[2], 5)
    matData <- c(testMatrix[, pvalInd])
    
    if (max(stats::na.omit(matData)) > 1 || min(stats::na.omit(matData)) < 0) 
        stop("p-values should be between 0 and 1")
    
    naInd <- is.na(matData)
    outie <- rep(NA, length(matData))
    if (sum(naInd) != length(matData)) {
        tmpRet <- qvalue::qvalue(matData[!naInd], pi0.method = ctrlMethod, ...)$qvalues
        outie[!naInd] <- tmpRet
    }
    ret <- matrix(outie, ncol = length(pvalInd), byrow = FALSE)
    outMat <- NULL
    j = 1
    for (i in seq_along(pvalInd)) {
        k = pvalInd[i]
        tmp <- cbind(testMatrix[, j:k], ret[, i])
        colnames(tmp)[6] <- gsub("P", "q", colnames(tmp)[5], ignore.case = TRUE)
        outMat <- cbind(outMat, tmp)
        j = j + k
    }
    message("Completed FDR with ", ctrlMethod, " estimation [get q-value columns: seq(", ncol(ret), ")*6 ]")
    message("Number of samples = ", ncol(ret))
    
    return(outMat)
}
