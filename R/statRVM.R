#' @title RVM Test
#'
#' @description Apply one-sample RVM t-test separately to each plate
#'
#' @param normMatrix Data frame or numeric matrix of normalized data. Columns are plates, and rows are plate wells.
#' @param normRows,normCols Optional integer vector. Indicate which row/column numbers from the normMatrix should be tested. If NULL then all rows/columns from the normMatrix are used.
#' @param testSide Optional. Type of t-test: 'two.sided', 'less', or 'greater'. Default is 'two.sided'.
#' @param repIndex Integer vector indicating replicates in normMatrix. Which plates are replicates of each other? Provide the same number for plates belonging to a replicate group. Each index in the vector matches the corresponding column of normMatrix.
#'
#' @family statistical methods
#' @include internal.R
#'
#' @details Random Variance Model one-sample t-test is applied to the normalized data. RVM assumes that the across replicate variances are distributed according to an inverse gamma distribution. This can be checked by using the \code{\link{plotIGFit}} function.
#'
#' @return A matrix of parameters for each replicate group is returned:
#' \item{RVM T-statistic}{Value of the RVM t-statistic.}
#' \item{Mean_Difference}{Difference between the calculated and the true mean.}
#' \item{Standard_Error}{Standard error of the difference between means.}
#' \item{Degrees_Of_Freedom}{Degrees of freedom for the t-statistic.}
#' \item{P-value}{P-value for the RVM test.}
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/16465162}{Malo et al. (2006). Statistical practice in high-throughput screening data analysis. Nature Biotechnology, 24(2), 167-175.}
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/14668230}{Wright & Simon (2003). A random variance model for detection of differential gene expression in small microarray experiments. Bioinformatics, 19(18), 2448-2455.}
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normSights(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' normMethod = 'normZ')
#' ## apply RVM test to normalized data matrix and get the p-values
#' ex_testMatrix <- statRVM(normMatrix = ex_normMatrix, repIndex = c(1,1,1,2,2,2))
#'
#' @export
statRVM <- function(normMatrix, repIndex, normRows = NULL, normCols = NULL, testSide = "two.sided") {
    if (missing(repIndex) | is.null(repIndex)) {
        stop(paste("argument", " \"", "repIndex", "\" ", "is missing, with no default", sep = ""))
    }
    normMatrix <- sightsCheck(normMatrix, "norm", normRows, normCols, ri = repIndex, ts = testSide)
    
    numRow <- dim(normMatrix)[[1]]
    numCol <- dim(normMatrix)[[2]]
    
    newDat <- NULL
    for (i in unique(repIndex)) {
        newDat <- rbind(newDat, normMatrix[, repIndex == i])
    }
    tmpRes <- matrix(myRVM(newDat, side = testSide), ncol = 5, byrow = FALSE)
    tmpRes <- t(apply(tmpRes, 1, checkNA))
    
    rowIndex <- seq(1, dim(tmpRes)[[1]], numRow)
    
    outMat <- NULL
    startNum <- 1
    for (i in 1:(length(unique(repIndex)))) {
        tmp <- tmpRes[startNum:(startNum + numRow - 1), ]
        colNames <- paste(i, c(":RVM T-statistic", ":Mean_Difference", ":Standard_Error", ":DegreesOfFreedom", 
            ":P-value"), sep = "")
        dimnames(tmp)[[2]] <- colNames
        outMat <- cbind(outMat, tmp)
        startNum <- startNum + numRow
    }
    
    if (!is.null(dimnames(normMatrix)[[1]])) {
        dimnames(outMat)[[1]] <- dimnames(normMatrix)[[1]]
    }
    message("Completed RVM test [get p-value columns: seq(", length(unique(repIndex)), ")*5 ]")
    message("Number of samples = ", length(unique(repIndex)))
    message("Number of plates = ", ncol(normMatrix))
    message("Number of plate wells = ", nrow(normMatrix))
    return(outMat)
}
