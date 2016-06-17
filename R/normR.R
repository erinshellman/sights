#' @title Normalization by R score method
#'
#' @description Apply Robust Regression model separately to each plate
#'
#' @inheritParams normZ
#' @param dataRows,dataCols Optional integer vector. Indicate which row/column numbers from the dataMatrix should be normalized. If NULL then all rows/columns from the dataMatrix are used.
#' @param plateRows,plateCols Number of rows/columns in plate.
#'
#' @family normalization methods
#'
#' @details R score normalization uses the robust regression method described by \href{http://www.ncbi.nlm.nih.gov/pubmed/18216390}{Wu et al (2008)}. Parameters are estimated through the \code{\link[MASS]{rlm}} function. Data is pre-normalized by median normalization prior to applying the regression algorithm. R scores are the residuals produced by the model and rescaled by dividing with the standard deviation estimate from the regression function.
#'
#' @include normRobZ.R
#'
#' @importFrom MASS rlm
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/18216390}{Wu et al. (2008). Quantitative Assessment of Hit Detection and Confirmation in Single and Duplicate High-Throughput Screenings. Journal of Biomolecular Screening, 13(2), 159-167.}
#'
#' @return Numeric matrix of normalized data in the same format as dataMatrix
#'
#' @note For information on how to arrange your dataset for dataMatrix, please see (\code{\link{ex_dataMatrix}})
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## apply R score
#' ex_normMatrix <- normR(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' plateRows = 8, plateCols = 10)
#'
#' @export
normR <- function(dataMatrix, plateRows, plateCols, dataRows = NULL, dataCols = NULL) {
    
    dataMatrix <- sightsCheck(dataMatrix, "data", dataRows, dataCols, plateRows, plateCols)
    
    rowIndex <- rep(1:plateRows, each = plateCols)
    colIndex <- rep(1:plateCols, plateRows)
    
    inData <- apply(dataMatrix, 2, medMe)
    outMat <- apply(inData, 2, rlmRConly, as.factor(rowIndex), as.factor(colIndex))
    
    outMat <- matrix(outMat, nrow = nrow(dataMatrix), ncol = ncol(dataMatrix), dimnames = dimnames(dataMatrix))
    
    message("Completed Robust Regression model normalization")
    message("Number of plates = ", ncol(dataMatrix))
    message("Number of plate wells = ", nrow(dataMatrix))
    
    return(outMat)
    
}


