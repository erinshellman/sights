#' @title Normalization by robust Z score method
#'
#' @description Apply robust Z score to data
#'
#' @inheritParams normZ
#' @param dataRows,dataCols Optional integer vector. Indicate which row/column numbers from the dataMatrix should be normalized. If NULL then all rows/columns from the dataMatrix are used.
#'
#' @family normalization methods
#'
#' @details Robust Z score normalization subtracts the median of the raw well intensities of a given plate from the signal intensity of a given compound and divides it by the median absolute deviation of the raw well intensities of that plate.
#'
#' @return Numeric matrix of normalized data in the same format as dataMatrix
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/16465162}{Malo et al. (2006). Statistical practice in high-throughput screening data analysis. Nature Biotechnology, 24(2), 167-175.}
#'
#' @note For information on how to arrange your dataset for dataMatrix, please see (\code{\link{ex_dataMatrix}})
#'
#' @include internal.R normZ.R
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## apply robust Z score
#' ex_normMatrix <- normRobZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#'
#' @export
normRobZ <- function(dataMatrix, dataRows = NULL, dataCols = NULL) {
    
    dataMatrix <- sightsCheck(dataMatrix, "data", dataRows, dataCols)
    
    outMat <- matrix(apply(dataMatrix, 2, myRobZ), nrow = nrow(dataMatrix), ncol = ncol(dataMatrix), dimnames = dimnames(dataMatrix))
    
    message("Completed Robust Z score normalization")
    message("Number of plates = ", ncol(dataMatrix))
    message("Number of plate wells = ", nrow(dataMatrix))
    
    
    return(outMat)
}
