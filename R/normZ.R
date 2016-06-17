#' @title Normalization by Z score method
#'
#' @description Apply Z score to data
#'
#' @param dataMatrix Data frame or numeric matrix. Columns are plates, and rows are plate wells.
#' @param dataRows,dataCols Optional integer vector. Indicate which row/column numbers from the dataMatrix should be normalized. If NULL then all rows/columns from the dataMatrix are used.
#'
#' @return Numeric matrix of normalized data in the same format as dataMatrix
#'
#' @family normalization methods
#' @include internal.R
#'
#' @details Z score normalization subtracts the mean of the raw well intensities of a given plate from the signal intensity of a given compound and divides it by the standard deviation of the raw well intensities of that plate.
#'
#' @note For information on how to arrange your dataset for dataMatrix, please see (\code{\link{ex_dataMatrix}})
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## apply Z score
#' ex_normMatrix <- normZ(dataMatrix = ex_dataMatrix, dataCols = 5:10)
#'
#' @export
normZ <- function(dataMatrix, dataRows = NULL, dataCols = NULL) {
    
    dataMatrix <- sightsCheck(dataMatrix, "data", dataRows, dataCols)
    
    outMat <- matrix(apply(dataMatrix, 2, myZ), nrow = nrow(dataMatrix), ncol = ncol(dataMatrix), dimnames = dimnames(dataMatrix))
    
    message("Completed Z score normalization")
    message("Number of plates = ", ncol(dataMatrix))
    message("Number of plate wells = ", nrow(dataMatrix))
    
    
    return(outMat)
    
}

