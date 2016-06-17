#' @title Normalization by median filter method
#'
#' @description Apply median filter normalization to data
#'
#' @param dataMatrix Data frame or numeric matrix. Columns are plates, and rows are plate wells.
#' @param dataRows,dataCols Optional integer vector. Indicate which row/column numbers from the dataMatrix should be normalized. If NULL then all rows/columns from the dataMatrix are used.
#' @param plateRows,plateCols Number of rows/columns in plate.
#' @param seqFilter Optional logical. If TRUE apply initial row median filter then standard filter, else just apply standard filter.
#'
#' @family normalization methods
#'
#' @details Median Filter normalization uses a two-step median filter process where each well is adjusted by the median score of a neighbouring group of wells [\href{http://www.ncbi.nlm.nih.gov/pubmed/21900202}{Bushway et al (2011)}]. The first median filter uses a neighbour set based on the Manhattan distance to each well. The second median filter uses a neighbour set based on the proximity along each row or column.
#'
#' @include internal.R normLoess.R
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/21900202}{Bushway et al.(2011). Optimization and application of median filter corrections to relieve diverse spatial patterns in microtiter plate data. Journal of Biomolecular Screening, 16(9), 1068-1080.}
#'
#' @return Numeric matrix of normalized data in the same format as dataMatrix
#'
#' @note For information on how to arrange your dataset for dataMatrix, please see (\code{\link{ex_dataMatrix}})
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## apply standard median filter method
#' ex_normMatrix <- normMedFil(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' plateRows = 8, plateCols = 10, seqFilter = FALSE)
#' ## apply initial row median filter then standard filter
#' ex_normMatrix <- normMedFil(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' plateRows = 8, plateCols = 10, seqFilter = TRUE)
#'
#' @export
normMedFil <- function(dataMatrix, plateRows, plateCols, dataRows = NULL, dataCols = NULL, seqFilter = TRUE) {
    
    dataMatrix <- sightsCheck(dataMatrix, "data", dataRows, dataCols, plateRows, plateCols)
    
    # Get neighbours for large plate (windows of 3)
    if (nrow(dataMatrix) > 384) {
        w = ww = 3
        # get neighbours for small and medium plates (windows of 2)
    } else if (nrow(dataMatrix) <= 384 & nrow(dataMatrix) > 96) {
        w = ww = 2
        # get neighbours for small and medium plates (windows of 1)
    } else {
        w = 1
        ww = 2
        
    }
    buddies <- createNeighbourhoodMatrix(nrows = plateRows, ncols = plateCols, wind = w)
    rowBuddies <- createSequentialNeighbourhoodMatrix(nrows = plateRows, ncols = plateCols, wind = ww)
    
    mat <- dataMatrix
    if (seqFilter) {
        mat <- apply(mat, 2, applyMedFilter, rowBuddies)
    }
    outMat <- apply(mat, 2, applyMedFilter, buddies)
    outMat <- matrix(apply(outMat, 2, myRobZ), nrow = nrow(dataMatrix), ncol = ncol(dataMatrix), dimnames = dimnames(dataMatrix))
    if (seqFilter) {
        message("Completed Median Filter normalization with initial row median filter")
    } else {
        message("Completed Median Filter normalization")
    }
    message("Number of plates = ", ncol(dataMatrix))
    message("Number of plate wells = ", nrow(dataMatrix))
    return(outMat)
}
