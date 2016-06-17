#' @title Normalization methods
#'
#' @description Apply any of the available SIGHTS normalization methods
#'
#' @param normMethod Normalization method name from SIGHTS ('Z', 'RobZ', 'R', 'SPAWN', 'Loess', or 'MedFil')
#' @param dataMatrix Data frame or numeric matrix. Columns are plates, and rows are plate wells.
#' @param plateRows,plateCols Number of rows/columns in plate. Applies to normMethods 'R', 'SPAWN', 'Loess', and 'MedFil'.
#' @param dataRows,dataCols Optional integer vector. Indicate which row/column numbers from the dataMatrix should be normalized. If NULL then all rows/columns from the dataMatrix are used.
#' @param wellCorrection Optional logical. If TRUE then individual wells are corrected based on spatial bias. Applies to normMethod 'SPAWN'.
#' @param biasMatrix Optional data frame or numeric matrix, in the same format as dataMatrix and with the same plateRows and plateCols specifications. If NULL then normalized data is used as bias template. Applies to normMethod 'SPAWN'.
#' @param biasCols Optional integer vector. Indicate which column numbers from biasMatrix or normalized dataMatrix (subset of dataCols) should be used to calculate bias template. Control plates or selection of dataMatrix plates to be used for well correction. If NULL then all plates of biasMatrix or normalized dataMatrix are used. Applies to normMethod 'SPAWN'.
#' @param seqFilter Optional logical. If TRUE apply initial row median filter then standard filter, else just apply standard filter. Applies to normMethod 'MedFil'.
#' @param trimFactor Optional trim value to be used in trimmed mean polish. It should be between 0 and 0.5. Default is 0.2. Applies to normMethod 'SPAWN'.
#'
#' @family SIGHTS functions
#'
#' @details One of the following SIGHTS normalization methods may be chosen: \code{\link{normZ}}, \code{\link{normRobZ}}, \code{\link{normR}}, \code{\link{normSPAWN}}, \code{\link{normLoess}}, or \code{\link{normMedFil}}. See their individual help pages for more details.
#'
#' @include internal.R normZ.R normRobZ.R normR.R normSPAWN.R normLoess.R normMedFil.R
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/25190066}{Murie et al. (2015). Improving detection of rare biological events in high-throughput screens. Journal of Biomolecular Screening, 20(2), 230-241.}
#'
#' @return Numeric matrix of normalized data in the same format as dataMatrix
#'
#' @note For information on how to arrange your dataset for dataMatrix, please see (\code{\link{ex_dataMatrix}})
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## choose a normalization method and provide relevant information
#' ex_normMatrix <- normSights(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' normMethod = 'RobZ')
#'
#' @export
normSights <- function(normMethod, dataMatrix, plateRows, plateCols, dataRows = NULL, dataCols = NULL, trimFactor = 0.2, 
    wellCorrection = FALSE, biasMatrix = NULL, biasCols = NULL, seqFilter = TRUE) {
    if (normMethod == "Z" | normMethod == "normZ") {
        mat <- normZ(dataMatrix, dataRows, dataCols)
    } else if (normMethod == "RobZ" | normMethod == "normRobZ") {
        mat <- normRobZ(dataMatrix, dataRows, dataCols)
    } else if (normMethod == "SPAWN" | normMethod == "normSPAWN") {
        mat <- normSPAWN(dataMatrix, plateRows, plateCols, dataRows, dataCols, trimFactor, wellCorrection, 
            biasMatrix, biasCols)
    } else if (normMethod == "R" | normMethod == "normR") {
        mat <- normR(dataMatrix, plateRows, plateCols, dataRows, dataCols)
    } else if (normMethod == "MedFil" | normMethod == "normMedFil") {
        mat <- normMedFil(dataMatrix, plateRows, plateCols, dataRows, dataCols, seqFilter)
    } else if (normMethod == "Loess" | normMethod == "normLoess") {
        mat <- normLoess(dataMatrix, plateRows, plateCols, dataRows, dataCols)
    } else {
        stop(paste(normMethod, "is not a valid method name. Note: names are case-sensitive."))
    }
    return(mat)
}
