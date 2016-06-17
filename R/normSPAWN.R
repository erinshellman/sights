#' @title Normalization by SPAWN method
#'
#' @description Apply trimmed mean polish to data
#'
#' @inheritParams normZ
#' @param dataRows,dataCols Optional integer vector. Indicate which row/column numbers from the dataMatrix should be normalized. If NULL then all rows/columns from the dataMatrix are used.
#' @param plateRows,plateCols Number of rows/columns in plate.
#' @param trimFactor Optional trim value to be used in trimmed polish. It should be between 0 and 0.5. Default is 0.2.
#' @param wellCorrection Optional logical. If TRUE then individual wells are corrected based on spatial bias.
#' @param biasMatrix Optional data frame or numeric matrix, in the same format as dataMatrix and with the same plateRows and plateCols specifications. If NULL then normalized data is used as bias template.
#' @param biasCols Optional integer vector. Indicate which column numbers from biasMatrix or normalized dataMatrix (subset of dataCols) should be used to calculate bias template. Control plates or selection of dataMatrix plates to be used for well correction. If NULL then all plates of biasMatrix or normalized dataMatrix are used.
#'
#' @family normalization methods
#'
#' @details Spatial Polish And Well Normalization (SPAWN) uses a trimmed mean polish on individual plates to remove row and column effects. Data from each well location on each plate are initially fitted to the same model as the \href{http://jbx.sagepub.com/content/13/2/159.short}{R score}. Model parameters are estimated with an iterative polish technique but with a trimmed mean, rather than a median, as a measure of central tendency for row and column effects. The residuals are rescaled by dividing by the median average deviation of their respective plates. Well correction uses a bias template, which can either be the normalized plates themselves or be supplied externally (and SPAWN normalized before application). At each well location of this bias template, a median of all plates is calculated and subtracted from the normalized plates, thus correcting for well location bias.
#'
#' @include internal.R normR.R
#'
#' @note For information on how to arrange your dataset for dataMatrix, please see (\code{\link{ex_dataMatrix}})
#'
#' @references SPAWN: \href{http://www.ncbi.nlm.nih.gov/pubmed/25190066}{Murie et al. (2015). Improving detection of rare biological events in high-throughput screens. Journal of Biomolecular Screening, 20(2), 230-241.}
#' @references R score: \href{http://www.ncbi.nlm.nih.gov/pubmed/18216390}{Wu et al. (2008). Quantitative Assessment of Hit Detection and Confirmation in Single and Duplicate High-Throughput Screenings. Journal of Biomolecular Screening, 13(2), 159-167.}
#' @references Trimmed Mean: \href{http://www.ncbi.nlm.nih.gov/pubmed/20817887}{Malo et al. (2010). Experimental design and statistical methods for improved hit detection in high-throughput screening. Journal of Biomolecular Screening, 15(8), 990-1000.}
#'
#' @return Numeric matrix of normalized data in the same format as dataMatrix
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## apply SPAWN method with default trim factor and without well correction
#' ex_normMatrix <- normSPAWN(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' plateRows = 8, plateCols = 10, trimFactor = 0.2)
#' ## apply SPAWN method with default trim factor and with well correction
#' ex_normMatrix <- normSPAWN(dataMatrix = ex_dataMatrix, dataCols = 5:10,
#' plateRows = 8, plateCols = 10, trimFactor = 0.2, wellCorrection = TRUE)
#'
#' @export
normSPAWN <- function(dataMatrix, plateRows, plateCols, dataRows = NULL, dataCols = NULL, trimFactor = 0.2, 
    wellCorrection = FALSE, biasMatrix = NULL, biasCols = NULL) {
    
    dataMatrix <- sightsCheck(dataMatrix, "data", dataRows, dataCols, plateRows, plateCols, biasMatrix, biasCols, 
        tf = trimFactor)
    
    outMat <- spawning(dataMatrix, plateRows, plateCols, trimFactor)
    outMat <- matrix(outMat, nrow = nrow(dataMatrix), ncol = ncol(dataMatrix), dimnames = dimnames(dataMatrix))
    
    if (is.null(biasMatrix)) {
        biasMatrix <- outMat
    } else {
        biasMatrix <- spawning(biasMatrix, plateRows, plateCols, trimFactor)
    }
    
    if (is.null(biasCols)) {
        inMat <- biasMatrix
    } else {
        inMat <- biasMatrix[, biasCols]
    }
    
    
    if (wellCorrection) {
        message("Completed Spatial Polish normalization, trim ", trimFactor, " with well correction")
        message("Number of plates in bias template = ", ncol(inMat))
    } else {
        message("Completed Spatial Polish normalization, trim ", trimFactor, " without well correction")
    }
    message("Number of plates = ", ncol(dataMatrix))
    message("Number of plate wells = ", nrow(dataMatrix))
    
    
    # apply individual well correction if required
    if (wellCorrection) {
        biasMat <- apply(inMat, 1, stats::median, na.rm = TRUE)
        return(outMat - biasMat)
    } else return(outMat)
}
