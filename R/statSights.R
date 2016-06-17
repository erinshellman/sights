#' @title Statistical methods
#'
#' @description Apply any of the available SIGHTS statistical methods
#'
#' @param statMethod Statistical testing method to use either 'T' or 'RVM'.
#' @param ctrlMethod Optional. FDR method to use either 'smoother' or 'bootstrap' to estimate null. Default is NULL, which does not apply FDR control to the statistical testing output.
#' @param normMatrix Data frame or numeric matrix of normalized data. Columns are plates, and rows are plate wells.
#' @param normRows,normCols Optional integer vector. Indicate which row/column numbers from the normMatrix should be tested. If NULL then all rows/columns from the normMatrix are used.
#' @param testSide Optional. Type of t-test: 'two.sided', 'less', or 'greater'. Default is 'two.sided'.
#' @param repIndex Integer vector indicating replicates in normMatrix. Which plates are replicates of each other? Provide the same number for plates belonging to a replicate group. Each index in the vector matches the corresponding column of normMatrix.
#' @param trueMean Optional. Number indicating true value of mean. Applies to statMethod 'T'. Default is 0.
#' @param ... Optional. Additional parameters passed to \code{\link[qvalue]{qvalue}} function.
#'
#' @family SIGHTS functions
#'
#' @return A matrix of parameters for each replicate group including p-values and q-values, if FDR control is applied.
#'
#' @details One of the two SIGHTS statistical testing methods may be chosen: \code{\link{statT}} or \code{\link{statRVM}}, and FDR control may be applied by \code{\link{statFDR}}. See their individual help pages for more details.
#'
#' @references \href{http://www.ncbi.nlm.nih.gov/pubmed/25190066}{Murie et al. (2015). Improving detection of rare biological events in high-throughput screens. Journal of Biomolecular Screening, 20(2), 230-241.}
#'
#' @include internal.R statRVM.R statT.R statFDR.R
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## normalize data matrix using any method and store in new variable
#' ex_normMatrix <- normSights(normMethod = 'RobZ', dataMatrix = ex_dataMatrix,
#' dataCols = 5:10, wellCorrection = TRUE)
#' ## choose a statistical testing method, indicate FDR control
#' ## and provide relevant information
#' ex_statMatrix <- statSights(normMatrix = ex_normMatrix, statMethod = 'RVM',
#' ctrlMethod = 'smoother', repIndex = c(1,1,1,2,2,2))
#'
#' @export
statSights <- function(statMethod, normMatrix, repIndex, normRows = NULL, normCols = NULL, ctrlMethod = NULL, 
    testSide = "two.sided", trueMean = 0, ...) {
    if (statMethod == "T" | statMethod == "statT") {
        mat <- statT(normMatrix, repIndex, normRows, normCols, testSide, trueMean)
    } else if (statMethod == "RVM" | statMethod == "statRVM") {
        mat <- statRVM(normMatrix, repIndex, normRows, normCols, testSide)
    } else {
        stop(paste(statMethod, "is not a valid method name. Note: names are case-sensitive."))
    }
    if (is.null(ctrlMethod)) {
        return(mat)
    } else {
        fat <- statFDR(mat, ctrlMethod, ...)
        return(fat)
    }
    
}
