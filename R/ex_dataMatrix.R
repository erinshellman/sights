#' @title High-Throughput Screening example data - CMBA
#'
#' @description An example dataset containing High-Throughput Screening (HTS) output and experimental design information. See References for details.
#'
#' @docType data
#' @usage data(ex_dataMatrix)
#'
#' @format A data frame with 80 rows and 9 columns:
#' \itemize{
#'  \item Wells. Plate well numbers for each sample
#'  \item Rows. Plate row identifiers for each sample
#'  \item Columns. Plate column identifiers for each sample
#'  \item S1_R1. Screen 1 Replicate 1
#'  \item S1_R2. Screen 1 Replicate 2
#'  \item S1_R3. Screen 1 Replicate 3
#'  \item S2_R1. Screen 2 Replicate 1
#'  \item S2_R2. Screen 2 Replicate 2
#'  \item S2_R3. Screen 2 Replicate 3}
#' This example data matrix consists of 6 plates with 80 wells each. Although these are 96-well plates, only 80 wells in each plate contained the active compounds. Therefore, the subsequent data matrix for this package excludes the inactive wells.
#'
#' @details The sights data format requires each plate matrix to be converted into a 1-dimensional vector. The plate wells in this vector should be arranged by row first. For example, this 3x3 plate matrix:
#' \tabular{rrrr}{
#' \tab Col 1 \tab Col 2 \tab Col 3 \cr
#' Row A \tab A1 \tab A2 \tab A3 \cr
#' Row B \tab B1 \tab B2 \tab B3 \cr
#' Row C \tab C1 \tab C2 \tab C3}
#' can be converted into its vector form as:
#' \tabular{rrrrrrrrrr}{
#' Row \tab Col \tab Data \cr
#' A \tab 1 \tab A1 \cr
#' A \tab 2 \tab A2 \cr
#' A \tab 3 \tab A3 \cr
#' B \tab 1 \tab B1 \cr
#' B \tab 2 \tab B2 \cr
#' B \tab 3 \tab B3 \cr
#' C \tab 1 \tab C1 \cr
#' C \tab 2 \tab C2 \cr
#' C \tab 3 \tab C3}
#' Here, number of columns in a plate is 3, and number of rows is 3 as well.
#' Each such plate vector should form a column in the data matrix before application of sights functions. Only the active wells should be included in the data matrix; inactive wells containing mock/control compounds should be marked as NAs, or if they are in entire rows/columns they can be removed completely as in this example dataset and the arguments plateRows and plateCols modified accordingly.
#'
#' @examples
#' ## load dataset
#' data(ex_dataMatrix)
#'
#' ## structure of dataset
#' str(ex_dataMatrix)
#' ## summary of dataset
#' summary(ex_dataMatrix)
#'
#' ## See help pages of SIGHTS functions for examples of using this dataset
#'
#' @references CMBA Titration series 10uM Tyr samples. \href{http://www.ncbi.nlm.nih.gov/pubmed/25190066}{Murie et al. (2015). Improving detection of rare biological events in high-throughput screens. Journal of Biomolecular Screening, 20(2), 230-241.}
#'
#' @return Dataframe of 80 rows and 9 columns as explained in Format
#'
"ex_dataMatrix"
