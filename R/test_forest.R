#' Example forest planning units
#'
#' A synthetic forest landscape used in examples and tests. Each row
#' represents a polygonal planning unit with associated attributes.
#'
#' @format An sf object (data frame) with 10,000 rows and 19 variables:
#' \describe{
#'   \item{id}{Integer; unique identifier for each planning unit.}
#'   \item{group}{Integer; grouping ID (e.g., block / stratum index).}
#'   \item{ha}{Numeric; area of the planning unit in hectares.}
#'   \item{col}{Integer; column index of the unit in a regular grid.}
#'   \item{row}{Numeric; row index of the unit in a regular grid.}
#'   \item{p1}{Numeric; continuous attribute p1 (e.g., proportion / score).}
#'   \item{p2}{Numeric; continuous attribute p2 (e.g., proportion / score).}
#'   \item{p3}{Numeric; continuous attribute p3 (e.g., proportion / score).}
#'   \item{p4}{Numeric; continuous attribute p4 (e.g., proportion / score).}
#'   \item{t1}{Numeric; indicator / weight for treatment type 1.}
#'   \item{t2}{Numeric; indicator / weight for treatment type 2.}
#'   \item{b1}{Integer; binary or categorical attribute b1.}
#'   \item{b2}{Integer; binary or categorical attribute b2.}
#'   \item{c1}{Numeric; continuous covariate c1.}
#'   \item{c2}{Numeric; continuous covariate c2.}
#'   \item{c3}{Numeric; continuous covariate c3.}
#'   \item{m1}{Integer; management / class index m1.}
#'   \item{m2}{Numeric; management / class attribute m2.}
#'   \item{geometry}{sfc_POLYGON; polygon geometry of each planning unit.}
#' }
#'
#' @usage data(test_forest)
#' @docType data
#' @keywords datasets
"test_forest"