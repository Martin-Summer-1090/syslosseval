#' A toy dataset for the example to illustrate the possibility of multiple fire sale equilibria.
#'
#' This is a toy dataset for the example in Breuer, Summer, Urosevic, Appendix A3 given in a format
#' that it can be processed by the standard functions of the package which are also used for computing
#' the actual eba data
#'
#' @format A 3 x 4 tibble with the following variables
#' \describe{
#'   \item{Country}{Usually an ISO code. Here the country is called Thomas}
#'   \item{Date}{A string. We use three ficticious 2020 dates}
#'   \item{Value}{Integers of ficticcious index value which will result in a volatility of approximately 1}
#'
#' }
#' @source \url{Breuer, T. and Summer M., and Urosevic B., "Systemic Loss Evaluation", A3}
"sov_bond_index_example"
