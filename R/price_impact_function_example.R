#' price_impact_function_example
#'
#' @param del       I times 1 vector of discount factors with each component in $[0,1]$
#' @param mat       A list containing the matrices which are the output of make_matrices
#' @param lb        A real number specifiying the leverage threshold $bar{lambda}$
#'
#' @return A list containing the fixed point and the number of iterations needed to find it
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples price_impact_function_example(0, example_multiple_equilibria, 44)
price_impact_function_example <- function(del, mat, lb){


    delta <- matrix(del, nrow = dim(mat$S_0)[2], ncol = 1)

    # Compute the volume sold by each bank for each marketable security class. See equation (12) in our paper
    # paper.

    q <- t(mat$S_1) %*% bank_behavior_function(delta, mat, lb)
    q_max <- rowSums(t(mat$S_1))

    # Compute volatility and adv for all market asset classes invoking the make_price_impact_data_function

    impact <- sqrt(0.000022*q)

  return(impact)

}
