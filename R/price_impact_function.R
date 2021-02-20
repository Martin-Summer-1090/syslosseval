#' price_impact_function
#'
#' @param del       I times 1 vector of discount factors with each component in $[0,1]$
#' @param mat       A list containing the matrices which are the output of make_matrices
#' @param lb        A real number specifiying the leverage threshold $bar{lambda}$
#' @param data_idx  A dataframe with price index data
#' @param data_adv  A dataframe with average daily volumes
#' @param base_year The base year for the stress test exercise
#' @param constant  The constant $kappa$ in the square root formula for price impact
#' @param method    In our paper we use the suqare root law of price impact because we can derive it
#'                  from evidence and the market-microstructure literature. But others, for example Schannning and Cont
#'                  use different impact functions. We leave space for allowing for different methods by which
#'                  impact is computed. For now we only implement the square root law.
#'
#' @return A list with An I times 1 vector of discount factors $delta$ measuring the price impact of
#'         a quantity sold and a T times 1 vector of maximum impact (the impact when all bonds held by the banks
#'         together were sold off.)
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' price_impact_function(
#'   0, example_multiple_equilibria, 44, sov_bond_index_example,
#'   adv_example, 2020, 0.9433962
#' )
price_impact_function <- function(del, mat, lb, data_idx, data_adv, base_year, constant, method = "squareroot") {
  if (method == "squareroot") {
    delta <- matrix(del, nrow = dim(mat$S_0)[2], ncol = 1)
    bank_behavior_function(delta, mat, lb)

    # Compute the volume sold by each bank for each marketable security class. See equation (12) in our paper
    # paper.

    q <- t(mat$S_1) %*% bank_behavior_function(delta, mat, lb)
    q_max <- rowSums(t(mat$S_1))

    # Compute volatility and adv for all market asset classes invoking the make_price_impact_data_function

    impact_data <- make_price_impact_data(data_idx, data_adv, base_year) %>%
      tibble::add_column(kappa = constant) %>%
      tibble::add_column(quantity = q) %>%
      tibble::add_column(total_quantity = q_max) %>%
      dplyr::mutate(Impact = .data$Volatility * constant * sqrt(.data$quantity / .data$Volume)) %>%
      dplyr::mutate(Maximum_Impact = .data$Volatility * constant * sqrt(.data$total_quantity / .data$Volume)) %>%
      dplyr::mutate(A_3 = .data$Maximum_Impact > 1)

    impact <- impact_data %>%
      dplyr::select(.data$Impact) %>%
      as.matrix()
    max_impact <- impact_data %>%
      dplyr::select(.data$Maximum_Impact) %>%
      as.matrix()
    A_3 <- impact_data %>%
      dplyr::select(.data$A_3) %>%
      as.matrix()
  }

  if (sum(A_3) > 0) {
    stop("Assumption 3 is violated !")
  }
  else {
    phi <- impact
  }

  return(phi)
}
