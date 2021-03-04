#' price_impact_function
#'
#' This function computes the price impact as a percentage loss in value of the
#' different bond exposures if a certain volume is sold to the market. It needs
#' to know the state variables, the data on bond volatilities and average daily
#' volumes as well as the base_year for which the stress test is taken as well
#' as the value of a constant of order unity. Finally it takes as argument a
#' method. Currently only one method is implemented, which we call
#' "square-root". This method implements equation (9) in the paper. For future
#' versions we keep space to include other impact functions which have been used
#' in the literature, in particular the functions of Cont and Schaanning 2017.
#'
#' @param del       I times 1 vector of discount factors with each component in
#'   [0,1]
#' @param mat       A list containing the matrices which are the output of
#'   \code{make_matrices}.
#' @param lb        A real number specifiying the leverage threshold lambd_bar.
#' @param data_idx  A dataframe with price index data. It must contain the
#'   variables Country (chr), Date, and Value (num)
#' @param data_adv  A dataframe with average daily volumes. It must contain the
#'   variables Country (chr), Year (num), Volume (num), Unit (chr) and Currency
#'   (chr)
#' @param base_year The base year for the stress test exercise
#' @param constant  The constant kappa in the square root formula for price
#'   impact
#' @param method    In our paper we use the square root law of price impact
#'   because we can derive it from evidence and the market-microstructure
#'   literature. But others, for example Schannning and Cont use different
#'   impact functions. We leave space for allowing for different methods by
#'   which impact is computed. For now we only implement the square root law.
#'
#' @return A list with An I times 1 vector of discount factors delta measuring
#'   the price impact of a quantity sold.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' matrices <- make_state_variables(stress_data)
#' delta <- rep(0, dim(matrices$S_0)[2])
#' price_impact_function(delta,
#' matrices, 33,
#' sovereign_bond_indices,
#' average_daily_volume_sovereign,
#' 2015,
#' 1.5,
#' method = "squareroot")
price_impact_function <- function(del, mat, lb, data_idx, data_adv, base_year, constant, method = "squareroot") {
  if (method == "squareroot") {

    delta <- matrix(del, nrow = dim(mat$S_0)[2], ncol = 1)

    # Compute the volume sold by each bank for each marketable security class. See equation (12) in our paper
    # paper.

    q <- t(mat$S_1) %*% bank_behavior_function(delta, mat, lb)


    # Compute volatility and adv for all market asset classes invoking the make_price_impact_data_function

    impact_data <- make_price_impact_data(data_idx, data_adv, base_year) %>%
      tibble::add_column(kappa = constant) %>%
      tibble::add_column(quantity = q) %>%
      dplyr::mutate(Impact = .data$Volatility * constant * sqrt(.data$quantity / .data$Volume))

    impact <- impact_data %>%
      dplyr::select(.data$Impact) %>%
      as.matrix()
  }

  return(impact)
}
