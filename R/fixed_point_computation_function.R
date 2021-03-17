#' fixed_point_computation_function
#'
#' This function computes the least and the greatest fire sale equilibrium
#' according to theorem 2 in the paper to a given level of accuracy, which is
#' set by default to 10^-9. In many cases the least and the greatest fire-sale
#' equilibrium will coincide for a given set of data, but this need not
#' generally be the case.
#'
#' @param mat       A list of the initial state variable (output of
#'   \code{make_state_variables()})
#' @param lb        The critical leverage threshold called lambda_bar in the
#'   paper.
#' @param data_idx  The data-frame with the sovereign bond indices.
#' @param data_adv  The data-frame with the average daily volume figures.
#' @param base_year The base year for the simulation
#' @param constant  The value of the constant kappa in the impact fuction
#'   (equation (9)).
#' @param accuracy  The accuracy of the fixed point approximation. Set by
#'   default to 10^9
#'
#' @return A tibble with variables delta_lower (lower fixed point), iterations_lower (iterations to
#' converge to lower fixed point), delta_upper (upper fixed point), iterations_uppper( iterations to
#' converge to the upper fixed point), delta_max (maximum impact), unique (logical variable which is
#' true if fixed point is unique and false if it is not unique)
#' @export
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' state_variables <- make_state_variables(stress_data)
#' fixed_point_computation_function(
#'   mat = state_variables, lb = 33,
#'   data_idx = sovereign_bond_indices,
#'   data_adv = average_daily_volume_sovereign,
#'   base_year = 2015,
#'   constant = 1.5
#' )
fixed_point_computation_function <- function(mat,
                                             lb, data_idx, data_adv, base_year,
                                             constant, accuracy = 10^(-9)) {

  # Check Assumption 3 (paper equation (8)):

  q_max <- rowSums(t(mat$S_1))

  impact_data <- make_price_impact_data(data_idx, data_adv, base_year) %>%
    tibble::add_column(kappa = constant) %>%
    tibble::add_column(quantity = q_max) %>%
    dplyr::mutate(Impact = .data$Volatility * constant * sqrt(.data$quantity / .data$Volume))

  A3 <- impact_data %>%
    dplyr::mutate(A3 = .data$Impact >= 1) %>%
    dplyr::summarize(idx = sum(.data$A3))

  if (A3$idx > 0) {
    stop("Assumption 3 is violated. Check your data!")
  }

  delta_max <- dplyr::select(impact_data, .data$Impact) %>% as.matrix()

  # Compute the lower fixed point:

  # Initialize for approximation from below:

  delta_lower <- rep(0, dim(mat$S_0)[2]) # start value for the discount factor
  iter_lower <- 0L # initialize counter

  # check condition. If delta is not a fixed point within the given accurracy update delta and
  # state variables

  while (norm((price_impact_function(delta_lower,
    mat = mat,
    lb = lb, data_idx = data_idx,
    data_adv = data_adv, base_year = base_year,
    constant = constant
  ) - delta_lower), type = "2") >= accuracy) {
    # update delta

    delta_lower <- price_impact_function(delta_lower,
      mat = mat,
      lb = lb, data_idx = data_idx,
      data_adv = data_adv, base_year = base_year, constant = constant
    ) %>% as.numeric()

    # increase iteration counter

    iter_lower <- iter_lower + 1L
  }


  # Compute the upper fixed point:

  # Initialize for approximation from below:

  delta_upper <- delta_max # start value for the discount factor
  iter_upper <- 0L # initialize counter

  # check condition. If delta is not a fixed point within the given accurracy update delta and
  # state variables

  while (norm((price_impact_function(delta_upper,
    mat = mat,
    lb = lb, data_idx = data_idx,
    data_adv = data_adv, base_year = base_year,
    constant = constant
  ) - delta_upper), type = "2") >= accuracy) {
    # update delta

    delta_upper <- price_impact_function(delta_upper,
      mat = mat,
      lb = lb, data_idx = data_idx,
      data_adv = data_adv, base_year = base_year, constant = constant
    ) %>% as.numeric()

    # increase iteration counter

    iter_upper <- iter_upper + 1L
  }

  # Create an ouptut tibble with the results

  res <- tibble::tibble(sec_class = colnames(mat$S_0), delta_lower = delta_lower, iter_lower = iter_lower,
                        delta_upper = delta_upper, iter_upper = iter_upper) %>%
    tibble::add_column(delta_max = as.numeric(delta_max)) %>%
    dplyr::mutate(unique = assertthat::are_equal(delta_lower, delta_upper, tol = 0.01))

  return(res)
}
