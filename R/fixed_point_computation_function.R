#' fixed_point_computation_function
#'
#' @param mat       A list of the initial state variable
#' @param lb        The critical leverage threshold called bar{lambda} in the paper.
#' @param data_idx  The data-frame with the sovereign bond indices.
#' @param data_adv  The data-frame with the average daily volume figures.
#' @param base_year The base year for the simulation
#' @param constant  The value of the constant kappa in the impact fuction.
#' @param accuracy The accuracy of the fixed point approximation. Set by default to 10^9
#'
#' @return A list with components equ_delta (equilibrium discount factor) and iter (number of iterations)
#' @export
#'
#' @examples
#' fixed_point_computation_function(mat = example_multiple_equilibria, lb = 44,
#'   data_idx = sov_bond_index_example, data_adv = adv_example, base_year = 2020, constant = 0.9433962
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

  if(A3$idx > 0){stop("Assumption 3 is violated. Check your data!")}

  delta_max <- impact_data$Impact %>% as.matrix()

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
    )

    # increase iteration counter

    iter_lower <- iter_lower + 1L
  }

  res_lower <- c(delta_lower, iter_lower)
  names(res_lower) <- c("d_lower", "iterations")


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
    )

    # increase iteration counter

    iter_upper <- iter_upper + 1L
  }

  res_upper <- c(delta_upper, iter_upper)
  names(res_upper) <- c("d_upper", "iterations")

  res <- c(res_lower, res_upper)

  return(res)


}
