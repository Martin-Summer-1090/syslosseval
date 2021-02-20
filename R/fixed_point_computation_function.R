#' fixed_point_computation_function
#'
#' @param del       A I time number of banks starting vector of discount factors
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
#' fixed_point_computation_function(0,
#'   mat = example_multiple_equilibria, lb = 44,
#'   data_idx = sov_bond_index_example, data_adv = adv_example, base_year = 2020, constant = 0.9433962
#' )
fixed_point_computation_function <- function(del, mat,
                                             lb, data_idx, data_adv, base_year,
                                             constant, accuracy = 10^(-9)) {
  # Initialize values

  delta <- del # start value for the discount factor
  iter <- 0L # initialize counter

  # check condition. If delta is not a fixed point within the given accurracy update delta and
  # state variables

  while (norm((price_impact_function(delta,
    mat = mat,
    lb = lb, data_idx = data_idx,
    data_adv = data_adv, base_year = base_year,
    constant = constant
  ) - delta), type = "2") >= accuracy) {
    # update delta

    delta <- price_impact_function(delta,
      mat = mat,
      lb = lb, data_idx = data_idx,
      data_adv = data_adv, base_year = base_year, constant = constant
    )

    # increase iteration counter

    iter <- iter + 1L
  }

  res <- c(delta, iter)

  return(res)
}
