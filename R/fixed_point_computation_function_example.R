#' fixed_point_computation_function_example
#'
#' This function computes the lower and upper fixed point for the multiple equilibrium example in
#' appendix A3.
#'
#' @param mat       A list of the initial state variable
#' @param lb        The critical leverage threshold called bar{lambda} in the paper.
#' @param accuracy The accuracy of the fixed point approximation. Set by default to 10^9
#'
#' @return A list with components equ_delta (equilibrium discount factor) and iter (number of iterations)
#' @export
#'
#' @examples
#' fixed_point_computation_function_example(mat = example_multiple_equilibria, lb = 44)
fixed_point_computation_function_example <- function(mat, lb, accuracy = 10^(-9)) {

  # Check Assumption 3 (paper equation (8)):

  q_max <- rowSums(t(mat$S_1))

  if (sqrt(0.000022 * q_max) > 1) {
    stop("Assumption 3 is violated. Check your data!")
  }

  delta_max <- sqrt(0.000022 * q_max)

  # Compute the lower fixed point:

  # Initialize for approximation from below:

  delta_upper <- delta_max
  iter_upper <- 0L # initialize counter


  # check condition. If delta is not a fixed point within the given accurracy update delta and
  # state variables

  while (norm((price_impact_function_example(delta_upper,
    mat = mat,
    lb = lb
  ) - delta_upper), type = "2") >= accuracy) {
    # update delta

    delta_upper <- price_impact_function_example(delta_upper,
      mat = mat,
      lb = lb
    )

    # increase iteration counter

    iter_upper <- iter_upper + 1L
  }




  # approximate from below

  delta_lower <- 0 # start value for the discount factor
  iter_lower <- 0L # initialize counter

  while (norm((price_impact_function_example(delta_lower,
    mat = mat,
    lb = lb
  ) - delta_lower), type = "2") >= accuracy) {
    # update delta

    delta_lower <- price_impact_function_example(delta_lower,
      mat = mat,
      lb = lb
    )

    # increase iteration counter

    iter_lower <- iter_lower + 1L
  }



  # Create an ouptut tibble with the results

  res <- tibble::tibble(delta_lower = delta_lower, iterations_lower = iter_lower,
                delta_upper = delta_upper, iterations_upper = iter_upper) %>%
    dplyr::mutate(unique = assertthat::are_equal(delta_lower, delta_upper, tol = 0.01))

  return(res)
}
