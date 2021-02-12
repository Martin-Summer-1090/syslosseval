#' fixed_point_computation_function_example
#'
#' @param del       A I time number of banks starting vector of discount factors
#' @param mat       A list of the initial state variable
#' @param lb        The critical leverage threshold called bar{lambda} in the paper.
#' @param accuracy The accuracy of the fixed point approximation. Set by default to 10^9
#'
#' @return A list with components equ_delta (equilibrium discount factor) and iter (number of iterations)
#' @export
#'
#' @examples fixed_point_computation_function_example(0, mat = example_multiple_equilibria, lb = 44)

fixed_point_computation_function_example <- function(del, mat, lb, accuracy = 10^(-9)){
  # Initialize values

  delta <-  del    # start value for the discount factor
  iter <- 0L       # initialize counter

  # check condition. If delta is not a fixed point within the given accurracy update delta and
  # state variables

  while( norm( (price_impact_function_example(delta,
                                      mat = mat,
                                      lb = lb) - delta), type = "2" ) >= accuracy){
    # update delta

    delta <- price_impact_function_example(delta,
                                   mat = mat,
                                   lb = lb)

    # increase iteration counter

    iter <- iter + 1L
  }

  res <- c(delta, iter)

  return(res)
}
