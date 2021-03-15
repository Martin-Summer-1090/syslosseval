#'make_state_variables
#'
#'This function takes the output of \code{make_stress_data} as an input
#'generates all the vectors and matrices, the state variables, which are the
#'basis of our loss computations and impact assessments plus an initial fire
#'sales proportion vector with entries 0.
#'
#'@param stress_data a dataframe which is the output of \code{make_stress_data}
#'
#'
#'@return A list, with the state variables at t = 0 (observation period) and t =
#'  1 (stress horizon) \describe{
#'  \item{e_0}{B x 1 (number of banks x 1) vector of core tier 1 equity at t = 0}
#'  \item{S_0}{B x I (number of banks x number of security classes) matrix of security exposures at t = 0}
#'  \item{L_0}{B x J (number of Banks x number of loan classes) matrix of loan exposures at t = 0}
#'  \item{e_1}{B x 1 (number of banks x 1) vector of core tier 1 equity at t=1}
#'  \item{S_1}{B x I (number of banks x number of security classes) matrix of security exposures at t = 1}
#'  \item{L_1}{B x J (number of Banks x number of loan classes) matrix of loan exposures at t=1}
#'  \item{theta}{B x 1 (number of banks x 1) vector of fire sale proportions (a vector of zeros)}
#'  }
#'
#'@export
#'
#'@importFrom rlang .data
#'@importFrom magrittr %>%
#'
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' make_state_variables(stress_data)
make_state_variables <- function(stress_data) {
  e_0 <- make_e0(stress_data)
  L_0 <- make_L0(stress_data)
  S_0 <- make_S0(stress_data)

  e_1 <- make_e1(stress_data)
  L_1 <- make_L1(stress_data)
  S_1 <- make_S1(stress_data)

  # Add the zero fire sale proportions to the list

  theta <- matrix(0, nrow = dim(e_0)[1], ncol = 1)
  rownames(theta) <- rownames(e_0)
  colnames(theta) <- "theta"

  # Return a list of all matrices e_0, S_0, L_0, e_1, S_1, L_1, theta)

  matrices <- list(e_0 = e_0, S_0 = S_0, L_0 = L_0, e_1 = e_1, S_1 = S_1, L_1 = L_1, theta = theta)
}
