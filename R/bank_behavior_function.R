#' bank_behavior_function
#'
#' @param del an I x 1 vector of discount factors with each entry in $[0,1]$
#' @param mat A list with all matrices. Initially the output of make_matrices
#' @param lb a real number specifying the leverage threshold a_1/e_1 above which banks start to sell bonds.
#'
#' @return A I x 1 vector of selling shares for all marketable asset classes.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#'           state_variables <- make_state_variables(stress_data, meth = "asymmetric residuals")
#'           bank_behavior_function(0, state_variables, 33)
#'
bank_behavior_function <- function(del, mat, lb){

  # initialize the state variables

  mat_init <- mat

  # transform delta into a column vector.

  delta <- matrix(del, nrow = dim(mat_init$S_0)[2], ncol = 1)


  # create 1_J, 1_I and 1_B column vectors (see paper section2)

  ones_J <- matrix(1, nrow = dim(mat_init$L_0)[2])
  ones_I <- matrix(1, nrow = dim(mat_init$S_0)[2])
  ones_B <- matrix(1, nrow = dim(mat_init$S_0)[1])

# Compute vector \lambda_min and \lambda_max (equation (6)) for the single
# components of this vector)

  lambda_min <- (mat_init$L_1 %*% ones_J)/(mat_init$e_1 - mat_init$S_1 %*% delta)
  lambda_max <- (mat_init$S_1 %*% (ones_I - delta) + mat_init$L_1 %*% ones_J)/(mat_init$e_1 - mat_init$S_1 %*% delta)

# Compute the vector of fire sales proportions

  theta <- matrix(NA, nrow = dim(mat_init$S_0)[1], ncol = 1)
  rownames(theta) <- rownames(mat_init$S_0)
  colnames(theta) <- "theta"


  aux1 <- (lb*(mat_init$e_1 - mat_init$S_1 %*% delta) - mat_init$L_1 %*% ones_J)
  aux2 <- mat_init$S_1 %*%(ones_I - delta)

  aux <- 1 - (aux1/aux2)

  theta[(lambda_min <= lb) & (lb <= lambda_max)] <- aux[(lambda_min <= lb) & (lb <= lambda_max)]
  theta[(lb < lambda_min) | mat_init$e_1 <= 0] <- 1
  theta[(lb > lambda_max)] <- 0

# return list of state table and state variables

return(theta)

}
