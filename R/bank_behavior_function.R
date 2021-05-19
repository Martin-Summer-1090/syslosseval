#' bank_behavior_function
#'
#' This function computes bank-behavior according to Assumption 2 in the paper. It gives for any
#' value of the discount factor delta a B x 1 (number of banks times 1) vector of the share sold
#' of any security class in a fire sale. If a bank does not engage in a fire sale at the given
#' discount factor the component of theta belonging to this bank is 0. If the bank sells its entire
#' bond portfolio this share is 1.
#'
#' @param del an I x 1 (number of security classes times 1) vector of discount factors with each entry in [0,1]
#' @param mat A list with all matrices of state variables. The output of \code{make_state_variables}
#' @param lb a real number specifying the leverage threshold a_1/e_1 above which banks start to sell bonds.
#'
#' @return A I x 1 (number of security classes times 1) vector of selling shares in each of the
#'         marketable asset classes.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' matrices <- make_state_variables(stress_data)
#' delta <- rep(0, dim(matrices$S_0)[2])
#' bank_behavior_function(delta, matrices, 33)
bank_behavior_function <- function(del, mat, lb) {

  # transform delta into a column vector.

  delta <- matrix(del, nrow = dim(mat$S_0)[2], ncol = 1)


  # create an I x 1 column vector of one components.


  ones_I <- matrix(1, nrow = dim(mat$S_0)[2])


  # Compute vector \lambda_min and \lambda_max (equation (6)) for the single
  # components of this vector). We avoid vector multiplication where possible and
  # use rowSums() instead.

  lambda_min <- rowSums(mat$L_1) / (mat$e_1 - mat$S_1 %*% delta)
  lambda_max <- (mat$S_1 %*% (ones_I - delta) + rowSums(mat$L_1)) / (mat$e_1 - mat$S_1 %*% delta)

  # Compute the vector of fire sales proportions

  theta <- matrix(NA, nrow = dim(mat$S_0)[1], ncol = 1)
  rownames(theta) <- rownames(mat$S_0)
  colnames(theta) <- "theta"


  aux1 <- (lb * (mat$e_1 - mat$S_1 %*% delta) - rowSums(mat$L_1))
  aux2 <- mat$S_1 %*% (ones_I - delta)

  aux <- 1 - (aux1 / aux2)


  theta[(lambda_min <= lb) & (lb <= lambda_max)] <- aux[(lambda_min <= lb) & (lb <= lambda_max)]
  theta[(lb < lambda_min) | mat$e_1 <= 0] <- 1
  theta[(lb > lambda_max)] <- 0

  # return list of state table and state variables

  return(theta)
}
