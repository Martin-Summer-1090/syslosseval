#' make_table_stress_state
#'
#' This function takes the list of state values and generates a B times 4 dataframe with the
#' Bank_name, a_1, e_1, and lambda_1
#'
#' @param dat a list of state matrices. The output of \code{make_state_variables}
#'
#' @return a B times 4 dataframe with the variables Bank_name, a_1, e_1, and lambda_1
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' state_variables <- make_state_variables(stress_data)
#' make_table_stress_state(state_variables)
make_table_stress_state <- function(dat){

  Bank_name <- rownames(dat$S_1)
  a_1 <- (rowSums(dat$S_1) + rowSums(dat$L_1))
  e_1 <- dat$e_1
  lambda_1 <- (a_1/e_1)

  state_table <- tibble::tibble(Bank_name = Bank_name, a_1 = a_1, e_1 = e_1, lambda_1 = lambda_1)

  return(state_table)

}
