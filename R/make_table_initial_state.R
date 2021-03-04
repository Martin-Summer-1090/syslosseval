#' make_table_initial_state
#'
#' This function takes the list of state values and generates a B times 4 dataframe with the
#' Bank_name, a_0, e_0, and lambda_0
#'
#' @param dat a list of state matrices. The output of \code{make_state_variables}
#'
#' @return a B times 4 dataframe with the variables Bank_name, a_0, e_0, and lambda_0
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' state_variables <- make_state_variables(stress_data)
#' make_table_initial_state(state_variables)
make_table_initial_state <- function(dat){

Bank_name <- rownames(dat$S_0)
a_0 <- (rowSums(dat$S_0) + rowSums(dat$L_0)) %>% as.numeric()
e_0 <- dat$e_0 %>% as.numeric()
lambda_0 <- (a_0/e_0) %>% as.numeric()

state_table <- tibble::tibble(Bank_name = Bank_name, a_0 = a_0, e_0 = e_0, lambda_0 = lambda_0)

return(state_table)

}
