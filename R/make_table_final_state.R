#' make_table_final_state
#'
#' This function takes the list of state values and the output of the
#' fixed point computation and generates a B times 4 dataframe with the
#' Bank_name, a_1_tau, e_1_tau, and lambda_1_tau
#'
#' @param dat a list of state matrices. The output of \code{make_state_variables}
#' @param fp a dataframe. The output of \code{fixed_point_computation_function}
#' @param lb a real number specifiying the leverage threshold. Same threshold as in
#'        \code{fixed_point_computation_function}
#'
#' @return a B times 4 dataframe with the variables Bank_name, a_1_tau, e_1_tau,
#' and lambda_1_tau if the fixed point is unique a B times 7 dataframe with the variables
#' Bank_name, a_1_tau_lower, a_1_tau_upper, e_1_tau_lower, e_1_tau_lower and lambda_1_tau_lower,
#' lambda_1_tau_upper
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' state_variables <- make_state_variables(stress_data)
#' fixed_points <- fixed_point_computation_function(
#'   mat = state_variables, lb = 33,
#'   data_idx = sovereign_bond_indices,
#'   data_adv = average_daily_volume_sovereign,
#'   base_year = 2015,
#'   constant = 1.5
#' )
#' make_table_final_state(state_variables, fixed_points, 33)
make_table_final_state <- function(dat,fp, lb){

# check whether fixed point is unique and select the unique or else the upper and lower fixed point

unique <- dplyr::select(fp, unique) %>% unlist()

if(all(unique) > 0){

del <- dplyr::select(fp, .data$delta_lower) %>% as.matrix()
theta <- bank_behavior_function(del = del, mat = dat, lb = lb) %>% as.matrix()

Bank_name <- rownames(dat$S_1)
a_1_tau <- ( (rowSums(dat$S_1) - dat$S_1%*%del)*(1-theta) + rowSums(dat$L_1)) %>% as.numeric()
e_1_tau <- (dat$e_1 - dat$S_1%*%del) %>% as.numeric()
lambda_1_tau <- (a_1_tau/e_1_tau) %>% as.numeric()

state_table <- tibble::tibble(Bank_name = Bank_name, a_1_tau = a_1_tau, e_1_tau = e_1_tau,
                              lambda_1_tau = lambda_1_tau)

return(state_table)


} else {

del_lower <- dplyr::select(fp, .data$delta_lower) %>% as.matrix()
del_upper <- dplyr::select(fp, .data$delta_upper) %>% as.matrix()

theta_lower <- bank_behavior_function(del = del_lower, mat = dat, lb = lb) %>% as.matrix()
theta_upper <- bank_behavior_function(del = del_upper, mat = dat, lb = lb) %>% as.matrix()

Bank_name <- rownames(dat$S_1)

a_1_tau_lower <- ( (rowSums(dat$S_1) - dat$S_1%*%del_lower)*(1-theta_lower) + rowSums(dat$L_1)) %>% as.numeric()
e_1_tau_lower <- (dat$e_1 - dat$S_1%*%del_lower) %>% as.numeric()
lambda_1_tau_lower <- (a_1_tau_lower/e_1_tau_lower) %>% as.numeric()

a_1_tau_upper <- ( (rowSums(dat$S_1) - dat$S_1%*%del_upper)*(1-theta_upper) + rowSums(dat$L_1)) %>% as.numeric()
e_1_tau_upper <- (dat$e_1 - dat$S_1%*%del_upper) %>% as.numeric()
lambda_1_tau_upper <- (a_1_tau_upper/e_1_tau_upper) %>% as.numeric()



state_table <- tibble::tibble(Bank_name = Bank_name,
                              a_1_tau_lower = a_1_tau_lower,
                              a_1_tau_upper = a_1_tau_upper,
                              e_1_tau_lower = e_1_tau_lower,
                              e_1_tau_upper = e_1_tau_upper,
                              lambda_1_tau_lower = lambda_1_tau_lower,
                              lambda_1_tau_upper = lambda_1_tau_upper
                              )

return(state_table)

}

}
