#' make_L1
#'
#' This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
#' output of \code{make_stress_data()}, and constructs a matrix giving the loan exposure in all IRB exposure
#' classes for all banks.
#'
#' @param data a dataframe which is the output of \code{make_stress_data()}
#'
#' @return a B x J (number of banks x number of IRB exposure categories)
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' make_L1(stress_data)
make_L1 <- function(data){

  # drop the total asset figure and the cet1 figures from the dataframe and select the relevant variables

  eba_loan_exposures <- data %>%
    dplyr::filter(!(.data$Exposure %in% c("Common tier1 equity capital", "Total assets")), .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Exposure, .data$Loan_Amount, .data$Loan_Losses) %>%
    dplyr::mutate(Loan_Amount = (.data$Loan_Amount - .data$Loan_Losses)) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Exposure, .data$Loan_Amount)

  eba_loan_exposures_table <- eba_loan_exposures %>%
    tidyr::pivot_wider(names_from = .data$Exposure, values_from = .data$Loan_Amount) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, "Central banks and central governments",
                  .data$Institutions, .data$Corporates, .data$Retail, .data$Equity,
                  .data$Securitisation, "Other non-credit obligation assets")

  # Now transform the table into a matrix

  loan_matrix_1 <- eba_loan_exposures_table %>%
    dplyr::select("Central banks and central governments",
                  .data$Institutions, .data$Corporates, .data$Retail, .data$Equity,
                  .data$Securitisation, "Other non-credit obligation assets") %>%
    as.matrix()

  row.names(loan_matrix_1) <- dplyr::select(eba_loan_exposures_table, .data$Bank_name) %>% unlist()

  return(loan_matrix_1)

}
