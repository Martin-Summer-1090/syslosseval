#' make_L0
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
#' make_L0(stress_data)
make_L0 <- function(data) {

  # drop the total asset figure and the cet1 figures from the dataframe and select the relevant variables

  eba_loan_exposures <- data %>%
    dplyr::filter(!(.data$Exposure %in% c("Common tier1 equity capital", "Total assets")), .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Exposure, .data$Loan_Amount)

  eba_loan_exposures_table <- eba_loan_exposures %>%
    tidyr::pivot_wider(names_from = .data$Exposure, values_from = .data$Loan_Amount) %>%
    dplyr::select(
      .data$LEI_code, .data$Bank_name, "Central banks and central governments",
      "Institutions", "Corporates", "Retail", "Equity", "Other non-credit obligation assets"
    )

  # In the EBA data there is a gap between the total value of exposures and the published value of total assets. The
  # precise source of these gaps is unclear but in general it comes from the fact that on the not all assets of the
  # banks are condidered subject to credit risk by EBA. On the other hand some exposures are inflated by
  # conversion factors. So the sum of EBA exposures can be larger or smaller than the reported total assets. The
  # gaps can be large. We therefore use a partial correction for the cases where eba-exposures are smaller that
  # reported total assets- For the cases where eba-exposures are larger we consider the sum of eba-exposures as the
  # value of total assets.

  total_assets <- data %>%
    dplyr::filter(.data$Exposure == "Total assets") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount)

  # value of total EBA exposures

  total_assets_eba <- data %>%
    dplyr::filter(!(.data$Exposure %in% c("Common tier1 equity capital", "Total assets")), .data$Country == "Total") %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarize(Total_Amount_EBA = sum(.data$Total_Amount, na.rm = F))

  # compute the residual position

  residual <- dplyr::left_join(total_assets, total_assets_eba, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(Residual = dplyr::if_else(
      (.data$Total_Amount - .data$Total_Amount_EBA) > 0,
      (.data$Total_Amount - .data$Total_Amount_EBA), 0
    )) %>%
    dplyr::select(.data$Residual)

  # add to eba_loan_exposures_table

  eba_loan_exposures_table_augmented <- dplyr::bind_cols(eba_loan_exposures_table, residual)

  # Now transform the table into a matrix

  loan_matrix_0 <- eba_loan_exposures_table_augmented %>%
    dplyr::select(-c(.data$LEI_code, .data$Bank_name)) %>%
    as.matrix()

  row.names(loan_matrix_0) <- dplyr::select(eba_loan_exposures_table_augmented, .data$Bank_name) %>% unlist()

  return(loan_matrix_0)
}
