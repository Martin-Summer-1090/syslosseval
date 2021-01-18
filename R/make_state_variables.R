#' make_state_variables
#'
#' @param exposure_data exposure data
#' @param impairment_data impairment data
#' @param horizon stress test horizon (1,2,3 years ahead)
#' @param base_year base year of the stress test
#' @return A list containing the following objects:
#'         states: A dataframe with dimensions number of banks times 4 with variables $a_0$, $a_1$,
#'         $\lambda_0$, $\lambda_1$, $\bar{\lambda}$ and fire_sales_init an indicator which takes on value 1 if
#'         $\lambda_1 > \bar{\lambda}$ and zero otherwise.
#'         Security matrix at t= 0: $S^0$
#'         Loan matrix at t = 0: $L^0$
#'         Equity vector at t = 0: $e^0$
#'         Security matrix at t = 1: $S^1$
#'         Loan matrix at t = 1: $L^1$
#'         Equity vector at t = 1: $e^1$
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples make_state_variables(eba_exposures_2016, eba_impairments_2016, 1, 2016)
make_state_variables <- function(exposure_data, impairment_data, horizon, base_year){

# filter the stress values of the impairment rates

  impairment_rates <- impairment_data %>%
    dplyr::filter(.data$Period == as.numeric(paste0(as.character(base_year + horizon), 12)),
                  Scenario == "Adverse scenario") %>%
    dplyr::select(.data$LEI_code, .data$Country, .data$Exposure, .data$Impairment_rate)

# make an exposure dataframe with impairments added

  stress_data <- dplyr::left_join(exposure_data, impairment_rates, by = c("LEI_code", "Country", "Exposure")) %>%
    dplyr::filter(!(.data$Exposure %in% c("Total assets", "Common tier1 equity capital"))) %>%
    dplyr::mutate(Loan_Losses = (.data$Loan_Amount)*(.data$Impairment_rate)) %>%
    dplyr::select(.data$LEI_code, .data$Country_code, .data$Bank_name,
                  .data$Period, .data$Country, .data$Exposure, .data$Loan_Amount, .data$Bond_Amount, .data$Total_Amount,
                  .data$Loan_Losses, .data$Unit, .data$Currency, .data$Impairment_rate)

# the EBA data report asset values which are regarded as assets with a credit risk. The sum of these values does
# not necessarily match the value of total assets. We add a "residual position to the stress data to get a size of
# the balance sheet which is consistent with the reported total assets. We compute the residual in  the next step:

  # sum of all eba exposures:

  eba_sum <- exposure_data %>%
    dplyr::filter(Country == "Total") %>%
    dplyr::group_by(LEI_code) %>%
    dplyr::summarize(Total_Amount_eba = sum(Total_Amount, na.rm = T))

  total_assets <- exposure_data %>%
    dplyr::filter(Exposure == "Total assets")

  # auxiliary dataframe to compute the residual values:

  aux <- dplyr::left_join(total_assets, eba_sum, by = "LEI_code") %>%
    dplyr::mutate(Total_Amount == (.data$Total_Amount) - (.data$Total_Amount_eba)) %>%
    dplyr::select(.data$LEI_code, .data$Country_code, .data$Bank_name, .data$Period, .data$Loan_Amount,
                  .data$Total_Amount, .data$Unit, .data$Currency)

  # append to the stress data frame

  append_data <- aux %>%
    tibble::add_column(Country = "Total", .after = "Period") %>%
    tibble::add_column(Exposure = "Residual position", .after = "Country") %>%
    tibble::add_column(Bond_Amount = 0, .after = "Loan_Amount") %>%
    tibble::add_column(Loan_Losses = 0, .after = "Total_Amount")

  # Append the residual position to our data frame and append the CET1 figures as well

  cet1_data <- dplyr::filter(exposure_data, Exposure == "Common tier1 equity capital")

  all_data <- dplyr::bind_rows(stress_data, append_data, cet1_data) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0)) %>%
    dplyr::filter(.data$Country == "Total")

  # Compute state variables:

  # Value of total assets at t = 0 for each bank

  a_0 <- all_data %>%
    dplyr::filter(.data$Exposure != "Common tier1 equity capital") %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarize(a_0 = sum(.data$Total_Amount, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Value of total assets at t = 1 for each bank

  a_1 <- all_data %>%
    dplyr::filter(.data$Exposure != "Common tier1 equity capital") %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarize(a_1 = sum((.data$Total_Amount - .data$Loan_Losses), na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Value of core tier 1 equity at t= 0 for each bank:

  e_0 <- all_data %>%
    dplyr::filter(.data$Exposure == "Common tier1 equity capital") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount) %>%
    dplyr::rename(e_0 = Total_Amount)

  # Total loan losses at t = 1 for each bank:

  total_loan_losses <- all_data %>%
    dplyr::filter(.data$Exposure != "Common tier1 equity capital") %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarise(Total_Loan_Losses = sum(.data$Loan_Losses, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Value of core tier 1 equity at t = 1


  e_1 <- dplyr::left_join(e_0, total_loan_losses, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(e_1 = dplyr::if_else((e_0 - .data$Total_Loan_Losses) > 0, (e_0 - .data$Total_Loan_Losses), 0)) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$e_1)

  # make a dataframe of the state vaieable

  states <- dplyr::left_join(a_0, e_0, by = c("LEI_code", "Bank_name")) %>%
    dplyr::left_join(a_1, by = c("LEI_code", "Bank_name")) %>%
    dplyr::left_join(e_1, by = c("LEI_code", "Bank_name"))

  states


  # states <- bind_cols(a0, select(a1, a1), select(e0, e0), select(e1, e1)) %>%
  #   mutate(lambda0 = a0/e0) %>%
  #   mutate(lambda1 = a1/e1) %>%
  #   mutate(threshold = lambda_bar) %>%
  #   mutate(fire_sales_init = (lambda1 > threshold))




}



