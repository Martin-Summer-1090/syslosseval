#' make_stress_data
#'
#' This function creates from exposure and impairment data a new dataframe which
#' makes all data available for conducting an EBA stress test, given two
#' parameters: \code{horizon}, the future horizon of losses assumed by the
#' stress test and \code{base_year}.
#'
#' @param exposure_data a dataframe with the exposure data. The exposure data
#'   must contain the variables LEI_code (chr), Country_code (chr), Bank_name
#'   (chr), Period (num), Country (chr), Exposure (chr), Loan_Amount (num),
#'   Bond_Amount (num), Total_Amount (num), Unit (chr), Currency (chr)
#' @param impairment_data a dataframe with the impairment data. The impairment
#'   data must contain the variables LEI_code (chr), Country_code (chr),
#'   Bank_name (chr), Period (num), Scenario (num), Country (chr), Exposure
#'   (chr), Impairment_rate (num)
#' @param horizon stress test horizon (1,2,3 years ahead)
#' @param base_year base year of the stress test (2015 or 2019)
#' @return A dataframe with the following variables: LEI_code (chr), Country_code (chr),
#'   Bank_name (chr), Period (num), Country (chr), Exposure (chr), Loan_Amount (num), Bond_Amount (num),
#'   Total_Amount (num), Loan_Losses (num), Unit (chr), Currency (chr), Impairment_rate (num)
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
make_stress_data <- function(exposure_data, impairment_data, horizon, base_year) {

  # filter the stress values of the impairment rates

  impairment_rates <- impairment_data %>%
    dplyr::filter(
      .data$Period == as.numeric(paste0(as.character(base_year + horizon), 12)),
      .data$Scenario == "Adverse scenario"
    ) %>%
    dplyr::select(.data$LEI_code, .data$Country, .data$Exposure, .data$Impairment_rate)



  # make an exposure dataframe with impairments added

  stress_data <- dplyr::left_join(exposure_data, impairment_rates, by = c("LEI_code", "Country", "Exposure")) %>%
    dplyr::filter(!(.data$Exposure %in% c("Total assets", "Common tier1 equity capital"))) %>%
    dplyr::mutate(Loan_Losses = (.data$Loan_Amount) * (.data$Impairment_rate)) %>%
    dplyr::select(
      .data$LEI_code, .data$Country_code, .data$Bank_name,
      .data$Period, .data$Country, .data$Exposure, .data$Loan_Amount, .data$Bond_Amount, .data$Total_Amount,
      .data$Loan_Losses, .data$Unit, .data$Currency, .data$Impairment_rate
    )


  # now prepare the total asset values and the cet1 figures to append to the stressed the dataframe

  data_append <- exposure_data %>%
    dplyr::filter(.data$Exposure %in% c("Total assets", "Common tier1 equity capital")) %>%
    dplyr::mutate(Loan_Amount = 0) %>%
    tibble::add_column(Loan_Losses = 0, .after = "Total_Amount") %>%
    tibble::add_column(Impairment_rate = 0, .after = "Currency")

  # append and replace NA by 0

  all_data <- dplyr::bind_rows(stress_data, data_append)

  all_data
}
