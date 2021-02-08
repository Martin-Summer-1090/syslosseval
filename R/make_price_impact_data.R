#' make_price_impact_data
#'
#' @param data_idx sovereign bond indices
#' @param data_adv average daily volume data
#' @param base_year the year for which the analysis is done
#'
#' @return A dataframe with variables: Country ADV_sov_bonds, sigma_sov_bonds.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples make_price_impact_data(sovereign_bond_indices, average_daily_volume_sovereign, 2017)
make_price_impact_data <- function(data_idx, data_adv, base_year){

  # compute the log returns of bond indices for DE, ES, FR, GB, IT, JP, US, Total based on the daily log returns
  # from the base year fo the analysis

  bond_log_returns <- data_idx %>%
    dplyr::filter(stringr::str_detect(.data$Date, as.character(base_year))) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(log_returns = c(diff(log(.data$Value)), NA)) %>%
    dplyr::ungroup() %>%
    stats::na.omit()

  # make a matrix of returns

  returns_matrix <- bond_log_returns %>%
    dplyr::select(.data$Country, .data$Date, .data$log_returns) %>%
    tidyr::pivot_wider(names_from = .data$Country, values_from = .data$log_returns) %>%
    stats::na.omit() %>%
    dplyr::select(-(.data$Date))

  # compute country volatilities

  Sigma <- stats::cov(as.matrix(returns_matrix)) %>%
    diag() %>%
    sqrt()

  Sigma

  Names <- names(Sigma)
  Values <- unname(Sigma)

  vola <- tibble::tibble(Country = Names, Volatility = Values)


  # extract average daily volume from the base_year

   ADV <- data_adv %>%
    dplyr::filter(.data$Year == base_year)


  # combine the impact data

   impact_data <- dplyr::left_join(ADV, vola, by = "Country")

   impact_data


}
