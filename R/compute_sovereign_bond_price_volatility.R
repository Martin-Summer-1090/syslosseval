#' compute_sovereign_bond_price_volatility
#'
#' @param data a dataframe/tibble containing the daily sovereign bond index values for all countries.
#' @param from start year of the data
#' @param to   year up to which we consider the data
#'
#' @return sovereign bond log returns by country.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @examples
#' compute_sovereign_bond_price_volatility(sovereign_bond_indices, 2010, 2019)
compute_sovereign_bond_price_volatility <- function(data, from, to){

  # check whether the data range is meaningful

  test_range <- data %>%
    dplyr::mutate(Date = lubridate::year(.data$Date)) %>%
    dplyr::select(.data$Date) %>%
    unique() %>%
    unlist()

  # determine the minimum and maximum date in your data

  minimum <- min(test_range)
  maximum <- max(test_range)

  # issue an error message if from and to are out of range

  if(from < minimum) stop("The specified start year is not contained in the range of your data")
  if(to > maximum) stop("The specified end year is not contained in the range of your data")


   # compute volatility per country

  bond_log_returns <- data %>%
    dplyr::filter(substr(.data$Date,1,4) %in% from:to) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(log_returns = c(diff(log(.data$Value)), NA)) %>%
    dplyr::ungroup() %>%
    stats::na.omit()

return(bond_log_returns)

}
