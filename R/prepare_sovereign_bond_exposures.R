#' prepare_sovereign_bond_exposures
#'
#' @param data a dataframe with the compiled EBA exposure data
#'
#' @return a dataframe with the bond exposures for DE, ES, IT, FR, UK, US and total
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' prepare_sovereign_bond_exposures(eba_exposures_2016)
prepare_sovereign_bond_exposures <- function(data) {

  # Extract the bond exposure data for the total and the specific countries we use in our paper, which are
  # Germany, Spain, Italy, France, United Kingdom, United States and Japan.

  # Filter Data for Exposure and Country and select bond amount

  data %>%
    dplyr::filter(
      .data$Exposure == "Central banks and central governments",
      .data$Country %in% c("Total", "DE", "ES", "FR", "IT", "JP", "UK", "US")
    ) %>%
    dplyr::select(
      .data$LEI_code,
      .data$Country_code,
      .data$Bank_name,
      .data$Period,
      .data$Country,
      .data$Exposure,
      .data$Bond_Amount,
      .data$Unit,
      .data$Currency
    )
}
