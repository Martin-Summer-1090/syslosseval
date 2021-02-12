#' make_sovereign_bond_exposures
#'
#' @param data a dataframe with the compiled EBA exposure data
#'
#' @return a dataframe with the bond exposures for DE, ES, IT, FR, JP, GB, US and Rest of the world
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' make_sovereign_bond_exposures(eba_exposures_2016)
make_sovereign_bond_exposures <- function(data) {

  # Extract the bond exposure data for the total and the specific countries we use in our paper, which are
  # Germany, Spain, Italy, France, United Kingdom, United States and Japan and
  # build a position Rest_of_the_world by computing the difference between the total exposure and the
  # aggregate exposure of "DE", "ES", "FR", "IT", "JP", "GB", "US"

  # Filter Data for Exposure and Country and select bond amount

  # We must filter also with respect to the entire bank list, because there might be some banks which have no exposure
  # to any of our categories and then this should be documented with a value of zero. If we do not include these banks
  # we emerge with a shortened bank list at the end.



  data_analyzed <- data %>%
    dplyr::filter(
      .data$Exposure == "Central banks and central governments") %>%
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


  # Build the rest of the world position


  # First filter all the banks which have a bond exposure to "DE", "ES", "FR", "IT", "JP", "GB", "US"

  sum_banks_with_exposures <- data_analyzed %>%
    dplyr::filter(.data$Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US")) %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarize(Bond_Amount = sum(.data$Bond_Amount, na.rm = T))

  # Next filter the Total position in bond exposure of all banks

  total_banks <- data_analyzed %>%
    dplyr::filter(.data$Country == "Total")

  # bind together in a left_join operation. This will reveal the banks which have no exposure to any of the listed individual
  # countries, which get an NA in the exposure, which we replace by a value of 0.

  row <- dplyr::left_join(total_banks, sum_banks_with_exposures, by = "LEI_code") %>%
    dplyr::mutate(Bond_res = .data$Bond_Amount.x - .data$Bond_Amount.y) %>%
    dplyr::rename(Bank_name = .data$Bank_name.x) %>%
    dplyr::select(.data$LEI_code, .data$Country_code, .data$Bank_name, .data$Period, .data$Exposure, .data$Bond_res, .data$Unit, .data$Currency) %>%
    tibble::add_column(Country = "Rest_of_the_world", .after = "Period") %>%
    dplyr::rename(Bond_Amount = .data$Bond_res)

  aux <- dplyr::filter(row, is.na(.data$Bond_Amount)) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Bond_Amount) %>%
    dplyr::left_join(total_banks, by = "LEI_code") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name.x, .data$Country_code, .data$Period,
                  .data$Exposure, .data$Bond_Amount.y, .data$Unit, .data$Currency) %>%
    tibble::add_column(Country = "Rest_of_the_world", .after = "Period") %>%
    dplyr::rename(Bank_name = .data$Bank_name.x) %>%
    dplyr::rename(Bond_Amount = .data$Bond_Amount.y)

  # Now we assemble the entire dataframe for the rest of the world position, which encompasses
  # all banks in "row" which are not in aux together with the "aux" banks

  lei <- dplyr::filter(row, is.na(.data$Bond_Amount)) %>%
    dplyr::select(.data$LEI_code) %>% unlist() %>% unique()

  subs <- row %>%
    dplyr::filter(!(.data$LEI_code %in% lei))

  rest_of_the_world <- dplyr::bind_rows(subs, aux)

  # stitch the whole data frame countries and rest of the world together

individual_countries <- data_analyzed %>%
  dplyr::filter(.data$Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US"))

bonds_total <- dplyr::bind_rows(individual_countries, rest_of_the_world) %>%
  dplyr::arrange(.data$LEI_code)

bonds_total

}
