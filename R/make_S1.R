#' make_S1
#'
#' This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
#' output of \code{make_stress_data()}, and constructs a matrix giving the sovereign bond exposures in
#' DE, ES, GB. FR, IT, JP, US and the rest of the world.
#'
#' @param data a dataframe which is the output of \code{make_stress_data()}
#'
#' @return a B x I (number of banks x number of marketable security classes) with the sovereign
#'         bond exposures for DE, ES, IT, FR, JP, GB, US and Rest of the world
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' make_S1(stress_data)
make_S1 <- function(data) {

  bond_exposures_eba <- data %>%
    dplyr::filter(
      .data$Exposure == "Central banks and central governments",
      .data$Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US", "Total")
    ) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Country, .data$Bond_Amount)

  # We bring this into a tabular form with each row a bank and each column a country. Not every bank has an
  # exposure to every country and so the table will have NA for these banks, which in our context is equivalent
  # to a bond exposure of 0, so we substitute NA with zero. In the table we can compute a new variable "Rest_of_the_world",
  # which gives us the differecne of the sum of individual country exposures and the total exposure figure.

  bond_exposures_eba_table <- bond_exposures_eba %>%
    tidyr::pivot_wider(names_from = .data$Country, values_from = .data$Bond_Amount) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0)) %>%
    dplyr::mutate(Rest_of_the_world = .data$Total - (.data$DE + .data$ES + .data$FR + .data$IT + .data$JP + .data$GB + .data$US)) %>%
    dplyr::select(.data$Bank_name, .data$DE, .data$ES, .data$FR, .data$GB, .data$IT, .data$JP, .data$US, .data$Rest_of_the_world)

  # Now transform the table into a matrix

  security_matrix_1 <- as.matrix(bond_exposures_eba_table[, c("DE", "ES", "FR", "GB", "IT", "JP", "US", "Rest_of_the_world")])

  row.names(security_matrix_1) <- dplyr::select(bond_exposures_eba_table, .data$Bank_name) %>% unlist()

  return(security_matrix_1)
}
