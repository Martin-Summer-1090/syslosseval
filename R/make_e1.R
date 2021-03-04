#' make_e1
#'
#' This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
#' output of \code{make_stress_data()}, and constructs a vector giving the common tier 1 equity of all banks at t = 1,
#' which is common tier 1 equity minus losses from the stressed impairments. The components of the vector e_1
#' are the differecne between the components of e_0 minus the projected losses of the stress test. If these
#' losses exceed e_0, e_1 is set to zero.
#'
#' @param data a dataframe which is the output of \code{make_stress_data()}
#'
#' @return a B x 1 (number of banks x 1) with Core tier1 equity capital of all banks at t = 1.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' make_e1(stress_data)
make_e1 <- function(data) {

  equity <- data %>%
    dplyr::filter(.data$Exposure == "Common tier1 equity capital", .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount)

  losses <- data %>%
    dplyr::filter(!(.data$Exposure %in% c("Common tier1 equity capital", "Total assets"))) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Loan_Losses) %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarize(Losses = sum(.data$Loan_Losses, na.rm = T)) %>%
    dplyr::ungroup()

  equity_vector_1 <- dplyr::left_join(equity, losses, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(e_1 = dplyr::if_else((.data$Total_Amount - .data$Losses) > 0, (.data$Total_Amount - .data$Losses), 0))

  e_1 <- matrix(unlist(dplyr::select(equity_vector_1, .data$e_1)), nrow = dim(equity_vector_1)[1], ncol = 1)

  rownames(e_1) <- dplyr::select(equity_vector_1, .data$Bank_name) %>% unlist()
  colnames(e_1) <- "e_1"

  return(e_1)
}
