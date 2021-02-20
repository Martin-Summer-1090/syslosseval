#' make_e0
#'
#' This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
#' output of \code{make_stress_data()}, and constructs a vector giving the common tier 1 equity of all banks.
#'
#' @param data a dataframe which is the output of \code{make_stress_data()}
#'
#' @return a B x 1 (number of banks x 1) with Core tier1 equity capital of all banks.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @examples
#' stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
#' make_e0(stress_data)
make_e0 <- function(data){


  equity_vector_0 <- data %>%
    dplyr::filter(.data$Exposure == "Common tier1 equity capital", .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount)

  e_0 <- matrix(unlist(dplyr::select(equity_vector_0, .data$Total_Amount)), nrow = dim(equity_vector_0)[1], ncol = 1)
  rownames(e_0) <- dplyr::select(equity_vector_0, .data$Bank_name) %>% unlist()
  colnames(e_0) <- "e_0"

return(e_0)

}
