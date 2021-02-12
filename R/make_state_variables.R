#' make_state_variables
#'
#' @param stress_data a dataframe which is the output of make_stress_data
#' @param meth a method, which can take the values "asymmetric residuals", "symmetric residuals".
#'             It influences how the construction of state variables deals with the gaps between
#'             EBA exposures and reported total assets.
#'             We have to deal with the fact that the total value of EBA credit exposures is
#'             for some banks larger than reported total assets and for some banks smaller.
#'             This has to be corrected for computations of leverage. We suggest two
#'             methods for correction. With the method "asymmetric residuals" we would compute
#'             a residual value for the assets of each banks such that the total value of
#'             EBA exposures for each bank plus the residual will add up to the value of
#'             total assets reported in the bank balancesheet. In  the case where the value
#'             of total assets is smaller than the value of EBA exposures the residual is
#'             set to 0 so that the total value of EBA exposures is identified with the value of
#'             total assets. In this case fro some banks leverage is perhaps overstated. With
#'             the method "symmetric residuals" we compute the residual as the
#'             difference between total assets as reported in the balance sheet and the total
#'             value of eba exposures fro each bank. When we add this residual to the sum of
#'             eba exposures we will get in total the value of total assets for all banks
#'             as reported in the balance sheet. This will relate the leverage
#'             measure to the reported total asset value for each bank. In this case for some
#'             banks the leverage may be underestimated. The default value will be "asymmetric
#'             residuals".
#'
#' @return A list, with the state variables at t = 0 and t = 1
#'         $e_0$ B x 1 vector of core tier 1 equity at t = 0
#'         $S_0$ B x I matrix of security exposures at t = 0
#'         $L_0$ B x J matrix of loan exposures at t = 0
#'         $e_1$ B x 1 vector of core tier 1 equity at t=1
#'         $S_1$ B x I matrix of security exposures at $t = 1$
#'         $L_1$ B x J matrix of loan exposures at $t=1$
#'         $theta$ B x 1 vector of fire sale proportions (a vector of zeros)
#'
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#'
#' @examples stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2016)
#'           make_state_variables(stress_data, meth = "asymmetric residuals")
make_state_variables <- function(stress_data, meth = "asymmetric residuals"){

# prepare data for method "asymmetric residuals"

if(meth == "asymmetric residuals"){

# construct the residual position from the stress data

  eba_total <- stress_data %>%
    dplyr::filter(!(.data$Exposure %in% c("Total assets", "Common tier1 equity capital"))) %>%
    dplyr::filter(.data$Country == "Total") %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarize(Total_Amount_eba = sum(.data$Total_Amount, na.rm = T))

  total_assets <- stress_data %>%
    dplyr::filter(.data$Exposure == "Total assets")

  residual_position <- dplyr::left_join(total_assets, eba_total, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(Total_Amount_res = dplyr::if_else(.data$Total_Amount > .data$Total_Amount_eba, (.data$Total_Amount - .data$Total_Amount_eba), 0)) %>%
    dplyr::select(.data$LEI_code, .data$Country_code, .data$Bank_name, .data$Period, .data$Country, .data$Loan_Amount,
                  .data$Bond_Amount, .data$Total_Amount, .data$Total_Amount_res, .data$Unit, .data$Currency, .data$Impairment_rate) %>%
    tibble::add_column(Exposure = "Residual", .after = "Country")

  # we write the total values of the residual to the Loan amount position because we need this later when we build the matrices

  residual_position$Loan_Amount <- residual_position$Total_Amount


# assemble the entrire dataframe

  all_data <- stress_data %>%
    dplyr::filter(.data$Exposure != "Total assets") %>%
    dplyr::bind_rows(residual_position) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0))

}

  # prepare data for method "symmetric residuals"

  if(meth == "symmetric residuals"){

    # construct the residual position from the stress data

    eba_total <- stress_data %>%
      dplyr::filter(!(.data$Exposure %in% c("Total assets", "Common tier1 equity capital"))) %>%
      dplyr::filter(.data$Country == "Total") %>%
      dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
      dplyr::summarize(Total_Amount_eba = sum(.data$Total_Amount, na.rm = T))

    total_assets <- stress_data %>%
      dplyr::filter(.data$Exposure == "Total assets")

    residual_position <- dplyr::left_join(total_assets, eba_total, by = c("LEI_code", "Bank_name")) %>%
      dplyr::mutate(Total_Amount_res = .data$Total_Amount - .data$Total_Amount_eba) %>%
      dplyr::select(.data$LEI_code, .data$Country_code, .data$Bank_name, .data$Period, .data$Country, .data$Loan_Amount,
                    .data$Bond_Amount, .data$Total_Amount, .data$Total_Amount_res, .data$Unit, .data$Currency, .data$Impairment_rate) %>%
      tibble::add_column(Exposure = "Residual", .after = "Country")

    # assemble the entrire dataframe

    all_data <- stress_data %>%
      dplyr::filter(.data$Exposure != "Total assets") %>%
      dplyr::bind_rows(residual_position) %>%
      dplyr::mutate_all(~ replace(., is.na(.), 0))
  }


  # make matrices of state variables

  equity_vector_0 <- all_data %>%
    dplyr::filter(.data$Exposure == "Common tier1 equity capital", .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount)

  e_0 <- matrix(unlist(dplyr::select(equity_vector_0, .data$Total_Amount)), nrow = dim(equity_vector_0)[1], ncol = 1)
  rownames(e_0) <- dplyr::select(equity_vector_0, .data$Bank_name) %>% unlist()
  colnames(e_0) <- "e_0"

  # Construct the loan matrix $L_0$

  loan_matrix_0 <- all_data %>%
    dplyr::filter(.data$Exposure != "Common tier1 equity capital", .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Exposure, .data$Loan_Amount)

  L_0_table <- dplyr::select(loan_matrix_0, .data$Bank_name, .data$Exposure, .data$Loan_Amount) %>%
    tidyr::pivot_wider(names_from = .data$Exposure, values_from = .data$Loan_Amount)

  L_0 <- data.matrix(L_0_table[,2:dim(L_0_table)[2]])

  rownames(L_0) <- (dplyr::select(loan_matrix_0, .data$Bank_name) %>% unlist() %>% unique())

  # Construct the security matrix $S_0$. To prepare the data we invoke the function we have written for
  # pre-processing bond exposures

  security_matrix_0 <- make_sovereign_bond_exposures(all_data) %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::arrange(.data$Country) %>%
    dplyr::ungroup()


  S_0_table <- dplyr::select(security_matrix_0, .data$Bank_name, .data$Country, .data$Bond_Amount) %>%
    tidyr::pivot_wider(names_from = .data$Country, values_from = .data$Bond_Amount) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0))

  S_0 <- data.matrix(S_0_table[,2:dim(S_0_table)[2]])

  rownames(S_0) <- (dplyr::select(security_matrix_0, .data$Bank_name)%>% unlist() %>% unique())

  # Construct core tier 1 equity column vector $e_1$:

  aux_1 <- all_data %>%
    dplyr::filter(.data$Exposure == "Common tier1 equity capital", .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount)

  aux_2 <- all_data %>%
    dplyr::filter(.data$Country == "Total") %>%
    dplyr::filter(.data$Exposure != "Common tier1 equity capital") %>%
    dplyr::group_by(.data$LEI_code, .data$Bank_name) %>%
    dplyr::summarise(Total_Loan_Losses = sum(.data$Loan_Losses, na.rm = T))

  equity_vector_1 <- dplyr::left_join(aux_1, aux_2, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(Total_Amount = dplyr::if_else((.data$Total_Amount - .data$Total_Loan_Losses) > 0, (.data$Total_Amount - .data$Total_Loan_Losses), 0)) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Total_Amount)

  e_1 <- matrix(unlist(dplyr::select(equity_vector_1, .data$Total_Amount)), nrow = dim(equity_vector_1)[1], ncol = 1)
  rownames(e_1) <- dplyr::select(equity_vector_1, .data$Bank_name) %>% unlist()
  colnames(e_1) <- "e_1"

  # Construct loan matrix L_1

  loan_matrix_1 <- all_data %>%
    dplyr::filter(.data$Exposure != "Common tier1 equity capital", .data$Country == "Total") %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Exposure, .data$Loan_Amount, .data$Loan_Losses) %>%
    dplyr::mutate(Loan_Amount = .data$Loan_Amount - .data$Loan_Losses) %>%
    dplyr::select(.data$LEI_code, .data$Bank_name, .data$Exposure, .data$Loan_Amount)

  L_1_table <- dplyr::select(loan_matrix_1, .data$Bank_name, .data$Exposure, .data$Loan_Amount) %>%
    tidyr::pivot_wider(names_from = .data$Exposure, values_from = .data$Loan_Amount)

  L_1 <- data.matrix(L_1_table[,2:dim(L_0_table)[2]])

  rownames(L_1) <- (dplyr::select(loan_matrix_1, .data$Bank_name) %>% unlist() %>% unique())

  # Construct security matrix S_1 (since we have by assumption no market shocks we can just copy S_0)

  S_1 <- S_0

  # Add the zero fire sale proportions to the list

  theta <- matrix(0, nrow = dim(equity_vector_0)[1], ncol = 1)
  rownames(theta) <- rownames(e_0)
  colnames(theta) <- "theta"

  # Return a list of all matrices e_0, S_0, L_0, e_1, S_1, L_1, theta)

  matrices <- list(e_0 = e_0, S_0 = S_0, L_0 = L_0, e_1 = e_1, S_1 = S_1, L_1 = L_1, theta = theta)

}



