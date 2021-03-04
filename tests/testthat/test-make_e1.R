test_that("equity vector at t=1 is constructed correctly", {

  library(syslosseval)

  # 2016 data

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)

  e1_from_function_2016 <- make_e1(stress_data_2016)

  e1_from_data_2016 <- stress_data_2016 %>%
    dplyr::filter(Exposure == "Common tier1 equity capital") %>%
    dplyr::select(LEI_code, Bank_name, Total_Amount)

  losses_2016 <- stress_data_2016 %>%
    dplyr::filter(!(Exposure %in% c("Common tier1 equity capital", "Total assets"))) %>%
    dplyr::select(LEI_code, Bank_name, Loan_Losses) %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::summarize(Losses = sum(Loan_Losses, na.rm = T)) %>%
    dplyr::ungroup()

  equity_vector_1 <- dplyr::left_join(e1_from_data_2016, losses_2016, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(e_1 = dplyr::if_else((Total_Amount - Losses) > 0, (Total_Amount - Losses), 0)) %>%
    dplyr::select(Bank_name, e_1)

  # test whether we get the same values for Bank_names and for common equity tier 1 after losses:

  expect_equal((rownames(e1_from_function_2016) %>% unname()), (unlist(dplyr::select(e1_from_data_2016, Bank_name)) %>% unname()))
  expect_equal(as.numeric(e1_from_function_2016), as.numeric(unlist(dplyr::select(equity_vector_1 , e_1))))


  # 2020 data

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)

  e1_from_function_2020 <- make_e1(stress_data_2020)

  e1_from_data_2020 <- stress_data_2020 %>%
    dplyr::filter(Exposure == "Common tier1 equity capital") %>%
    dplyr::select(LEI_code, Bank_name, Total_Amount)

  losses_2020 <- stress_data_2020 %>%
    dplyr::filter(!(Exposure %in% c("Common tier1 equity capital", "Total assets"))) %>%
    dplyr::select(LEI_code, Bank_name, Loan_Losses) %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::summarize(Losses = sum(Loan_Losses, na.rm = T)) %>%
    dplyr::ungroup()

  equity_vector_1 <- dplyr::left_join(e1_from_data_2020, losses_2020, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(e_1 = dplyr::if_else((Total_Amount - Losses) > 0, (Total_Amount - Losses), 0)) %>%
    dplyr::select(Bank_name, e_1)

  # test whether we get the same values for Bank_names and for common equity tier 1 after losses:

  expect_equal((rownames(e1_from_function_2020) %>% unname()), (unlist(dplyr::select(e1_from_data_2020, Bank_name)) %>% unname()))
  expect_equal(as.numeric(e1_from_function_2020), as.numeric(unlist(dplyr::select(equity_vector_1 , e_1))))

})
