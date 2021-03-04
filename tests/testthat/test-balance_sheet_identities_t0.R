test_that("state variable matrices S_0 and L_0 are consistent with the total asset numbers from data", {

  library(syslosseval)

  # 2016 data

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)

  # make matrices

  L_0_2016 <- make_L0(stress_data_2016)
  S_0_2016 <- make_S0(stress_data_2016)

  a0_from_matrices_2016 <- rowSums(L_0_2016) + rowSums(S_0_2016)

  a0_from_data_2016 <- stress_data_2016 %>%
    dplyr::filter(Exposure == "Total assets") %>%
    dplyr::select(Bank_name, Total_Amount)

  # Do banks appear in the same order in both representations?

  expect_equal(names(a0_from_matrices_2016),
               (unlist(dplyr::select(a0_from_data_2016, Bank_name)) %>% unname()))

  # Is the row sum larger or equal to total assets?
  # We need to round numbers because there are differences behind the 11th digit in the case of one bank.
  # This is a numerical artifact. Actually the sum of the matrices is indeed larger of equal to the total
  # assets reported in the data, as it should be. The following test confirms this expectation.

  m_2016 <- a0_from_matrices_2016 %>% unname()
  d_2016 <- dplyr::select(a0_from_data_2016, Total_Amount) %>% unlist() %>% unname()
  comp_2016 <- round(m_2016-d_2016,9)

  expect_equal(all(comp_2016 >= 0), T)

  # 2020 data

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)

  # make matrices

  L_0_2020 <- make_L0(stress_data_2020)
  S_0_2020 <- make_S0(stress_data_2020)

  a0_from_matrices_2020 <- rowSums(L_0_2020) + rowSums(S_0_2020)

  a0_from_data_2020 <- stress_data_2020 %>%
    dplyr::filter(Exposure == "Total assets") %>%
    dplyr::select(Bank_name, Total_Amount)

  # Do banks appear in the same order in both representations?

  expect_equal(names(a0_from_matrices_2020),
               (unlist(dplyr::select(a0_from_data_2020, Bank_name)) %>% unname()))

  # Is the row sum larger or equal to total assets?
  # We need to round numbers because there are differences behind the 11th digit in the case of one bank.
  # This is a numerical artifact. Actually the sum of the matrices is indeed larger of equal to the total
  # assets reported in the data, as it should be. The following test confirms this expectation.

  m_2020 <- a0_from_matrices_2020 %>% unname()
  d_2020 <- dplyr::select(a0_from_data_2020, Total_Amount) %>% unlist() %>% unname()
  comp_2020 <- round(m_2020-d_2020,9)

  expect_equal(all(comp_2020 >= 0), T)



})
