test_that("equity vector at t0 is constructed correctly", {

  library(syslosseval)

  # data 201512

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)


  e0_from_function_2016 <- make_e0(stress_data_2016)

  e0_from_data_2016 <- stress_data_2016 %>%
    dplyr::filter(Exposure == "Common tier1 equity capital") %>%
    dplyr::select(Bank_name, Total_Amount)

  # test whether we get the same values for Bank_names and for Total_Amount:

  expect_equal(rownames(e0_from_function_2016), unlist(dplyr::select(e0_from_data_2016, Bank_name)))
  expect_equal(as.numeric(e0_from_function_2016), as.numeric(unlist(dplyr::select(e0_from_data_2016, Total_Amount))))

  # data 201912

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)

  e0_from_function_2020 <- make_e0(stress_data_2020)

  e0_from_data_2020 <- stress_data_2020 %>%
    dplyr::filter(Exposure == "Common tier1 equity capital") %>%
    dplyr::select(Bank_name, Total_Amount)

  # test whether we get the same values for Bank_names and for Total_Amount:

  expect_equal(rownames(e0_from_function_2020), unlist(dplyr::select(e0_from_data_2020, Bank_name)))
  expect_equal(as.numeric(e0_from_function_2020), as.numeric(unlist(dplyr::select(e0_from_data_2020, Total_Amount))))

})
