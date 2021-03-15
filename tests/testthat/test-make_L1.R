test_that("matrix L_1 is built correctly", {

  library(syslosseval)

  # data 201512

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)

  L1_from_function_2016 <- make_L1(stress_data_2016)

  L1_from_data_2016 <- stress_data_2016 %>%
    dplyr::filter(!(Exposure %in% c("Common tier1 equity capital", "Total assets")), Country == "Total") %>%
    dplyr::select(LEI_code, Bank_name, Exposure, Loan_Amount, Loan_Losses) %>%
    dplyr::mutate(Loan_Amount = (Loan_Amount - Loan_Losses)) %>%
    dplyr::select(LEI_code, Bank_name, Exposure, Loan_Amount)

  # residual position

  total_assets_2016 <- stress_data_2016 %>%
    dplyr::filter(Exposure == "Total assets") %>%
    dplyr::select(LEI_code, Bank_name, Exposure, Total_Amount)

  total_assets_eba_2016 <- stress_data_2016 %>%
    dplyr::filter(!(Exposure %in% c("Common tier1 equity capital", "Total assets")), Country == "Total") %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::summarize(Total_Amount_EBA = sum(Total_Amount, na.rm = F))

  # compute the residual position

  residual <- dplyr::left_join(total_assets_2016, total_assets_eba_2016, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(Residual = dplyr::if_else((Total_Amount - Total_Amount_EBA) > 0, (Total_Amount - Total_Amount_EBA), 0)) %>%
    dplyr::select(Residual)

  # check whether the rownames and columns of the matrix have the same values as expected from the filtering
  # of the data. If yes the matrix is constructed correctly

  expect_equal(rownames(L1_from_function_2016), unlist(unique(dplyr::select(L1_from_data_2016, Bank_name))))

  expect_equal(as.numeric(L1_from_function_2016[,"Central banks and central governments"]),
               (dplyr::filter(L1_from_data_2016, Exposure == "Central banks and central governments") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2016[,"Retail"]),
               (dplyr::filter(L1_from_data_2016, Exposure == "Retail") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2016[,"Other non-credit obligation assets"]),
               (dplyr::filter(L1_from_data_2016, Exposure == "Other non-credit obligation assets") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2016[,"Equity"]),
               (dplyr::filter(L1_from_data_2016, Exposure == "Equity") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2016[,"Corporates"]),
               (dplyr::filter(L1_from_data_2016, Exposure == "Corporates") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2016[,"Institutions"]),
               (dplyr::filter(L1_from_data_2016, Exposure == "Institutions") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2016[,"Residual"]),
               (residual %>% unlist %>% as.numeric()))


  # data 201912

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)

  L1_from_function_2020 <- make_L1(stress_data_2020)

  L1_from_data_2020 <- stress_data_2020 %>%
    dplyr::filter(!(Exposure %in% c("Common tier1 equity capital", "Total assets")), Country == "Total") %>%
    dplyr::select(LEI_code, Bank_name, Exposure, Loan_Amount, Loan_Losses) %>%
    dplyr::mutate(Loan_Amount = (Loan_Amount - Loan_Losses)) %>%
    dplyr::select(LEI_code, Bank_name, Exposure, Loan_Amount)

  # residual position

  total_assets_2020 <- stress_data_2020 %>%
    dplyr::filter(Exposure == "Total assets") %>%
    dplyr::select(LEI_code, Bank_name, Exposure, Total_Amount)

  total_assets_eba_2020 <- stress_data_2020 %>%
    dplyr::filter(!(Exposure %in% c("Common tier1 equity capital", "Total assets")), Country == "Total") %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::summarize(Total_Amount_EBA = sum(Total_Amount, na.rm = F))

  # compute the residual position

  residual <- dplyr::left_join(total_assets_2020, total_assets_eba_2020, by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate(Residual = dplyr::if_else((Total_Amount - Total_Amount_EBA) > 0, (Total_Amount - Total_Amount_EBA), 0)) %>%
    dplyr::select(Residual)

  # check whether the rownames and columns of the matrix have the same values as expected from the filtering
  # of the data. If yes the matrix is constructed correctly

  expect_equal(rownames(L1_from_function_2020), unlist(unique(dplyr::select(L1_from_data_2020, Bank_name))))

  expect_equal(as.numeric(L1_from_function_2020[,"Central banks and central governments"]),
               (dplyr::filter(L1_from_data_2020, Exposure == "Central banks and central governments") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2020[,"Retail"]),
               (dplyr::filter(L1_from_data_2020, Exposure == "Retail") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2020[,"Other non-credit obligation assets"]),
               (dplyr::filter(L1_from_data_2020, Exposure == "Other non-credit obligation assets") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2020[,"Equity"]),
               (dplyr::filter(L1_from_data_2020, Exposure == "Equity") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2020[,"Corporates"]),
               (dplyr::filter(L1_from_data_2020, Exposure == "Corporates") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2020[,"Institutions"]),
               (dplyr::filter(L1_from_data_2020, Exposure == "Institutions") %>%
                  dplyr::select(Loan_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(L1_from_function_2020[,"Residual"]),
               (residual %>% unlist %>% as.numeric()))


})
