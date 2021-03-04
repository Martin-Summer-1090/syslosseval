test_that("matrix S_0 is constructed correctly", {

  library(syslosseval)

  # 2016 data

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)

  S0_from_function_2016 <- make_S0(stress_data_2016)

  S0_from_data_2016 <- stress_data_2016 %>%
    dplyr::filter(
      Exposure == "Central banks and central governments",
      Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US", "Total")
    ) %>%
    dplyr::select(LEI_code, Bank_name, Country, Bond_Amount)

  S0_country_sum_2016 <- S0_from_data_2016 %>%
    dplyr::filter(Country != "Total") %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::summarize(Bond_Amount_countries = sum(Bond_Amount, na.rm = T)) %>%
    dplyr::select(LEI_code, Bank_name, Bond_Amount_countries)

  S0_country_total_2016 <- S0_from_data_2016 %>%
    dplyr::filter(Country == "Total") %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::select(LEI_code, Bank_name, Bond_Amount) %>%
    dplyr::ungroup()

  S0_row_2016 <- dplyr::left_join(S0_country_total_2016, S0_country_sum_2016,
                                  by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0)) %>%
    dplyr::mutate(Bond_Amount = (Bond_Amount - Bond_Amount_countries)) %>%
    dplyr::select(Bank_name, Bond_Amount)

  # check whether the rownames and columns of the matrix have the same values as expected from the filtering
  # of the data. If yes the matrix is constructed correctly

  expect_equal(rownames(S0_from_function_2016), unlist(unique(dplyr::select(S0_from_data_2016, Bank_name))))

  # check whether the values in the columns in S_0 correspond to the values retrieved from the data. Here
  # we have the problem that the data are recorded such that only positive entries are shown. We thus don't
  # see the exposure to each country in the list for all banks. We need to "homogenize" the dataframe
  # S0_from_data_2016 such that for each bank all country exposures are recorded with exposure zero if a
  # country does not have an exposure.

  # create an artificial country list.

  bn <- dplyr::select(S0_from_data_2016, Bank_name) %>% unique() %>% unlist()
  lc <- dplyr::select(S0_from_data_2016, LEI_code) %>% unique() %>% unlist()
  bdl <- c("DE", "ES", "FR", "IT", "JP", "GB", "US", "Total")

  countries_aux <- rep(bdl, length(bn))
  bank_names_aux <- rep(bn, each = length(bdl))
  lei_codes_aux <- rep(lc, each = length(bdl))

  reference_frame <- tibble::tibble(LEI_code = lei_codes_aux, Bank_name = bank_names_aux, Country = countries_aux)

  hom_frame_2016 <- dplyr::left_join(reference_frame, S0_from_data_2016,
                                     by = c("LEI_code", "Bank_name", "Country")) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0))

 # Now we can check whether the columns for the countries in S_0 are correctly formed

  expect_equal(as.numeric(S0_from_function_2016[,"DE"]),
               (dplyr::filter(hom_frame_2016, Country == "DE") %>%
                dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2016[,"ES"]),
               (dplyr::filter(hom_frame_2016, Country == "ES") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2016[,"FR"]),
               (dplyr::filter(hom_frame_2016, Country == "FR") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2016[,"IT"]),
               (dplyr::filter(hom_frame_2016, Country == "IT") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2016[,"JP"]),
               (dplyr::filter(hom_frame_2016, Country == "JP") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2016[,"US"]),
               (dplyr::filter(hom_frame_2016, Country == "US") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2016[,"Rest_of_the_world"]),
               (dplyr::select(S0_row_2016, Bond_Amount) %>% unlist() %>% as.numeric()))

  # 2020 data

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)

  S0_from_function_2020 <- make_S0(stress_data_2020)

  S0_from_data_2020 <- stress_data_2020 %>%
    dplyr::filter(
      Exposure == "Central banks and central governments",
      Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US", "Total")
    ) %>%
    dplyr::select(LEI_code, Bank_name, Country, Bond_Amount)

  S0_country_sum_2020 <- S0_from_data_2020 %>%
    dplyr::filter(Country != "Total") %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::summarize(Bond_Amount_countries = sum(Bond_Amount, na.rm = T)) %>%
    dplyr::select(LEI_code, Bank_name, Bond_Amount_countries)

  S0_country_total_2020 <- S0_from_data_2020 %>%
    dplyr::filter(Country == "Total") %>%
    dplyr::group_by(LEI_code, Bank_name) %>%
    dplyr::select(LEI_code, Bank_name, Bond_Amount) %>%
    dplyr::ungroup()

  S0_row_2020 <- dplyr::left_join(S0_country_total_2020, S0_country_sum_2020,
                                  by = c("LEI_code", "Bank_name")) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0)) %>%
    dplyr::mutate(Bond_Amount = (Bond_Amount - Bond_Amount_countries)) %>%
    dplyr::select(Bank_name, Bond_Amount)

  # check whether the rownames and columns of the matrix have the same values as expected from the filtering
  # of the data. If yes the matrix is constructed correctly

  expect_equal(rownames(S0_from_function_2020), unlist(unique(dplyr::select(S0_from_data_2020, Bank_name))))

  # check whether the values in the columns in S_0 correspond to the values retrieved from the data. Here
  # we have the problem that the data are recorded such that only positive entries are shown. We thus don't
  # see the exposure to each country in the list for all banks. We need to "homogenize" the dataframe
  # S0_from_data_2020 such that for each bank all country exposures are recorded with exposure zero if a
  # country does not have an exposure.

  # create an artificial country list.

  bn <- dplyr::select(S0_from_data_2020, Bank_name) %>% unique() %>% unlist()
  lc <- dplyr::select(S0_from_data_2020, LEI_code) %>% unique() %>% unlist()
  bdl <- c("DE", "ES", "FR", "IT", "JP", "GB", "US", "Total")

  countries_aux <- rep(bdl, length(bn))
  bank_names_aux <- rep(bn, each = length(bdl))
  lei_codes_aux <- rep(lc, each = length(bdl))

  reference_frame <- tibble::tibble(LEI_code = lei_codes_aux, Bank_name = bank_names_aux, Country = countries_aux)

  hom_frame_2020 <- dplyr::left_join(reference_frame, S0_from_data_2020,
                                     by = c("LEI_code", "Bank_name", "Country")) %>%
    dplyr::mutate_all(~ replace(., is.na(.), 0))

  # Now we can check whether the columns for the countries in S_0 are correctly formed

  expect_equal(as.numeric(S0_from_function_2020[,"DE"]),
               (dplyr::filter(hom_frame_2020, Country == "DE") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2020[,"ES"]),
               (dplyr::filter(hom_frame_2020, Country == "ES") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2020[,"FR"]),
               (dplyr::filter(hom_frame_2020, Country == "FR") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2020[,"IT"]),
               (dplyr::filter(hom_frame_2020, Country == "IT") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2020[,"JP"]),
               (dplyr::filter(hom_frame_2020, Country == "JP") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2020[,"US"]),
               (dplyr::filter(hom_frame_2020, Country == "US") %>%
                  dplyr::select(Bond_Amount) %>% unlist() %>% as.numeric()))

  expect_equal(as.numeric(S0_from_function_2020[,"Rest_of_the_world"]),
               (dplyr::select(S0_row_2020, Bond_Amount) %>% unlist() %>% as.numeric()))

})
