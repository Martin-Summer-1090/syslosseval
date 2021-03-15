test_that("price impact function fulfills assumption 3", {

  library(syslosseval)

# 2016 data

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
  matrices_2016 <- make_state_variables(stress_data_2016)

# compute lower and upper discount factor:

delta_lower_2016 <- rep(0, dim(matrices_2016$S_0)[2])

q_max_2016 <- rowSums(t(matrices_2016$S_1))

impact_data_2016 <- make_price_impact_data(sovereign_bond_indices, average_daily_volume_sovereign, 2015) %>%
  tibble::add_column(kappa = 1.5) %>%
  tibble::add_column(quantity = q_max_2016) %>%
  dplyr::mutate(Impact = Volatility * kappa * sqrt(quantity/Volume))

delta_upper_2016 <- dplyr::select(impact_data_2016, Impact) %>% as.matrix()

pil_2016 <- price_impact_function(del = delta_lower_2016, mat = matrices_2016, lb = 33,
                                  data_idx = sovereign_bond_indices,
                                  data_adv = average_daily_volume_sovereign,
                             base_year = 2015, constant = 1.5)

piu_2016 <- price_impact_function(del = delta_upper_2016, mat = matrices_2016, lb = 33,
                                  data_idx = sovereign_bond_indices,
                                  data_adv = average_daily_volume_sovereign,
                             base_year = 2015, constant = 1.5)

# is pil larger or equal to zero

expect_equal(all(pil_2016 >= 0), T)

# is piu lower than one

expect_equal(all(piu_2016 < 1), T)

# 2020 data

stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)
matrices_2020 <- make_state_variables(stress_data_2020)

# compute lower and upper discount factor:

delta_lower_2020 <- rep(0, dim(matrices_2020$S_0)[2])

q_max_2020 <- rowSums(t(matrices_2020$S_1))

impact_data_2020 <- make_price_impact_data(sovereign_bond_indices, average_daily_volume_sovereign, 2015) %>%
  tibble::add_column(kappa = 1.5) %>%
  tibble::add_column(quantity = q_max_2020) %>%
  dplyr::mutate(Impact = Volatility * kappa * sqrt(quantity/Volume))

delta_upper_2020 <- dplyr::select(impact_data_2020, Impact) %>% as.matrix()

pil_2020 <- price_impact_function(del = delta_lower_2020, mat = matrices_2020, lb = 33,
                                  data_idx = sovereign_bond_indices,
                                  data_adv = average_daily_volume_sovereign,
                                  base_year = 2015, constant = 1.5)

piu_2020 <- price_impact_function(del = delta_upper_2020, mat = matrices_2020, lb = 33,
                                  data_idx = sovereign_bond_indices,
                                  data_adv = average_daily_volume_sovereign,
                                  base_year = 2015, constant = 1.5)

# is pil larger or equal to zero

expect_equal(all(pil_2020 >= 0), T)

# is piu lower than one

expect_equal(all(piu_2020 < 1), T)

  })
