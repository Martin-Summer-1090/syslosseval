test_that("computes fixed points are indeed fixed points", {

  library(syslosseval)


# test on the 2016 data

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
  state_variables_2016 <- make_state_variables(stress_data_2016)

  fixed_points_2016 <- fixed_point_computation_function(mat = state_variables_2016, lb = 33,
                                                        data_idx = sovereign_bond_indices,
                                                        data_adv = average_daily_volume_sovereign,
                                                        base_year = 2015,
                                                        constant = 1.5
                                  )

  lfp_2016 <- fixed_points_2016 %>% dplyr::select(delta_lower) %>% unlist() %>% unname() %>% as.matrix()
  ufp_2016 <- fixed_points_2016 %>% dplyr::select(delta_upper) %>% unlist() %>% unname() %>% as.matrix()

  pimpl_2016 <- price_impact_function(del = lfp_2016, mat = state_variables_2016,
                        lb = 33, data_idx = sovereign_bond_indices,
                        data_adv = average_daily_volume_sovereign, base_year = 2015,
                        constant = 1.5) %>% unname()

  pimpu_2016 <- price_impact_function(del = ufp_2016, mat = state_variables_2016,
                                 lb = 33, data_idx = sovereign_bond_indices,
                                 data_adv = average_daily_volume_sovereign, base_year = 2015,
                                 constant = 1.5) %>% unname()

  expect_equal(lfp_2016, pimpl_2016)
  expect_equal(ufp_2016, pimpu_2016)

# test on the 2020 data

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)
  state_variables_2020 <- make_state_variables(stress_data_2020)

  fixed_points_2020 <- fixed_point_computation_function(mat = state_variables_2020, lb = 33,
                                                        data_idx = sovereign_bond_indices,
                                                        data_adv = average_daily_volume_sovereign,
                                                        base_year = 2019,
                                                        constant = 1.5
  )

  lfp_2020 <- fixed_points_2020 %>% dplyr::select(delta_lower) %>% unlist() %>% unname() %>% as.matrix()
  ufp_2020 <- fixed_points_2020 %>% dplyr::select(delta_upper) %>% unlist() %>% unname() %>% as.matrix()

  pimpl_2020 <- price_impact_function(del = ufp_2020, mat = state_variables_2020,
                                 lb = 33, data_idx = sovereign_bond_indices,
                                 data_adv = average_daily_volume_sovereign, base_year = 2019,
                                 constant = 1.5) %>% unname()

  pimpu_2020 <- price_impact_function(del = ufp_2020, mat = state_variables_2020,
                                 lb = 33, data_idx = sovereign_bond_indices,
                                 data_adv = average_daily_volume_sovereign,
                                 base_year = 2019, constant = 1.5) %>% unname()

  expect_equal(lfp_2020, pimpl_2020)
  expect_equal(ufp_2020, pimpu_2020)

})
