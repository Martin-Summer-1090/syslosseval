test_that("bank behavior function returns fire sale shares in the correct order of banks", {

  library(syslosseval)

# 2016 data

  stress_data_2016 <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
  matrices_2016 <- make_state_variables(stress_data_2016)
  delta_2016 <- rep(0, dim(matrices_2016$S_0)[2])

  fs_shares_2016 <- bank_behavior_function(delta_2016, matrices_2016, 33)

  expect_equal(rownames(matrices_2016$S_0), rownames(fs_shares_2016))
  expect_equal(rownames(matrices_2016$L_0), rownames(fs_shares_2016))
  expect_equal(rownames(matrices_2016$e_0), rownames(fs_shares_2016))
  expect_equal(rownames(matrices_2016$S_1), rownames(fs_shares_2016))
  expect_equal(rownames(matrices_2016$L_1), rownames(fs_shares_2016))
  expect_equal(rownames(matrices_2016$e_1), rownames(fs_shares_2016))

# Is the output between 0 and 1 ?

  expect_equal(all(fs_shares_2016 >= 0), T)
  expect_equal(all(fs_shares_2016 <= 1), T)

# 2020 data

  stress_data_2020 <- make_stress_data(eba_exposures_2020, eba_impairments_2020, 1, 2019)
  matrices_2020 <- make_state_variables(stress_data_2020)
  delta_2020 <- rep(0, dim(matrices_2020$S_0)[2])

  fs_shares_2020 <- bank_behavior_function(delta_2020, matrices_2020, 33)

  expect_equal(rownames(matrices_2020$S_0), rownames(fs_shares_2020))
  expect_equal(rownames(matrices_2020$L_0), rownames(fs_shares_2020))
  expect_equal(rownames(matrices_2020$e_0), rownames(fs_shares_2020))
  expect_equal(rownames(matrices_2020$S_1), rownames(fs_shares_2020))
  expect_equal(rownames(matrices_2020$L_1), rownames(fs_shares_2020))
  expect_equal(rownames(matrices_2020$e_1), rownames(fs_shares_2020))

  # Is the output between 0 and 1 ?

  expect_equal(all(fs_shares_2020 >= 0), T)
  expect_equal(all(fs_shares_2020 <= 1), T)

})
