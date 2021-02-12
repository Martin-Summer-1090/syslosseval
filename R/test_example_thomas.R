# This is a test-script for checking the fixed point computation using the example by Thomas. In the paper the
# example is in A3

# Input data

S_0 <- matrix(c(145, 435, 300), nrow = 3, ncol = 1)
rownames(S_0) <- c("Bank1", "Bank2", "Bank3")
colnames(S_0) <- "Bond"

L_0 <- matrix(c(1352, 1364, 1330), nrow = 3, ncol = 1)
rownames(S_0) <- c("Bank1", "Bank2", "Bank3")
colnames(S_0) <- "Loan"

e_0 <- matrix(c(210, 199, 205), nrow = 3, ncol = 1)
rownames(e_0) <- c("Bank1", "Bank2", "Bank3")
colnames(e_0) <- "CET1"

L_1 <- matrix(c(1170, 1245, 1190), nrow = 3, ncol = 1)
rownames(L_1) <- c("Bank1", "Bank2", "Bank3")
colnames(L_1) <- "Loan"

S_1 <- matrix(c(145, 435, 300), nrow = 3, ncol = 1)
rownames(S_1) <- c("Bank1", "Bank2", "Bank3")
colnames(S_1) <- "Bond"

e_1 <- matrix(c(28, 80, 65), nrow = 3, ncol = 1)
rownames(e_1) <- c("Bank1", "Bank2", "Bank3")
colnames(e_1) <- "CET1"

theta <-  matrix(c(0,0,0), nrow = 3, ncol = 1)
rownames(theta) <- c("Bank1", "Bank2", "Bank3")
colnames(theta) <- "theta"

example_multiple_equilibria <- list(e_0 = e_0, S_0 = S_0, L_0 = L_0, e_1 = e_1, S_1 = S_1, L_1 = L_1, theta = theta)
#usethis::use_data(example_multiple_equilibria, overwrite = T)

adv_example <- tibble::tibble(Country = "Thomas", Year = 2020, Volume = 45454.55, Unit = "Million", Currency = "Euro")
#usethis::use_data(adv_example, overwrite = T)

sov_bond_index_example <- tibble::tibble(Country = c("Thomas", "Thomas", "Thomas"),
                                         Date = c("2020-01-01", "2020-01-02", "2020-01-03"),
                                         Value = c(1,3,2))
#usethis::use_data(sov_bond_index_example, overwrite = T)


delta1 <- 0.0564801
delta2 <- 0.114085
delta3 <- 0.13914

lambda_bar <- 44

# test bank behavior function

out1 <- bank_behavior_function(delta1, example_multiple_equilibria, lambda_bar)
out2 <- bank_behavior_function(delta2, example_multiple_equilibria, lambda_bar)
out3 <- bank_behavior_function(delta3, example_multiple_equilibria, lambda_bar)

# test the price impact function

imp1 <- price_impact_function(delta1, example_multiple_equilibria, 44, sov_bond_index_example, adv_example, 2020, 0.9433962)
imp2 <- price_impact_function(delta2, example_multiple_equilibria, 44, sov_bond_index_example, adv_example, 2020, 0.9433962)
imp3 <- price_impact_function(delta3, example_multiple_equilibria, 44, sov_bond_index_example, adv_example, 2020, 0.9433962)
