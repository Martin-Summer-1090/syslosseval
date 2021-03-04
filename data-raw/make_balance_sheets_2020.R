# This script assembles the EBA raw data to a balance-sheet for each bank using. Assumptions to be spelled out
# The raw data are provided by EBA for its spring 2020 transparency exercise
# on the EBA home page at:


# loading the necessary packages

library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)

# IRB exposures
# loading the data for credit risk exposures from TR_CR_2020.csv. These are data
# which are reported under internal rating based approach to credit risk (IRB). The
# description of this approach including the categorization of exposures can be found on the website of the BIS under:
# https://www.bis.org/basel_framework/chapter/CRE/30.htm.
# We filter the data according to the following query table as reported in the paper in appendix B EBA-data:
#
# \begin{tabular}{l r l}
# Variable  & Value   & Meaning\\ \hline
# Period    & 201012  & December 31 2019 \\
# Portfolio & 2       & IRB \\
# Item      & 2020502 & Original Exposure (IRB) \\
# Status    & 0     & No breakdown by status \\
# Exposure  & 103, 203 & Central governments or central banks, Institutions \\
# & 303, 404, & Corporates, Retail \\
# & 606, 607 & Equity , Other \\
# Perf\_status & 0 & No-breakdown by performance status \\ \hline
# \end{tabular}


# IRB exposures

exposures_irb <- read_csv("data-raw/TR_CR_2020.csv") %>%
  filter(
    Period == 201912, Item == 2020502, Portfolio == 2,
    Exposure %in% c(103, 203, 303, 404, 606, 607, 608), Status == 0, Perf_Status == 0, NACE_codes == 0
  )

# loading bank names

bank_names <- read_csv("data-raw/Bank_names_2020.csv") %>%
  rename(Bank_name = Name)

# Adding bank names to exposures and adjust variables names to the 2016 convention

exposures_irb_with_names <- left_join(exposures_irb, bank_names, by = "LEI_Code") %>%
  select(LEI_Code, NSA, Bank_name, Period, Country, Exposure, Amount) %>%
  rename(Country_code = NSA) %>%
  rename(LEI_code = LEI_Code) %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro")

# Observation 1: The IRB data seem to contain no entries for Exposure 607 and 608 (other items, securitisation).
# 607 does occur in STA but 608 does not occur in STA either.
# Observation 2: There are many redundant entries. For example for the exposure category
# County == 0 (all exposures or total) there are multiple or non unique records for 32 banks out of 128. These
# are specifically the LEI_codes 222100K6QL2V4MLHWQ08, 391200EEGLNXBBCVKC73, 3M5E1GQGKL17HI6CPN30, 529900GM944JT8YIRL63,
# 529900S9YO2JHTIIDG38, 5493008QOCP58OLEN998, 549300HFEHJOXGE4ZE63, 549300PPXHEU2JF0AM85, 635400AKJBGNS5WNQL34,
# 635400C8EK6DRI12LJ39, 724500A1FNICHSDF2I11, 7437003B5WFBOIEFY714, 7CUNS533WID6K7DGFI87, 815600AD83B2B6317788,
# 815600E4E6DCD2D25E30, 9695000CG7B84NLR5984, A5GWLFH3KM7YV2SFQL84, DG3RU1DBUFHT4ZF9WN62, H0YX5LBGKDVOWCXBZ594,
# J48C8PCSJVUBR8KCW529, J4CP7MHCXR8DAQMKIL78, JEUVK5RWVJEN8W0C9M24, JU1U6S0DG9YLT7N8ZV32, LIU16F6VZJSD6UKHD557,
# LSGM84136ACA92XCN876, M312WZV08Y7LYUC71685, MAES062Z21O4RZ2U7M96, N747OI7JINV7RUUH6190, NHBDILHZTYCNBV5UYZ31,
# PSNL19R2RXX5U3QWHI44, SI5RG2M0WQQLZCXKRM20, VWMYAEQSTOPNV0SUGU82. The source of these multiple records is
# unclear because according to the data files provided on the net the filter should select a unique set of records for
# each bank. This occurs only for the report about Country == 0. We clean the data by a heuristic which only
# reads the first five records for Country == 0 and discards the redundant records.

# Count the number of exposure categories

exp_vec_irb <- select(exposures_irb_with_names, Exposure) %>%
  unlist() %>%
  unique()

# Take exposures with Country == 0 and the rest:

exp_sub1_irb <- exposures_irb_with_names %>%
  filter(Country == 0)

exp_sub2_irb <- exposures_irb_with_names %>%
  filter(Country != 0)

aux_irb <- exp_sub1_irb %>%
  group_split(LEI_code, Country) %>%
  lapply(function(x) x[1:length(exp_vec_irb), ])

# reassemble list entries to a dataframe

aux_reassembled_irb <- do.call(bind_rows, aux_irb)

# reassemble with the rest of the dataframe:

exposures_irb_with_names_clean <- bind_rows(aux_reassembled_irb, exp_sub2_irb)

# STA exposures

exposures_sta <- read_csv("data-raw/TR_CR_2020.csv") %>%
  filter(
    Period == 201912, Item == 2020502, Portfolio == 1,
    Exposure %in% c(103, 104, 105, 106, 107, 203, 303, 404, 501, 601, 602, 603, 605, 606, 607, 608),
    Status == 0, Perf_Status == 0, NACE_codes == 0)

# Adding bank names to exposures and adjust variable names to the 2016 convention

exposures_sta_with_names <- left_join(exposures_sta, bank_names, by = "LEI_Code") %>%
  select(LEI_Code, NSA, Bank_name, Period, Country, Exposure, Amount) %>%
  rename(Country_code = NSA) %>%
  rename(LEI_code = LEI_Code) %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro")

# We have the same problem as with STA exposures. In the case country == 0 there are multiple records. We clean the data
# by selecting the first 14 entries (number of unique exposure categories for STA) in the same way as we did for IRB

# Count the number of exposure categories

exp_vec_sta <- select(exposures_sta_with_names, Exposure) %>%
  unlist() %>%
  unique()

# Take exposures with Country == 0 and the rest:

exp_sub1_sta <- exposures_sta_with_names %>%
  filter(Country == 0)

exp_sub2_sta <- exposures_sta_with_names %>%
  filter(Country != 0)

aux_sta <- exp_sub1_sta %>%
  group_split(LEI_code, Country) %>%
  lapply(function(x) x[1:length(exp_vec_sta), ])

# reassemble list entries to a dataframe

aux_reassembled_sta <- do.call(bind_rows, aux_sta)

# reassemble with the rest of the dataframe:

exposures_sta_with_names_clean <- bind_rows(aux_reassembled_sta, exp_sub2_sta)

# We read the bank exposure data that are independent of the IRB or STA framework: Common tier 1
# equity

common_equity_tier_1 <- read_csv("data-raw/TR_OTH_2020.csv") %>%
  filter(Period == 201912, Item == 2020143, ASSETS_FV == 0, ASSETS_Stages == 0, Fin_end_year == 0, n_quarters == 4) %>%
  select(LEI_Code, NSA, Period, Amount) %>%
  add_column(Country = "Total", .after = "Period") %>%
  add_column(Exposure = "Common tier1 equity capital", .after = "Country")



common_equity_tier_1_with_names <- left_join(bank_names, common_equity_tier_1, by = "LEI_Code") %>%
  select(LEI_Code, NSA, Bank_name, Period, Country, Exposure, Amount) %>%
  rename(Country_code = NSA) %>%
  rename(LEI_code = LEI_Code) %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro")


# Mapping of STA into the IRB scheme. We proceed as with the 2016 data:
# The IRB data and the STA data have to be added because we want to have the exposure data in a common balance sheet like aggregate
# structure. The problem is that the IRB and STA schemes use different exposure categories. We therefore map the exposure categories in
# STA into the IRB scheme using the following mapping:
# Map exposures ((103, 104, 105, 106, 107, 203), STA) into (103, IRB) (central banks and central government)
# Map exposures ((404, 501), STA) into (404, IRB) (retail)
# Map exposures ((601, 602, 603, 605, 607), STA) into (607, IRB) (other non-credit obligation assets). Note
# that 607 is not there in IRB so we need a full join here.
# In all the other cases institutions (203), corporates (303), equity (606), there is a one to one map between
# IRB and STA categories and amounts can be directly added up.

# We first merge the STA and IRB exposures by a left_join and replace the NA Amounts by 0.

exposures_total <- full_join(exposures_sta_with_names_clean, exposures_irb_with_names_clean,
                             by = c("LEI_code", "Country_code", "Bank_name", "Period", "Country", "Exposure", "Unit", "Currency")
) %>%
  mutate_all(~ replace(., is.na(.), 0))

cb_cg <- subset(exposures_total, Exposure %in% c(103, 104, 105, 106, 107)) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country, Unit, Currency) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  add_column(Exposure = 103, .before = "Amount")

rt <- subset(exposures_total, Exposure %in% c(404, 501)) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country, Unit, Currency) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  add_column(Exposure = 404, .before = "Amount")

o_nco_a <- subset(exposures_total, Exposure %in% c(601, 602, 603, 605, 607)) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country, Unit, Currency) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  add_column(Exposure = 607, .before = "Amount")

# Now aggregate the rest

exposures_rest <- filter(exposures_total, !(Exposure %in% c(103, 104, 105, 106, 107, 404, 501, 601, 602, 603, 605, 607))) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  select(-c("Amount.x", "Amount.y"))

# Now join the whole frame again:

exposures <- bind_rows(cb_cg, rt, o_nco_a, exposures_rest) %>%
  select(LEI_code, Country_code, Bank_name, Period, Country, Exposure, Amount, Unit, Currency) %>%
  ungroup() %>%
  arrange(LEI_code, Exposure)

# Total assets. Unlike the 2016 Transparency Exercise the new transparency exercise contains information on total assets. So
# this time we can read total assets directly from the EBA data. Note that for total assets we seem to lack the information
# for some banks. These banks are: Commbank Europe Ltd, RCB Bank Ltd, Deutsche Apotheker- und Ärztebank EG,
# Münchener Hypothekenbank EG, J.P. Morgan Bank Luxembourg S.A., C.R.H. - Caisse de refinancement de l'habitat,
# Nederlandse Waterschapsbank N.V., Banque centrale de compensation, Banque et Caisse d’Epargne de l’Etat, Luxembourg. The raw data
# contain a flaw, because the raw data assign two non existing codes as a LEI_code, namely 9.59800201400059E+019. We ignore these
# observations, because we cannot assign them.

total_assets <- read_csv("data-raw/TR_OTH_2020.csv") %>%
  filter(Period == 201912, Item == 2021010, ASSETS_FV == 0, ASSETS_Stages == 0, Fin_end_year == 0, n_quarters == 4) %>%
  select(LEI_Code, NSA, Period, Amount) %>%
  rename(LEI_code = LEI_Code) %>%
  rename(Country_code = NSA) %>%
  add_column(Country = "Total", .after = "Period") %>%
  add_column(Exposure = "Total assets", .after = "Country")

total_assets_with_gaps <- left_join(common_equity_tier_1_with_names, total_assets, by = "LEI_code") %>%
  select(LEI_code, Country_code.x, Bank_name, Period.x, Country.x, Exposure.y, Amount.y, Unit, Currency) %>%
  rename(Country_code = Country_code.x, Period = Period.x, Country = Country.x, Exposure = Exposure.y, Amount = Amount.y)

total_assets_with_gaps$Exposure[is.na(total_assets_with_gaps$Exposure)] <- "Total assets"

# We now see whether we can fill the gaps manually by searching the numbers from the annual reports published on the web

total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "213800EUDXECGWMKKR98"] <- 418 # https://thebanks.eu/banks/16343
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "253400EBCBBVB9TUHN50"] <- 4705.26 # https://thebanks.eu/banks/10900/financials
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "5299007S3UH5RKUYDA52"] <- 49785 # https://thebanks.eu/banks/11226/financials
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "529900GM944JT8YIRL63"] <- 40391.23 # https://thebanks.eu/banks/11514/financials
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "7W1GMC6J4KGLBBUSYP52"] <- 49488.18 # https://thebanks.eu/banks/16261/financials
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "969500TVVZM86W7W5I94"] <- 26.290 # SNL database query 2.12.2020
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "JLP5FSPH9WPSHY3NIM24"] <- 96205.00 # https://thebanks.eu/banks/16386/financials
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "R1IO4YJ0O79SMWVCHB58"] <- 553380 # https://thebanks.eu/banks/13875/financials
total_assets_with_gaps$Amount[total_assets_with_gaps$LEI_code == "R7CQUF1DQM73HUTV1078"] <- 48063.16 # https://thebanks.eu/banks/16217/financials

total_assets_2020 <- total_assets_with_gaps

# To make the data-frames more readable we will substitute some codes with a more informative description. In order to achieve this
# we load some lookup tables which will help us in this effort. In particular we use the actual terms for the
# Country and Exposure Variable. We try to replace these codes by the actual names, for example use the ISO-code for a country
# instead of for example 9 and use central banks and central governments instead of 1100. This is also applied to the impairment data.

Lookup_exposures <- read_csv("data-raw/Lookup_table_exposures_2020.csv")


Lookup_countries <- read_csv("data-raw/Lookup_table_countries.csv") %>%
  rename(Name = Label)

Lookup_ISO <- read_csv("data-raw/Lookup_table_ISO.csv")


exposures_plain <- left_join(exposures, Lookup_exposures, by = "Exposure") %>%
  left_join(Lookup_countries, by = "Country") %>%
  left_join(Lookup_ISO, by = "Name")

# The EBA data contain exposures to regions or aggregates for which we have no ISO code. We replace these by the plain descriptors

exposures_plain$Code[exposures_plain$Name == "Total / No breakdown"] <- "Total"
exposures_plain$Code[exposures_plain$Name == "Africa"] <- "Africa"
exposures_plain$Code[exposures_plain$Name == "International organisations"] <- "International_organisations"
exposures_plain$Code[exposures_plain$Name == "Other advanced non EEA"] <- "Other_advanced_non_EEA"
exposures_plain$Code[exposures_plain$Name == "Other CEE non EEA"] <- "Other_CEE_non_EEA"
exposures_plain$Code[exposures_plain$Name == "Other"] <- "Other"

# Now we rearrange columns and drop columns not needed anymore. We add two additional variables so we can understand from the dataframe directly
# the unit and currency of the amounts reported. We have three banks for which we have missing exposures. We drop these records. They are
# LEI_codes: 95980020140005881190, PSNL19R2RXX5U3QWHI44, 213800EUDXECGWMKKR98, 549300XFX12G42QIKN82, xxxxxxxxxxxxxxxxxxxx
# We also drop the unasignable bank 9.59800201400059E+019

exposures_final <- select(exposures_plain, LEI_code, Country_code, Bank_name, Period, Code, Label, Amount) %>%
  rename(Exposure = Label) %>%
  rename(Country = Code) %>%
  add_column(Unit = "Millions") %>%
  add_column(Currency = "Euro") %>%
  bind_rows(total_assets_2020) %>%
  bind_rows(common_equity_tier_1_with_names) %>%
  select(LEI_code, Country_code, Bank_name, Period, Country, Exposure, Amount, Unit, Currency) %>%
  filter(!(LEI_code %in% c(
    "95980020140005881190", "PSNL19R2RXX5U3QWHI44", "213800EUDXECGWMKKR98",
    "549300XFX12G42QIKN82", "XXXXXXXXXXXXXXXXXXXX", "xxxxxxxxxxxxxxxxxxxx",
    "9.59800201400059E+019"
  )))

# We now attribute the sovereign bond exposures. Unlike in the 2016 data we have now a more precise and transparent dataset we can make
# use of. Here we take Item 2020811 Direct exposures - On balance sheet - Total carrying amount of
# non-derivative financial assets (net of short positions). We drop the same institutions as in exposures

sovereign_exposures <- read_csv("data-raw/TR_SOV_2020.csv") %>%
  filter(Item == 2020811, Maturity == 8, Accounting_portfolio == 0) %>%
  filter(!(LEI_Code %in% c(
    "95980020140005881190", "PSNL19R2RXX5U3QWHI44", "213800EUDXECGWMKKR98",
    "549300XFX12G42QIKN82", "XXXXXXXXXXXXXXXXXXXX", "xxxxxxxxxxxxxxxxxxxx",
    "9.59800201400059E+019"
  )))

# Add bank names

sovereign_exposures_with_names <- left_join(sovereign_exposures, bank_names, by = "LEI_Code") %>%
  select(LEI_Code, NSA, Bank_name, Period, Country, Amount) %>%
  rename(Country_code = NSA) %>%
  rename(LEI_code = LEI_Code)

# replace numerical country code by ISO or description

sovereign_exposures_plain <- left_join(sovereign_exposures_with_names, Lookup_countries, by = "Country") %>%
  left_join(Lookup_ISO, by = "Name")

sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Total / No breakdown"] <- "Total"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Africa"] <- "Africa"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "International organisations"] <- "International_organisations"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Other advanced non EEA"] <- "Other_advanced_non_EEA"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Other CEE non EEA"] <- "Other_CEE_non_EEA"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Other"] <- "Other"

# clean up:

sovereign_exposures_final <- select(sovereign_exposures_plain, LEI_code, Country_code, Bank_name, Period, Code, Amount) %>%
  rename(Country = Code) %>%
  add_column(Unit = "Millions") %>%
  add_column(Currency = "Euro") %>%
  add_column(Exposure = "Central banks and central governments", .after = "Country")

# The sovereign exposure file contains no position for the total exposure. We create the totals from the country and region exposures

sovereign_exposures_final_total <- sovereign_exposures_final %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Exposure, Unit, Currency) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  select(LEI_code, Country_code, Bank_name, Period, Exposure, Amount, Unit, Currency) %>%
  add_column(Country = "Total", .after = "Period")

sovereign_exposures_final_all <- bind_rows(sovereign_exposures_final, sovereign_exposures_final_total) %>%
  arrange(LEI_code)

# Now we would like to match these data with the exposure data


matched_data <- left_join(exposures_final, sovereign_exposures_final_all,
                          by = c("LEI_code", "Country_code", "Bank_name", "Period", "Country", "Exposure", "Unit", "Currency")
) %>%
  select(LEI_code, Country_code, Bank_name, Period, Country, Exposure, Amount.x, Amount.y, Unit, Currency) %>%
  mutate_all(~ replace(., is.na(.), 0))

# We have the same data problem as with the 2016 data. In 20 % of cases the bond exposures exceed the total exposures. We
# apply the following heuristics. If the exposure reported exposure is smaller than the bond exposure we assign the entire
# exposure as a bond exposure. If the reported exposure is smaller than the bond exposure we assign the difference as a loan exposure
# and the bond exposure as bond exposure

matched_data_corr <- matched_data %>%
  mutate(Loan_Amount = if_else(Amount.x > Amount.y, (Amount.x - Amount.y), 0)) %>%
  mutate(Bond_Amount = if_else(Amount.x > Amount.y, Amount.y, Amount.x)) %>%
  mutate(Total_Amount = Loan_Amount + Bond_Amount) %>%
  select(
    LEI_code, Country_code, Bank_name, Period, Country, Exposure,
    Loan_Amount, Bond_Amount, Total_Amount, Unit, Currency
  )

# write the data to a R format

eba_exposures_2020 <- matched_data_corr

usethis::use_data(eba_exposures_2020, overwrite = TRUE)
