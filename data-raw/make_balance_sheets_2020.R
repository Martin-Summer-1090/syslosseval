# This script assembles the EBA raw data to a balance-sheet for each bank using. Assumptions to be spelled out
# The raw data are provided by EBA for its spring 2020 transparency exercise
# on the EBA home page at:


# loading the necessary packages

library(readr)
library(tibble)
library(dplyr)
library(tidyr)

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

exposures_irb <- read_csv("data-raw/TR_CR_2020.csv") %>%
  filter(
    Period == 201912, Item == 2020502, Portfolio == 2,
    Exposure %in% c(103, 203, 303, 404, 606, 607), Status == 0, Perf_Status == 0, NACE_codes == 0
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

# There seem to be problems with the data. 32 banks report redundant exposures for the total exposures,
# in the sense that they report multiple entries for each exposure
# 103, 203, 303, 404, 606. Exposure 607 seems absent from the irb data. This can be conveniently dealt with by aggregating since the redundant
# exposures report an amount of 0. There are two exceptions: LEI_code == "724500A1FNICHSDF2I11" and LEI_code == "DG3RU1DBUFHT4ZF9WN62".
# We correct for this first:

exposures_irb_with_names_corrected <- exposures_irb_with_names %>%
  filter(!(LEI_code %in% c("724500A1FNICHSDF2I11", "DG3RU1DBUFHT4ZF9WN62"))) %>%
  group_by(LEI_code, Country_code, Bank_name, Country, Period, Exposure, Unit, Currency) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  select(LEI_code, Country_code, Bank_name, Country, Period, Exposure, Amount, Unit, Currency)


volksb <- exposures_irb_with_names %>%
  filter(LEI_code == "724500A1FNICHSDF2I11") %>%
  slice(1:5)

rabob <- exposures_irb_with_names %>%
  filter(LEI_code == "DG3RU1DBUFHT4ZF9WN62") %>%
  slice(1:5)

exposures_irb_with_names_corrected_total <- bind_rows(exposures_irb_with_names_corrected, volksb, rabob)

# STA exposures

exposures_sta <- read_csv("data-raw/TR_CR_2020.csv") %>%
  filter(
    Period == 201912, Item == 2020502, Portfolio == 1,
    Exposure %in% c(103, 104, 105, 106, 107, 203, 303, 404, 501, 601, 602, 603, 605, 606, 607), Status == 0, Perf_Status == 0, NACE_codes == 0
  )

# Adding bank names to exposures and adjust variable names to the 2016 convention

exposures_sta_with_names <- left_join(exposures_sta, bank_names, by = "LEI_Code") %>%
  select(LEI_Code, NSA, Bank_name, Period, Country, Exposure, Amount) %>%
  rename(Country_code = NSA) %>%
  rename(LEI_code = LEI_Code) %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro")

# It also looks like there are redundant entries like in irb. Here the exceptional banks are
# LEI_code == "529900GGYMNGRQTDOO93", LEI_code == "724500A1FNICHSDF2I11", LEI_code == "DG3RU1DBUFHT4ZF9WN62". We proceed in the same
# way as with the irb exposures

exposures_sta_with_names_corrected <- exposures_sta_with_names %>%
  filter(!(LEI_code %in% c("529900GGYMNGRQTDOO93", "724500A1FNICHSDF2I11", "DG3RU1DBUFHT4ZF9WN62"))) %>%
  group_by(LEI_code, Country_code, Bank_name, Country, Period, Exposure, Unit, Currency) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  select(LEI_code, Country_code, Bank_name, Country, Period, Exposure, Amount, Unit, Currency)

exc1 <- exposures_sta_with_names %>%
  filter(LEI_code == "529900GGYMNGRQTDOO93") %>%
  slice(1:14)

exc2 <- exposures_sta_with_names %>%
  filter(LEI_code == "724500A1FNICHSDF2I11") %>%
  slice(1:14)

exc3 <- exposures_sta_with_names %>%
  filter(LEI_code == "DG3RU1DBUFHT4ZF9WN62") %>%
  slice(1:14)

exposures_sta_with_names_corrected_total <- bind_rows(exposures_sta_with_names_corrected, exc1, exc2, exc3)


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
# Map exposures ((601, 602, 603, 605, 607), STA) into (607, IRB) (other non-credit obligation assets)
# In all the other cases institutions (203), corporates (303), equity (606), there is a one to one map between
# IRB and STA categories and amounts can be directly added up.

# We first merge the STA and IRB exposures by a left_join and replace the NA Amounts by 0.

exposures_total <- left_join(exposures_sta_with_names_corrected_total, exposures_irb_with_names_corrected_total,
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
  ungroup()

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
  add_column(Exposure = "Central governments or central banks", .after = "Country")

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
  by = c("LEI_code", "Country_code", "Bank_name", "Period", "Exposure", "Country", "Unit", "Currency")
) %>%
  select(LEI_code, Country_code, Bank_name, Period, Exposure, Country, Amount.x, Amount.y, Unit, Currency) %>%
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
    LEI_code, Country_code, Bank_name, Period, Exposure, Country,
    Loan_Amount, Bond_Amount, Total_Amount, Unit, Currency
  )



# write the data to a R format

eba_exposures_2020 <- matched_data_corr

usethis::use_data(eba_exposures_2020, overwrite = TRUE)
