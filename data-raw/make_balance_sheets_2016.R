# This script assembles the EBA raw data to a balance-sheet for each bank using the assumptions which are
# described in our paper in appendix B EBA-data. The raw data are provided by EBA for its 2016 stress test
# on the EBA home page at: https://eba.europa.eu/risk-analysis-and-data/eu-wide-stress-testing/2016


# loading the necessary packages

library(readr)
library(tibble)
library(dplyr)
library(tidyr)

# load the exposures from TR_CR_2016.csv data file, which are reported under internal rating based approach to credit risk (IRB). The
# description of this approach including the categorization of exposures can be found on the website of the BIS under:
# https://www.bis.org/basel_framework/chapter/CRE/30.htm.
# We filter the data according to the following query table as reported in the paper in appendix B EBA-data:
#
# \begin{tabular}{l r l}
# Variable  & Value   & Meaning\\ \hline
# Period    & 201512  & December 31 2015 \\
# Portfolio & 3,4     & Foundation IRB (F-IRB), Advances IRB (A-IRB) \\
# Item      & 1690201 & Exposure values (IRB) \\
# Scenario  & 1       & Actual Figures \\
# Status    & 1,2     & Non defaulted assets, defaulted assets \\
# Exposure  & 1100, 2000, & Central banks and government, Institutions \\
# & 3000, 4000, & Corporates, Retail \\
# & 6100, 6200, 6300 & Equity, Securitisation, Other \\
# Perf\_status & 0 & No-breakdown by performance status \\ \hline
# \end{tabular}

# IRB exposures and IRB impairments


# We aggregate exposure values across F-IRB and A-IRB, across non defaulted assets and defaulted assets for each bank and each country
# to which this bank holds exposures and for each exposure category of IRB. There is an issue with the bank names.
# The bank names are encoded in Latin1 instead of UTF-8. This encoding
# is corrected by using the inconv() function from the base package, so we have a correct display of all bank names and a uniform UTF-8 encoding
# of characters.

exposures_irb <- read_csv("data-raw/TR_CR_2016.csv") %>%
  filter(Period == 201512, Portfolio %in% c(3, 4), Item == 1690201,
         Scenario == 1, Status %in% c(1, 2), Exposure %in% c(1100, 2000, 3000, 4000, 6100, 6200, 6300),
         Perf_Status == 0) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country, Exposure) %>%
  summarise(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(across(Bank_name, ~ iconv(., "Latin1", "UTF-8")))

# We retrieve the impairment data associated with the IRB exposures and aggregate them to the same
# aggregation level as the
# exposure data. The EBA stress test assumes a 3 year horizon and thus impairment rates are projections for
# December 2016, 2017 and 2018. We
# retrieve the values at all future horizons. Unlike in the exposure data the impairment
# data are not broken down by F-IRB and A-IRB. They are also not broken down by Status. The
# rates are retrieved for the baseline-scenario as well as for the stress scenario for each of the
# years 2016, 2017 and 2018.
# The EBA data file contains NA, where no values are reported. These NA values are replaced by an
# impairment rate value of 0.
# There seems to be data recording mistakes for some of the banks where we see multiple redundant records.
# Specifically this seems to be the case for LEI_codes:
#  "3M5E1GQGKL17HI6CPN30", "3U8WV1YX2VMUHH7Z1Q21", "529900GGYMNGRQTDOO93", "529900W3MOO00A18X956", "5493006P8PDBI8LC0O96", "549300PPXHEU2JF0AM85",
#  "549300TJUHHEE8YXKI59", "7437003B5WFBOIEFY714", "80H66LPTVDLM0P28XF25", "81560097964CBDAED282", "959800DQQUAMV0K08004", "9695000CG7B84NLR5984",
#  "96950066U5XAAIRCPA78", "A5GWLFH3KM7YV2SFQL84", "J4CP7MHCXR8DAQMKIL78", "LIU16F6VZJSD6UKHD557", "M312WZV08Y7LYUC71685", "NHBDILHZTYCNBV5UYZ31",
#  "P4GTT6GF1W40CVIMFR43" "Q2GQA2KF6XJ24W42G291" "SI5RG2M0WQQLZCXKRM20" "VDYMYTQGZZ6DU0912C88".
# It seems that for this bank for each period and each scenario and for each bank there are several
# redundant records with an empty impairment rate and the exposure 6200 with a recurring impairment rate 0. We try
# to clean the data accordingly to get a unique representation of impairment records.

# read the data first as recorded in the raw data file:

impairments_irb_prelim <- read_csv("data-raw/TR_CR_2016.csv") %>%
  filter(Period %in% c(201612, 201712, 201812), Portfolio == 2, Item == 1690205, Scenario %in% c(2, 3),
         Status == 0, Exposure %in% c(1100, 2000, 3000, 4000, 6100, 6200, 6300),
         Perf_Status == 0) %>%
  select(-Country_rank)

exp_vec <- select(impairments_irb_prelim, Exposure) %>% unlist() %>% unique()

# For every bank and every country in the baseline as well as in the stress scenario take the first 7 (because this
# is  the length of  the actual exposure selection) and discard the rest for the 2016 data.

aux_2016 <- filter(impairments_irb_prelim, Period == 201612) %>%
  group_split(LEI_code, Country, Scenario) %>%
  lapply(function(x) x[1:length(exp_vec), ])

# reassemble the split list into one dataframe again and replace the NA recors of the impairment rate by 0:

impairments_irb_2016 <- do.call(bind_rows, aux_2016) %>%
  mutate_all(~ replace(., is.na(.), 0))

# For every bank and every country in the baseline as well as in the stress scenario take the first 7 (because this
# is  the length of  the actual exposure selection) and discard the rest for the 2017 data.

aux_2017 <- filter(impairments_irb_prelim, Period == 201712)%>%
  group_split(LEI_code, Country, Scenario) %>%
  lapply(function(x) x[1:length(exp_vec), ])

# reassemble the split list into one dataframe again and replace the NA recors of the impairment rate by 0:

impairments_irb_2017 <- do.call(bind_rows, aux_2017) %>%
  mutate_all(~ replace(., is.na(.), 0))

# For every bank and every country in the baseline as well as in the stress scenario take the first 7 (because this
# is  the length of  the actual exposure selection) and discard the rest for the 2018 data.

aux_2018 <- filter(impairments_irb_prelim, Period == 201812)%>%
  group_split(LEI_code, Country, Scenario) %>%
  lapply(function(x) x[1:length(exp_vec), ])

# reassemble the split list into one dataframe again and replace the NA recors of the impairment rate by 0:

impairments_irb_2018 <- do.call(bind_rows, aux_2018) %>%
  mutate_all(~ replace(., is.na(.), 0))

# reassemble dataframe for all dates:

impairments_irb <- bind_rows(impairments_irb_2016, impairments_irb_2017, impairments_irb_2018) %>%
  select(LEI_code, Country_code, Bank_name, Period, Scenario, Country, Exposure, Amount) %>%
  rename(Impairment_rate = Amount) %>%
  mutate(across(Bank_name, ~ iconv(., "Latin1", "UTF-8")))

# STA exposures and STA impairments

# We read first all STA exposures

exposures_sta <- read_csv("data-raw/TR_CR_2016.csv") %>%
  filter(
    Period == 201512, Portfolio == 1, Item == 1690301, Scenario == 1, Status %in% c(1, 2),
    Exposure %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 2000, 3000, 4000, 5000, 6400, 6500, 6600, 6700, 6100, 6200, 6300),
    Perf_Status == 0
  ) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country, Exposure) %>%
  summarise(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(across(Bank_name, ~ iconv(., "Latin1", "UTF-8")))

# We next read all the STA impairments corresponding to these exposures. Here we also seem to see issues spurious records as in the
# IRB case.

impairments_sta_prelim <- read_csv("data-raw/TR_CR_2016.csv") %>%
  filter(
    Period %in% c(201612, 201712, 201812), Portfolio == 1, Item == 1690305, Scenario %in% c(2, 3), Status == 0,
    Exposure %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 2000, 3000, 4000, 5000, 6400, 6500, 6600, 6700, 6100, 6200, 6300), Perf_Status == 0
  ) %>%
  select(-Country_rank)

# There seems to exist the same recording bug as in the case of IRB data. We therefore correct in the same way

exp_vec_sta <- select(impairments_sta_prelim, Exposure) %>% unlist() %>% unique()

# For every bank and every country in the baseline as well as in the stress scenario take the first 7 (because this
# is  the length of  the actual exposure selection) and discard the rest for the 2016 data.

aux_2016_sta <- filter(impairments_sta_prelim, Period == 201612) %>%
  group_split(LEI_code, Country, Scenario) %>%
  lapply(function(x) x[1:length(exp_vec_sta), ])

# reassemble the split list into one dataframe again and replace the NA recors of the impairment rate by 0:

impairments_sta_2016 <- do.call(bind_rows, aux_2016_sta) %>%
  mutate_all(~ replace(., is.na(.), 0))

# For every bank and every country in the baseline as well as in the stress scenario take the first 7 (because this
# is  the length of  the actual exposure selection) and discard the rest for the 2017 data.

aux_2017_sta <- filter(impairments_sta_prelim, Period == 201712)%>%
  group_split(LEI_code, Country, Scenario) %>%
  lapply(function(x) x[1:length(exp_vec_sta), ])

# reassemble the split list into one dataframe again and replace the NA recors of the impairment rate by 0:

impairments_sta_2017 <- do.call(bind_rows, aux_2017_sta) %>%
  mutate_all(~ replace(., is.na(.), 0))

# For every bank and every country in the baseline as well as in the stress scenario take the first 7 (because this
# is  the length of  the actual exposure selection) and discard the rest for the 2018 data.

aux_2018_sta <- filter(impairments_sta_prelim, Period == 201812)%>%
  group_split(LEI_code, Country, Scenario) %>%
  lapply(function(x) x[1:length(exp_vec_sta), ])

# reassemble the split list into one dataframe again and replace the NA recors of the impairment rate by 0:

impairments_sta_2018 <- do.call(bind_rows, aux_2018_sta) %>%
  mutate_all(~ replace(., is.na(.), 0))

# reassemble dataframe for all dates:

impairments_sta <- bind_rows(impairments_sta_2016, impairments_sta_2017, impairments_sta_2018) %>%
  select(LEI_code, Country_code, Bank_name, Period, Scenario, Country, Exposure, Amount) %>%
  rename(Impairment_rate = Amount) %>%
  mutate(across(Bank_name, ~ iconv(., "Latin1", "UTF-8")))

# We read the bank exposure data that are independent of the IRB or STA framework: Common tier 1
# equity

common_equity_tier_1 <- read_csv("data-raw/TR_OTH_2016.csv") %>%
  filter(Period == 201512, Item == 1690106, Scenario == 1) %>%
  add_column(Country = "Total", .after = "Period") %>%
  add_column(Exposure = "Common tier1 equity capital", .after = "Country") %>%
  mutate(across(Bank_name, ~ iconv(., "Latin1", "UTF-8"))) %>%
  select(-c("index", "Item", "Scenario")) %>%
  add_column(Unit = "Million", .after = "Amount") %>%
  add_column(Currency = "Euro", .after = "Unit")


# The IRB data and the STA data have to be added because we want to have the exposure data in a common balance sheet like aggregate
# structure. The problem is that the IRB and STA schemes use different exposure categories. We therefore map the exposure categories in
# STA into the IRB scheme using the following mapping:
# Map exposures ((1100,1200,1300,1400,1500,1600,1700), STA) into (1100, IRB) (central banks and central government)
# Map exposures ((4000, 5000), STA) into (4000, IRB) (retail)
# Map exposures ((6300, 6400, 6500, 6600, 6700), STA) into (6300, IRB) (other non-credit obligation assets)
# In all the other cases institutions (2000), corporates (3000), equity (6100), securitization (6200) there is a one to one map between
# IRB and STA categories and amounts can be directly added up.

# We first merge the STA and IRB exposures by a left_join and replace the NA Amounts by 0.

exposures_total <- left_join(exposures_sta, exposures_irb, by = c("LEI_code", "Country_code", "Bank_name", "Period", "Country", "Exposure")) %>%
  mutate_all(~ replace(., is.na(.), 0))

# Map STA into IRB for the subsets where this is necessary

cb_cg <- subset(exposures_total, Exposure %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700)) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  add_column(Exposure = 1100, .before = "Amount")

rt <- subset(exposures_total, Exposure %in% c(4000, 5000)) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  add_column(Exposure = 4000, .before = "Amount")

o_nco_a <- subset(exposures_total, Exposure %in% c(6300, 6400, 6500, 6600, 6700)) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country) %>%
  summarize(Amount = sum(Amount, na.rm = T)) %>%
  add_column(Exposure = 6300, .before = "Amount")

# Now aggregate the rest

exposures_rest <- filter(exposures_total, !(Exposure %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 4000, 5000, 6300, 6400, 6500, 6600, 6700))) %>%
  mutate(Amount = Amount.x + Amount.y) %>%
  select(-c("Amount.x", "Amount.y"))

# Now join the whole frame again:

exposures <- bind_rows(cb_cg, rt, o_nco_a, exposures_rest) %>%
  ungroup()

# We now do the same thing for impairments as we did for exposures.


# combine sta and irb impairments in one dataframe

impairments_total <- left_join(impairments_sta, impairments_irb,
                               by = c("LEI_code", "Country_code", "Bank_name", "Period",
                                      "Scenario", "Country", "Exposure")) %>%
  mutate_all(~ replace(., is.na(.), 0))

# Now map all impairment rates into the IRB scheme by taking averages across the subcategories (since the impairments
# are a rate we should perhaps take exposure weighted averages. We can do this later.)

# exposures for the subcategories:

cb_cg_imp <- subset(impairments_total, Exposure %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700)) %>%
  mutate(Impairment_rate = Impairment_rate.x + Impairment_rate.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Scenario, Country) %>%
  summarize(Impairment_rate = mean(Impairment_rate, na.rm = T)) %>%
  add_column(Exposure = 1100, .before = "Impairment_rate")

rt_imp <- subset(impairments_total, Exposure %in% c(4000, 5000)) %>%
  mutate(Impairment_rate = Impairment_rate.x + Impairment_rate.y)%>%
  group_by(LEI_code, Country_code, Bank_name, Period, Scenario, Country) %>%
  summarize(Impairment_rate = mean(Impairment_rate, na.rm = T)) %>%
  add_column(Exposure = 4000, .before = "Impairment_rate")

o_nco_a_imp <- subset(impairments_total, Exposure %in% c(6300, 6400, 6500, 6600, 6700)) %>%
  mutate(Impairment_rate = Impairment_rate.x + Impairment_rate.y) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Scenario, Country) %>%
  summarize(Impairment_rate = mean(Impairment_rate, na.rm = T)) %>%
  add_column(Exposure = 6300, .before = "Impairment_rate")

# rest of impairment rates:

impairments_rest <- filter(impairments_total, !(Exposure %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 4000, 5000, 6300, 6400, 6500, 6600, 6700))) %>%
  rowwise() %>%
  mutate(Impairment_rate = Impairment_rate.x + Impairment_rate.y) %>%
  select(-c("Impairment_rate.x", "Impairment_rate.y"))

# stich all together (there are two entries with negative impairment rates. We set these impairment rates to zero)

impairments <- bind_rows(cb_cg_imp, rt_imp, o_nco_a_imp, impairments_rest) %>%
  mutate(Impairment_rate = if_else(Impairment_rate < 0, 0, Impairment_rate)) %>%
  ungroup()


# The EBA data contain no information on total assets. We do need this information to build balance sheets and compute
# leverage ratios. We add this information which we have collected by hand from the annual reports of the banks in our
# sample. Since the annual reports are published on the internet we do not include the pdfs of there reports in the data-raw
# directory. If you have access to a database like SNL you can also retrieve this information from there. Our experience is
# that SNL contains mistakes and the numbers have to be double checked. The numbers we use here have been collected from SNL,
# double checked and when necessary corrected by hand and written into a csv. We read this csv here. The numbers are the
# value of total assets for the given banks at 31.12.2015 in millions of Euro.

total_assets <- read_csv("data-raw/Total_assets.csv") %>%
  add_column(Unit = "Millions") %>%
  add_column(Currency = "Euro") %>%
  add_column(Period = 201512, .after = "Bank_name") %>%
  rename(Amount = Amount_sum) %>%
  select(LEI_code, Country_code, Bank_name, Period, Country, Exposure, Amount, Unit, Currency)

total_assets$Country[total_assets$Country == 0] <- "Total"
total_assets$Exposure[total_assets$Exposure == "total_assets"] <- "Total assets"

# To make the data-frames more readable we will substitute some codes with a more informative description. In order to achieve this
# we load some lookup tables which will help us in this effort. In particular we use the actual terms for the
# Country and Exposure Variable. We try to replace these codes by the actual names, for example use the ISO-code for a country
# instead of for example 9 and use central banks and central governments instead of 1100. This is also applied to the impairment data.

Lookup_exposures <- read_csv("data-raw/Lookup_table_exposures.csv") %>%
  rename(Asset_classes = "Asset classes")

Lookup_countries <- read_csv("data-raw/Lookup_table_countries.csv") %>%
  rename(Name = Label)

Lookup_ISO <- read_csv("data-raw/Lookup_table_ISO.csv")

Lookup_scenario <- read_csv("data-raw/Lookup_table_scenarios.csv")



exposures_plain <- left_join(exposures, Lookup_exposures, by = "Exposure") %>%
  left_join(Lookup_countries, by = "Country") %>%
  left_join(Lookup_ISO, by = "Name")

impairments_plain <- left_join(impairments, Lookup_exposures, by = "Exposure") %>%
  left_join(Lookup_countries, by = "Country") %>%
  left_join(Lookup_ISO, by = "Name") %>%
  left_join(Lookup_scenario, by = "Scenario")

# The EBA data contain exposures to regions or aggregates for which we have no ISO code. We replace these by the plain descriptors

exposures_plain$Code[exposures_plain$Name == "Total / No breakdown"] <- "Total"
exposures_plain$Code[exposures_plain$Name == "Africa"] <- "Africa"
exposures_plain$Code[exposures_plain$Name == "International organisations"] <- "International_organisations"
exposures_plain$Code[exposures_plain$Name == "Other advanced non EEA"] <- "Other_advanced_non_EEA"
exposures_plain$Code[exposures_plain$Name == "Other CEE non EEA"] <- "Other_CEE_non_EEA"
exposures_plain$Code[exposures_plain$Name == "Other"] <- "Other"

impairments_plain$Code[impairments_plain$Name == "Total / No breakdown"] <- "Total"
impairments_plain$Code[impairments_plain$Name == "Africa"] <- "Africa"
impairments_plain$Code[impairments_plain$Name == "International organisations"] <- "International_organisations"
impairments_plain$Code[impairments_plain$Name == "Other advanced non EEA"] <- "Other_advanced_non_EEA"
impairments_plain$Code[impairments_plain$Name == "Other CEE non EEA"] <- "Other_CEE_non_EEA"
impairments_plain$Code[impairments_plain$Name == "Other"] <- "Other"

# Now we rearrange columns and drop columns not needed anymore. We add two additional variables so we can understand from the dataframe directly
# the unit and currency of the amounts reported.

exposures_final <- select(exposures_plain, LEI_code, Country_code, Bank_name, Period, Code, Asset_classes, Amount) %>%
  rename(Exposure = Asset_classes) %>%
  rename(Country = Code) %>%
  add_column(Unit = "Millions") %>%
  add_column(Currency = "Euro") %>%
  bind_rows(total_assets) %>%
  bind_rows(common_equity_tier_1) %>%
  ungroup()

impairments_final <- select(impairments_plain, LEI_code, Country_code, Bank_name, Period, Label, Code, Asset_classes, Impairment_rate) %>%
  rename(Exposure = Asset_classes) %>%
  rename(Country = Code) %>%
  rename(Scenario = Label) %>%
  ungroup()

# Finally we want to attribute bond exposure data. The sovereign exposure data (TR_SOV_2016.csv) break down net foreign exposures into
# four categories which only apply to securities: Net direct exposures available for sale (AFS) Item 1690503, Net direct
# exposures designated at fair value through profit an loss (FVO) Item 1690506, Net direct exposures held for trading (HFT) Item 1690507,
# net direct exposures helt to maturity (HTM) Item 1690508. It also contains the complementary position Item 1690509 net direct exposures
# loans and receivables. The positive components of the sum of 1690503, 1690506, 1690507, 1690508 should then be the long position in
# sovereign bonds.

sovereign_exposures <- read_csv("data-raw/TR_SOV_2016.csv") %>%
  filter(Period == 201512, SOV_Maturity == 8, Item %in% c(1690503, 1690506, 1690507, 1690508)) %>%
  group_by(LEI_code, Country_code, Bank_name, Period, Country) %>%
  summarize(Amount = if_else(sum(Amount, na.rm = TRUE) < 0, 0, sum(Amount, na.rm = TRUE))) %>%
  add_column(Exposure = "Central banks and central governments", .after = "Bank_name")

# replace numerical country code by ISO or description

sovereign_exposures_plain <- left_join(sovereign_exposures, Lookup_countries, by = "Country") %>%
  left_join(Lookup_ISO, by = "Name")

sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Total / No breakdown"] <- "Total"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Africa"] <- "Africa"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "International organisations"] <- "International_organisations"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Other advanced non EEA"] <- "Other_advanced_non_EEA"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Other CEE non EEA"] <- "Other_CEE_non_EEA"
sovereign_exposures_plain$Code[sovereign_exposures_plain$Name == "Other"] <- "Other"

# clean up:

sovereign_exposures_final <- select(sovereign_exposures_plain, LEI_code, Country_code, Bank_name, Period, Code, Exposure, Amount) %>%
  rename(Country = Code) %>%
  add_column(Unit = "Millions") %>%
  add_column(Currency = "Euro") %>%
  ungroup()

# match data

matched_data <- left_join(exposures_final, sovereign_exposures_final,
  by = c("LEI_code", "Country_code", "Bank_name", "Period", "Country", "Exposure", "Unit", "Currency")
) %>%
  replace_na(list(Amount.y = 0)) %>%
  mutate(check = (Amount.x > Amount.y))

# Now here we have something strange in the data. For about 23 % of exposures the reported sum of positive net foreign exposures in securities
# is larger than the reported total exposure to Central governments and central banks. We cannot clarify this mystery here. We decide the
# following assignment rule. When  the total amount is greater we have no problem. When the securities amount is greater than the total
# amount we assume that the total amount is entiriely held in securities. This is of course an assumption and probably not true but
# it leads to a reasonable and consistent assignemnt rule.

matched_data_corr <- matched_data %>%
  mutate(Loan_Amount = if_else((Amount.x > Amount.y), Amount.x - Amount.y, Amount.y)) %>%
  mutate(Bond_Amount = if_else((Amount.x > Amount.y), Amount.y, Amount.x)) %>%
  mutate(Total_Amount = Loan_Amount + Bond_Amount) %>%
  select(LEI_code, Country_code, Bank_name, Period, Country, Exposure, Loan_Amount, Bond_Amount, Total_Amount, Unit, Currency)

# Now we are ready to save the aggregated, cleaned and relabeled data into an R format. These are then the data made available in the package.
# We save the aggregated and cleaned raw data also in a csv file in data-raw

eba_exposures_2016 <- matched_data_corr
eba_impairments_2016 <- impairments_final

usethis::use_data(eba_exposures_2016, overwrite = TRUE)
usethis::use_data(eba_impairments_2016, overwrite = TRUE)
