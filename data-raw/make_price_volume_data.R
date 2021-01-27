# This script assembles data on the average daily volume figures of sovereign bonds of
# Germany, Spain, Italy, France, United Kingdom,
# United States and Japan.
# It collects corresponding bond indices
# from the website http://us.spindices.com following Cont and Schaaning 2016.

# loading the necessary packages

library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)



# Data on average daily volumes:
#
# General comment on the collection of volume data:
# We collect these data manually. It does not pay off to collect these data by scraping because the web environemnt
# changes all the time, the data are only collected at large time intervals and the sample is very small. Note that the
# specific links might be deprecated after a while due to reorganisations of the webpage etc. Usually the new locations can
# be recovered easily via the homepage of the respective institutions, e.g. Deutsche Finanzierungsargentur, Tesoro etc.
#
# Germany (DE) provides data on annual turnover of German Government Bonds on the page of Deutsche Finanzargentur:
# https://www.deutsche-finanzagentur.de/en/institutional-investors/secondary-market/.

volume_de <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(4091, 4694, 4790, 4294, 4715, 4869, 5832, 5501, 6184, 5863, 4762),
  Period = "Annual",
  Unit = "Billion",
  Currency = "Euro"
) %>%
  add_column(Country = "DE", .before = "Year")

# Spain (ES) provides turnover data on the webpage
# https://www.tesoro.es/sites/default/files/estadisticas/15I.xlsx. We take the data from there.


volume_es <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(16055.926, 14075.520, 9310.570, 8288.116, 10351.437, 14763.478, 11864.388, 11045.241, 16298.817, 14978.448, 15001.690),
  Period = "Daily",
  Unit = "Million",
  Currency = "Euro"
) %>%
  add_column(Country = "ES", .before = "Year")

# Italy (IT) provides turnover data at the data homepage of the banca dItalia https://infostat.bancaditalia.it/. The data
# have the description:
# Famiglia: QMTS - Titoli di Stato quotati al Mercato Telematico (M.T.S.)
# Chiave della serie storica
# D Frequency:Daily
# 100010 Type of securities:Italian government securities
# 926 Type of data:Traded quantity
# MKV Valuation:Market value
# EUR Operation currency:Euro
# 9 Original maturity:Total maturity
# Series identifier used in previous version :    S199164D
# Measure: Value
#
# These data are provided daily. We retrieve the raw data from the webpage as an Excel-file and transform it to a csv format. We compute
# the average daily turnover per annum from this file in line with the other data.

volume_it <- read_csv("data-raw/Turnover_value_government_bonds_IT.csv") %>%
  rename(Date = "Observation date") %>%
  mutate(time = dmy(Date)) %>%
  select(time, Value) %>%
  separate(time, into = c("Year", "Month", "Day")) %>%
  group_by(Year) %>%
  summarise(Volume = mean(Value, na.rm = T)) %>%
  ungroup() %>%
  add_column(Period = "Daily", .after = "Volume") %>%
  add_column(Unit = "Million", .after = "Period") %>%
  add_column(Currency = "Euro", .after = "Unit") %>%
  add_column(Country = "IT", .before = "Year")

# we have to change the year variable from type character into numeric

volume_it$Year <- as.numeric(volume_it$Year)

# France (FR): Here we take the data from afme
# https://www.afme.eu/reports/data/details//Government-Bond-Data-Report-Q2-2019
# The data have to be entered by hand since they are only available graphically. The values are measured from the
# historgram by hand.

volume_fr <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(16, 10, 11.5, 8.5, 9, 11, 11.5, 10.5, 10, 9.5, 6.5),
  Period = "Daily",
  Unit = "Billion",
  Currency = "Euro"
) %>%
  add_column(Country = "FR", .before = "Year")

# United Kindom (GB). Data are reported on the website https://www.dmo.gov.uk/data/gilt-market/turnover-data/
# The data are entered manually

volume_gb <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(37.37, 36.33, 33.00, 28.58, 26.24, 28.45, 27.53, 28.86, 28.82, 20.71, 18.85),
  Period = "Daily",
  Unit = "Billion",
  Currency = "GBP"
) %>%
  add_column(Country = "GB", .before = "Year")

# United States (US). Data are reported on the website https://www.sifma.org/resources/research/us-treasury-trading-volume/
# The data are enteree manually

volume_us <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(593.6, 547.8, 505.2, 519.1, 490.0, 505.0, 545.4, 520.3, 567.8, 523.9, 411.1),
  Period = "Daily",
  Unit = "Billion",
  Currency = "USD"
) %>%
  add_column(Country = "US", .before = "Year")

# Japan (JP). Data are from the website https://asianbondsonline.adb.org/data-portal/
# The data are manually entered.

volume_jp <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(10486.24297947, 10280.05880208, 10364.49729346, 10275.9370725, 12223.53684911, 14143.36784104, 14967.04728896, 20974.33717579, 22715.84839423, 21140.87154832, 17389.10825629),
  Period = "Annual",
  Unit = "Billion",
  Currency = "USD"
) %>%
  add_column(Country = "JP", .before = "Year")

# Stitch these files together:

adv_selected <- bind_rows(volume_de, volume_es, volume_fr, volume_it, volume_jp, volume_gb, volume_us) %>%
  mutate(volume_d = if_else(Period == "Annual", Volume / 252, Volume)) %>%
  mutate(volume_d_m = if_else(Unit == "Billion", volume_d * 10^3, volume_d))

# Convert all values to Euro. We have already the USD exchange rate we now also need the GBP exchange rate. All other
# values are in Euro. The rates are taken from https://www.macrotrends.net/

# annual exchange rate Euro-USD and Euro-GBP

euro_usd_fx <- read_csv("data-raw/euro_usd_fx_annual.csv") %>%
  add_column(Currency = "USD") %>%
  rename(FX_euro_usd = FX)

euro_gbp_fx <- read_csv("data-raw/euro_gbp_fx_annual.csv") %>%
  add_column(Currency = "GBP") %>%
  rename(FX_euro_gbp = FX)

# Add exchange rates for conversion into Euro to our adv data

adv_sov <- left_join(adv_selected, euro_usd_fx, by = c("Year", "Currency")) %>%
  left_join(euro_gbp_fx, by = c("Year", "Currency")) %>%
  select(Country, Year, Currency, volume_d_m, FX_euro_usd, FX_euro_gbp) %>%
  mutate(adv = if_else(Currency == "USD", volume_d_m * 1/(FX_euro_usd), volume_d_m)) %>%
  mutate(adv = if_else(Currency == "GBP", adv * 1/(FX_euro_gbp), adv)) %>%
  select(Country, Year, adv) %>%
  rename(Volume = adv) %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro")

# Global: We have no data on the ADV in all the rest of exposures, because there are no
# or insufficient direct observations. Cont and Schaanning (2016) observe a high correlation between
# nominal deat outsanding and adv. The use this correlation by estimating unobserved ADV using regression
# techniques. We apply an even simpler estimate. We compute the share of nominal debt oustanding of our
# observed countries DE, ES, FR, IT, JP, GB, US in the total nominal government debt outstanding. We use this
# proportionality factor to impute an ADV on the residual position.

# We have downloaded the quarterly data from 2009 - 2020 q2 from the BIS as individual excel files to our data-raw
# directory into a seperate folder Debt_securities.

# create a list of the downloaded bis excel files and read them into a list of files for each downloaded quarter
# (2009 Q1) up to 2020 Q2)

file_list <- list.files(path ="data-raw/Debt_securities/", pattern =" *.xlsx", full.names = T)[-1]

bis_data <- lapply(file_list, read_excel, skip = 12, col_names = F)

# Extract the data needed. We only collect data of nominal government debt outstanding for each country and
# drop all the other information and add information about unit and currency

clean_bis_data <- function(data){

  dat <- data[,c(1,5)]

  colnames(dat) <- c("Name", "Value")

  data <- dat %>%
    na.omit() %>%
    filter( !(Name %in% c("Offshore centres", "Emerging market and developing economies",
                          "Developing Africa and Middle East", "Developing Asia and Pacific",
                          "Developing Latin America & Caribbean",	"International organisations"))) %>%
    mutate_all(na_if, "...") %>%
    mutate(across("Value", as.numeric)) %>%
    add_column(Exposure = "Debt_securities_outstanding_general_government", .before = "Value") %>%
    add_column(Unit = "Billions") %>%
    add_column(Currency = "USD")
}

# Apply the cleaning of each function. Note that there are warnings issued for each operation because empty cells
# are going to be transformed to NA. But this is what we want anyway, so the warnings may be ignored.

bis_data_clean <- lapply(bis_data, clean_bis_data)

# add year columns from 2009 - 2019

for(i in 1:(length(bis_data_clean)-2)){

  j <- rep(1:11, each = 4)

  bis_data_clean[[i]] <- add_column(bis_data_clean[[i]], Year = (2008 + j[i]), .after = "Name")

}

# add the last two year columns for 2020

bis_data_clean[[44]] <- add_column(bis_data_clean[[44]], Year = 2020, .after = "Name")
bis_data_clean[[45]] <- add_column(bis_data_clean[[45]], Year = 2020, .after = "Name")

# add quarter columns for 2009 - 2019

for(i in 1:(length(bis_data_clean)-2)){


  j <- rep(seq(1,4),43)

  bis_data_clean[[i]] <- add_column(bis_data_clean[[i]], Quarter = paste0("q", j[i]), .after = "Year")

}

# add the last two columns for 2020

bis_data_clean[[44]] <- add_column(bis_data_clean[[44]], Quarter = "q1", .after = "Year")
bis_data_clean[[45]] <- add_column(bis_data_clean[[45]], Quarter = "q2", .after = "Year")

# stitch all data together in one big dataframe

bis_data_clean_agg <- do.call(bind_rows, bis_data_clean)

# add iso codes and aggregate to annual by averaging quarters:

LT <- read_csv("data-raw/Lookup_table_ISO.csv")

bis_data_clean_agg_with_iso <- left_join(bis_data_clean_agg, LT, by = "Name") %>%
  select(Code, Year, Quarter, Exposure, Value, Unit, Currency) %>%
  rename(Country = Code) %>%
  na.omit() %>%
  group_by(Country, Year) %>%
  summarise(Value = mean(Value, na.rm = T)) %>%
  ungroup()

# Filter countries DE, ES, FR, IT, JP, GB, US from bis_data_clean_agg_with_iso

bis_data_observed <- bis_data_clean_agg_with_iso %>%
  filter( Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US")) %>%
  mutate(Value = (Value * 10^3)) %>%
  left_join(euro_usd_fx, by = "Year") %>%
  filter(Year != 2020) %>%
  mutate(Volume = Value*(1/FX_euro_usd)) %>%
  select(Country, Year, Volume) %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro")

# prepare the data for regression:

full_data <- left_join(adv_sov,
                       bis_data_observed, by = c("Country", "Year", "Unit", "Currency"))

model <- lm( log(Volume.x) ~ log(Volume.y), data = full_data)

# Select all countries which are not in the list of explicitly considered countries DE, ES, FR, IT, JP, GB, US

bis_data_unobserved <- bis_data_clean_agg_with_iso %>%
  filter( !(Country %in% c("DE", "ES", "FR", "IT", "JP", "GB", "US"))) %>%
  group_by(Year) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  filter(Year != 2020) %>%
  arrange(desc(Year)) %>%
  mutate(Volume = Value*10^3) %>%
  left_join(euro_usd_fx, by = "Year") %>%
  mutate(Volume = Volume*(1/FX_euro_usd)) %>%
  select(Year, Volume) %>%
  add_column(Country = "Rest_of_the_world", .before = "Year") %>%
  add_column(Unit = "Million") %>%
  add_column(Currency = "Euro") %>%
  arrange(desc(Year)) %>%
  mutate(ADV = exp(model$coefficients[2]*log(Volume) + model$coefficients[1])) %>%
  select(Country, Year, ADV, Unit, Currency) %>%
  rename(Volume = ADV)

# Combine with the individual countries dataframe

average_daily_volume_sovereign <- bind_rows(adv_sov, bis_data_unobserved)

# add to the data folder of the package

usethis::use_data(average_daily_volume_sovereign, overwrite = TRUE)

# Data on debt indices:

# We read data on sovereign bond indices from the website http://us.spindices.com
# Data are downloaded manually via the websites export function. Due to permanent changes in the
# website and the small amount of indices it does not pay off to implement a web scraping solution here.


# Germany (DE): The S&P Germany Sovereign Bond Index is a comprehensive, market-value-weighted index
# designed to track the performance of euro-denominated securities publicly issued by Germany for its domestic market.

prices_DE <- read_csv("data-raw/Sovereign_Bond_Index_DE.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P Germany Sovereign Bond Index") %>%
  add_column(Country = "DE", .before = "Date")

# Spain (ES): The S&P Spain Sovereign Bond Index is a comprehensive, market-value-weighted index
# designed to track the performance of euro-denominated securities publicly issued by Spain for its domestic market.

prices_ES <- read_csv("data-raw/Sovereign_Bond_Index_ES.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P Spain Sovereign Bond Index") %>%
  add_column(Country = "ES", .before = "Date")

# France (FR): The S&P France Sovereign Bond Index is a comprehensive, market-value-weighted index
# designed to track the performance of euro-denominated securities publicly issued by France for its domestic market.

prices_FR <- read_csv("data-raw/Sovereign_Bond_Index_FR.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P France Sovereign Bond Index") %>%
  add_column(Country = "FR", .before = "Date")

# Italy (IT): The S&P Italy Sovereign Bond Index is a comprehensive, market-value-weighted index designed to
# track the performance of euro-denominated securities publicly issued by Italy for its domestic market.

prices_IT <- read_csv("data-raw/Sovereign_Bond_Index_IT.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P Italy Sovereign Bond Index") %>%
  add_column(Country = "IT", .before = "Date")

# Japan (JP): The S&P Japan Bond Index is designed to track the performance of local-currency denominated
# government and corporate bonds issued in Japan.

prices_JP <- read_csv("data-raw/Sovereign_Bond_Index_JP.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P Japan Bond Index") %>%
  add_column(Country = "JP", .before = "Date")

# United Kingdom (GB) The S&P U.K. Gilt Index is a comprehensive, market-value-weighted index
# designed to track the performance of British pound-denominated securities publicly issued by the U.K. for its domestic market.

prices_GB <- read_csv("data-raw/Sovereign_Bond_Index_UK.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P U.K. Gilt Index") %>%
  add_column(Country = "GB", .before = "Date")

# United States (US) The S&P U.S. Government Bond Index seeks to track the
# performance of U.S. dollar-denominated U.S. Treasury and U.S. agency debt issued in the
# U.S. domestic market. This series has a history only back to 2017. No 2016 data.

prices_US <- read_csv("data-raw/Sovereign_Bond_Index_US.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P U.S. Government Bond Index") %>%
  add_column(Country = "US", .before = "Date")

# Global The S&P Global Developed Aggregate Ex-Collateralized Bond Index seeks to track
# the performance of investment-grade debt publicly issued by sovereign, quasi-government,
# and investment-grade corporate entities, excluding collateralized/securitized bonds.

prices_row <- read_csv("data-raw/Sovereign_Bond_Index_Global.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P Global Developed Aggregate Ex-Collateralized Bond Index (USD)") %>%
  add_column(Country = "Rest_of_the_world", .before = "Date")

# make the total price file

sovereign_bond_indices <- bind_rows(prices_DE, prices_ES, prices_FR, prices_IT, prices_JP, prices_GB, prices_US,
                                    prices_row) %>%
  mutate(Date = mdy(Date))

# add to the data folder of the package

usethis::use_data(sovereign_bond_indices, overwrite = TRUE)
