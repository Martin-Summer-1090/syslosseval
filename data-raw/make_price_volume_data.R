# This script assembles data on the average daily volume figures of sovereign bonds of Germany, Spain, Italy, France, United Kingdom,
# United States and Japan.
# It collects corresponding bond indices from - insert address - following Cont and Schaaning 2016 (reference).

# loading the necessary packages

library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)



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

# United Kindom (UK). Data are reported on the website https://www.dmo.gov.uk/data/gilt-market/turnover-data/
# The data are entered manually

volume_uk <- tibble(
  Year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
  Volume = c(37.37, 36.33, 33.00, 28.58, 26.24, 28.45, 27.53, 28.86, 28.82, 20.71, 18.85),
  Period = "Daily",
  Unit = "Billion",
  Currency = "GBP"
) %>%
  add_column(Country = "UK", .before = "Year")

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

# Now stitch all the countries together in one file and make sure we have the same units and the same currency in all entries.
# We want average daily volume in Million Euro.

avd <- bind_rows(volume_de, volume_es, volume_fr, volume_it, volume_jp, volume_uk, volume_us) %>%
  mutate(volume_d = if_else(Period == "Annual", Volume / 252, Volume)) %>%
  mutate(volume_d_m = if_else(Unit == "Billion", volume_d * 10^3, volume_d))

# Add a columns with exchange rates:

fx <- rep(c(1, 1, 1, 1, 0.9209, 1.356, 0.9209), each = 11)


adv_e <- avd %>%
  add_column(fx)

average_daily_volume_sovereign <- adv_e %>%
  mutate(volume_d_m_e = volume_d_m * fx) %>%
  select(Country, Year, volume_d_m_e) %>%
  rename(Volume = volume_d_m_e) %>%
  add_column(Unit = "Million", Currency = "Euro")


# write the data to a csv file and add to the data folder of the package

write_csv(average_daily_volume_sovereign, "data-raw/average_daily_volume_sovereign.csv")
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

# United Kingdom (UK) The S&P U.K. Gilt Index is a comprehensive, market-value-weighted index
# designed to track the performance of British pound-denominated securities publicly issued by the U.K. for its domestic market.

prices_UK <- read_csv("data-raw/Sovereign_Bond_Index_UK.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P U.K. Gilt Index") %>%
  add_column(Country = "UK", .before = "Date")

# United States (US) The S&P U.S. Government Bond Index seeks to track the
# performance of U.S. dollar-denominated U.S. Treasury and U.S. agency debt issued in the
# U.S. domestic market.

prices_US <- read_csv("data-raw/Sovereign_Bond_Index_US.csv") %>%
  rename(Date = "Effective date") %>%
  rename(Value = "S&P U.S. Government Bond Index") %>%
  add_column(Country = "US", .before = "Date")

# make the total price file

sovereign_bond_indices <- bind_rows(prices_DE, prices_ES, prices_FR, prices_IT, prices_JP, prices_UK, prices_US) %>%
  mutate(Date = mdy(Date))

# write the data to a csv file and add to the data folder of the package

write_csv(sovereign_bond_indices, "data-raw/sovereign_bond_indices.csv")
usethis::use_data(sovereign_bond_indices, overwrite = TRUE)
