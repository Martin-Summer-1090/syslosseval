% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_price_impact_data.R
\name{make_price_impact_data}
\alias{make_price_impact_data}
\title{make_price_impact_data}
\usage{
make_price_impact_data(data_idx, data_adv, base_year)
}
\arguments{
\item{data_idx}{sovereign bond indices}

\item{data_adv}{average daily volume data}

\item{base_year}{the year for which the analysis is done}
}
\value{
A dataframe with variables: Country ADV_sov_bonds, sigma_sov_bonds.
}
\description{
This function prepares the price impact parameters volatility and average
daily volume for the bond exposures in DE, ES, IT, FR, JP, GB, US and Rest of
the world for a given stress test base year. Daily volatility for a given
year is computed on the daily index log returns of the base year.
}
\examples{
make_price_impact_data(sovereign_bond_indices, average_daily_volume_sovereign, 2015)
}
