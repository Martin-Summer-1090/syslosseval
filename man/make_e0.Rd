% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_e0.R
\name{make_e0}
\alias{make_e0}
\title{make_e0}
\usage{
make_e0(data)
}
\arguments{
\item{data}{a dataframe which is the output of \code{make_stress_data()}}
}
\value{
a B x 1 (number of banks x 1) with Core tier1 equity capital of all banks.
}
\description{
This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
output of \code{make_stress_data()}, and constructs a vector giving the common tier 1 equity of all banks.
}
\examples{
stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
make_e0(stress_data)
}
