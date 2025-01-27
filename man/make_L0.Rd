% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_L0.R
\name{make_L0}
\alias{make_L0}
\title{make_L0}
\usage{
make_L0(data)
}
\arguments{
\item{data}{a dataframe which is the output of \code{make_stress_data()}}
}
\value{
a B x J (number of banks x number of IRB exposure categories)
}
\description{
This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
output of \code{make_stress_data()}, and constructs a matrix giving the loan exposure in all IRB exposure
classes for all banks.
}
\examples{
stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
make_L0(stress_data)
}
