% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_e1.R
\name{make_e1}
\alias{make_e1}
\title{make_e1}
\usage{
make_e1(data)
}
\arguments{
\item{data}{a dataframe which is the output of \code{make_stress_data()}}
}
\value{
a B x 1 (number of banks x 1) with Core tier1 equity capital of all banks at t = 1.
}
\description{
This function takes the dataframe which assembles the stresstest data from exposures and impairments, the
output of \code{make_stress_data()}, and constructs a vector giving the common tier 1 equity of all banks at t = 1,
which is common tier 1 equity minus losses from the stressed impairments. The components of the vector e_1
are the differecne between the components of e_0 minus the projected losses of the stress test. If these
losses exceed e_0, e_1 is set to zero.
}
\examples{
stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
make_e1(stress_data)
}
