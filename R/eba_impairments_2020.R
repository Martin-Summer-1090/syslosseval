#' Hypothetical Impairment data of 123 European banks which took part in the EBA
#' transparency exercise 2020.
#'
#' A dataset containing hypothetical impairment data of 123 European banks who
#' took part in the EBA transparency exercise. The data are postulated with
#' reference to the paper "Rules of Thumb for Bank Solvency Stress Tests" by
#' Daniel C. Hardy and Christian Schmiederer. IMF working paper 13/232
#' (\url{https://www.imf.org/external/pubs/ft/wp/2013/wp13232.pdf}). We use the
#' numbers from table 2 in this paper and the assumption that impairment rates
#' will be 2.4 % in the first year after the outbreak of the covid pandemic
#' (severe), 4.3% in the second year after the pandemic and 1.1 % in the third
#' year after the pandemic. The loss rate in the normal scenario is assumed to
#' be 0.3 %. The file is generated automatically by the \code{make_2020_impairment_scenarios}
#' script stored in the data-raw folder.
#'
#' @format A data frame with 32290 rows and 8 variables: \describe{
#'   \item{LEI_code}{LEI_code, legal entity identifyer of a bank. chr.}
#'   \item{Country_code}{Country_code, ISO-code of the country where the bank is
#'   domiciled, chr.}
#'   \item{Bank_name}{Bank_name, Name of the bank, chr.}
#'   \item{Period}{Period, Reporting period of the data given as 201912 (31.
#'   December 2019). There are three impairment periods in total. num}
#'   \item{Scenario}{Scenario, reports whether it is the baseline or the stress
#'   scenario, chr}
#'   \item{Country}{Country, Country to which the bank is
#'   exposed. This is either a ISO code like AT, DE etc. or it is called Total
#'   if it is the overall exposure aggregated across all countries, or there are
#'   special abbreviations which are further explained in the EBA metadata in
#'   the raw-data folder. They are not relevant for our analysis and not
#'   described further here. chr}
#'   \item{Exposure}{Asset class according to the
#'   IRB classification scheme. chr}
#'   \item{Impairment_rate}{Impairment_rate, the
#'   Impairment rate in percentage according to Hardy and Schmieder 2013. num} }
#' @source
#' \url{https://www.eba.europa.eu/risk-analysis-and-data/eu-wide-transparency-exercise}
#'
"eba_impairments_2020"
