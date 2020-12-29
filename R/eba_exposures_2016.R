#' Exposure data of 51 European banks which took part in the EBA stress test 2016.
#'
#' A dataset containing the exposure data of 51 European banks who took part in the EBA stress test 2016. The data
#' are compiled from a raw data set. The script of compiling the data from the raw data can be inspected by editing the
#' file make_balance_sheets_2016.R in the raw-data folder of the syslosseval package source code.
#'
#' @format A data frame with 3112 rows and 11 variables:
#' \describe{
#'   \item{LEI_code}{LEI_code, legal entity identifyer of a bank.}
#'   \item{Country_code}{Country_code, ISO-code of the country where the bank is domiciled}
#'   \item{Bank_name}{Bank_name, Name of the bank}
#'   \item{Period}{Period, Reporting period of the data given as 201512 (31. December 2015)}
#'   \item{Country}{Country, Country to which the bank is exposed. This is either a ISO code like AT, DE etc. or it is
#'   called Total if it is the overall exposure aggregated across all countries, or there are special abbreviations
#'   which are further explained in the EBA metadata in the raw-data folder. They are not relevant for our analysis and
#'   not described further here.}
#'   \item{Exposure}{Asset class according to the IRB classification scheme}
#'   \item{Loan_Amount}{Loan_Amount, the exposure value in Million Euro held in the form of loans}
#'   \item{Bond_Amount}{Bond_Amount, the exposure value in Million Euro held as bonds}
#'   \item{Total_Amount}{Total_Amount, the exposure value as the sum of loan and bond amount in Million Euro}
#'   \item{Unit}{Unit, the units of Loan_Amount, Bond_Amount, Total_Amount}
#'   \item{Currency}{Currency, the currency of Loan_Amount, Bond_Amount, Total_Amount}
#' }
#' @source \url{https://eba.europa.eu/risk-analysis-and-data/eu-wide-stress-testing/2016}
"eba_exposures_2016"
