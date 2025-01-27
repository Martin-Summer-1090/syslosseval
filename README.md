
<!-- README.md is generated from README.Rmd. Please edit that file -->

# syslosseval

<!-- badges: start -->
<!-- badges: end -->

The goal of `syslosseval` is to provide the data and R-functions to
support the analysis of the paper “Systemic Loss Evaluation” by Thomas
Breuer, Martin Summer and Branko Urosevic. You can download the paper at
<https://ideas.repec.org/p/onb/oenbwp/235.html#download> The code and
the data published in this repository support the analysis of this
paper.

## Installation

This package is not on CRAN. You can only install the the development
version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Martin-Summer-1090/syslosseval")
```

## Example

When you install the package there will be in total seven datasets
available to you. These datasets are:

| Dataset number | Dataset name                     | Data Description                                         |
|:---------------|:---------------------------------|:---------------------------------------------------------|
| 1              | `eba_exposures_2016`             | Exposure data from the EBA 2016 stress test              |
| 2              | `eba_exposures_2020`             | Exposure data from the EBA 2020 transparency exercises   |
| 3              | `eba_impairments_2016`           | Impairment data from the EBA 2016 stress test            |
| 4              | `eba_impairments_2020`           | Imputed impairments data based on IMF methods            |
| 5              | `sovereign_bond_indices`         | Daily values of sovereign bond indices from 2009-2019    |
| 6              | `average_daily_volume_sovereign` | Average daily volumes of sovereign bonds from 2009 -2019 |
| 7              | `example_multiple_equilibria`    | A toy example, where multiple equilibria occur           |

A detailed description of how the data are compiled is given in the
paper in appendix B. Alternatively you can look at the scripts
`make_balance_sheets_2016.R`, `make_balance_sheets_2020.R`,
`make_price_volume_data.R` and `make_2020_impairment_scenarios.R`, which
are contained in the `syslosseval_raw_data.tar.gz` in the
`data-raw`folder of the project source code.

Here is a basic example where you:
Prepare a dataframe with exposures and impairments under the one year
ahead EBA stress scenario in the EBA 2016 stress test.
Prepare all the matrices and vectors needed to make a systemic loss
evaluation
Compute a fire sale equilibrium for these data.

``` r
library(syslosseval)
## basic example code

stress_data <- make_stress_data(eba_exposures_2016, eba_impairments_2016, 1, 2015)
state_variables <- make_state_variables(stress_data)
fixed_point_computation_function(mat = state_variables, lb = 33, data_idx = sovereign_bond_indices, 
                                 data_adv = average_daily_volume_sovereign, base_year = 2015, constant = 1.5)
#> # A tibble: 8 x 7
#>   sec_class       delta_lower iter_lower delta_upper iter_upper delta_max unique
#>   <chr>                 <dbl>      <int>       <dbl>      <int>     <dbl> <lgl> 
#> 1 DE                 0.00492          10    0.00492           9  0.0146   TRUE  
#> 2 ES                 0.000640         10    0.000640          9  0.0191   TRUE  
#> 3 FR                 0.00688          10    0.00688           9  0.0203   TRUE  
#> 4 GB                 0.0106           10    0.0106            9  0.0166   TRUE  
#> 5 IT                 0.0117           10    0.0117            9  0.0322   TRUE  
#> 6 JP                 0.000494         10    0.000494          9  0.000907 TRUE  
#> 7 US                 0.00434          10    0.00434           9  0.00908  TRUE  
#> 8 Rest_of_the_wo…    0.00132          10    0.00132           9  0.00418  TRUE
```

If you only want to expect particular dataframes, you can do so by
writing them to an object. Say you want to inspect the EBA 2016 exposure
and impairment data you could do the following:

``` r
exposures <- eba_exposures_2016
impairments <- eba_impairments_2016
```

If you are not familiar with R or you prefer to work rather in Python,
Mathematica, Matlab, Excel or any other language you prefer you could
export these data to a csv file by writing by using the `write.csv()`
function (or the `write.csv2()`) depending on the settings of your
system), load them into another program and work from there. In this
case you do not have available the functions which prepare state
variables, compute fixed points etc. If you don’t know how to use
`write.csv()` or `write.csv2()`, please consult the help functions of R
either by using the Help pane in R studio or by typing `?write.csv` at
the R prompt.
