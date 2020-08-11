
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/trending)](https://CRAN.R-project.org/package=trending)
[![Codecov test
coverage](https://codecov.io/gh/reconhub/trending/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/trending?branch=master)
[![R build
status](https://github.com/reconhub/trending/workflows/R-CMD-check/badge.svg)](https://github.com/reconhub/trending/actions)
<!-- badges: end -->

<br> **<span style="color: red;">Disclaimer</span>**

This package is a work in progress. Please reach out to the authors
before using.

``` r
library(trendbreaker)  # for data
library(trending)      # for trends
library(tidyverse, warn.conflicts = FALSE)  # for data manipulation
#> ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
#> ✓ tibble  3.0.3     ✓ dplyr   1.0.1
#> ✓ tidyr   1.1.1     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

# load data
data(nhs_pathways_covid19)

# select last 6 weeks of data
first_date <- max(nhs_pathways_covid19$date, na.rm = TRUE) - 6*7
pathways_recent <- filter(nhs_pathways_covid19, date >= first_date)


# individual model --------------------------------------------------------
model = lm_model(count ~ day + weekday)

# split data for fitting and prediction
dat <- group_split(pathways_recent, date <= first_date + 5*7)
fitting_data <- dat[[1]]
pred_data <- dat[[2]]

fitted_model <- fit(model, fitting_data)
pred <- predict(fitted_model, pred_data)
str(pred)
#> tibble [54,802 × 16] (S3: tbl_df/tbl/data.frame)
#>  $ site_type                 : chr [1:54802] "111" "111" "111" "111" ...
#>  $ date                      : Date[1:54802], format: "2020-04-16" "2020-04-16" ...
#>  $ sex                       : chr [1:54802] "female" "female" "female" "female" ...
#>  $ age                       : chr [1:54802] "0-18" "0-18" "0-18" "0-18" ...
#>  $ ccg_code                  : chr [1:54802] "e38000007" "e38000044" "e38000074" "e38000084" ...
#>  $ ccg_name                  : chr [1:54802] "nhs_basildon_and_brentwood_ccg" "nhs_doncaster_ccg" "nhs_harrow_ccg" "nhs_hounslow_ccg" ...
#>  $ count                     : int [1:54802] 3 3 6 1 6 2 8 2 1 9 ...
#>  $ postcode                  : chr [1:54802] "ss143hg" "dn45hz" "ha13aw" "tw33eb" ...
#>  $ nhs_region                : chr [1:54802] "East of England" "North East and Yorkshire" "London" "London" ...
#>  $ day                       : int [1:54802] 29 29 29 29 29 29 29 29 29 29 ...
#>  $ weekday                   : Factor w/ 3 levels "rest_of_week",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ date <= first_date + 5 * 7: logi [1:54802] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ pred                      : Named num [1:54802] 8.5 8.5 8.5 8.5 8.5 ...
#>   ..- attr(*, "names")= chr [1:54802] "1" "2" "3" "4" ...
#>  $ lower                     : Named num [1:54802] -13.2 -13.2 -13.2 -13.2 -13.2 ...
#>   ..- attr(*, "names")= chr [1:54802] "1" "2" "3" "4" ...
#>  $ upper                     : Named num [1:54802] 30.2 30.2 30.2 30.2 30.2 ...
#>   ..- attr(*, "names")= chr [1:54802] "1" "2" "3" "4" ...
#>  $ observed                  : int [1:54802] 3 3 6 1 6 2 8 2 1 9 ...


# select from multiple models ---------------------------------------------
models <- list(
  regression = lm_model(count ~ day),
  poisson_constant = glm_model(count ~ 1, family = "poisson"),
  negbin_time = glm_nb_model(count ~ day),
  negbin_time_weekday = glm_nb_model(count ~ day + weekday)
)

results <- evaluate_models(pathways_recent, models, v = 10)
results
#> # A tibble: 4 x 2
#>   model                rmse
#>   <chr>               <dbl>
#> 1 regression           21.8
#> 2 poisson_constant     22.0
#> 3 negbin_time          21.8
#> 4 negbin_time_weekday  21.8
```
