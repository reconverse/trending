
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

## An individual model

``` r
library(trendbreaker)  # for data
library(trending)      # for trends
library(dplyr)         # for data manipulation
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# load data
data(nhs_pathways_covid19)

# select last 6 weeks of data
first_date <- max(nhs_pathways_covid19$date, na.rm = TRUE) - 6*7
pathways_recent <- filter(nhs_pathways_covid19, date >= first_date)

# define a linear model
model = lm_model(count ~ day + weekday)

# split data for fitting and prediction
dat <- group_split(pathways_recent, date <= first_date + 5*7)
fitting_data <- dat[[1]]
pred_data <- dat[[2]]

fitted_model <- fit(model, fitting_data)
pred <- predict(fitted_model, pred_data)
glimpse(pred)
#> Rows: 54,802
#> Columns: 16
#> $ site_type                    <chr> "111", "111", "111", "111", "111", "111"…
#> $ date                         <date> 2020-04-16, 2020-04-16, 2020-04-16, 202…
#> $ sex                          <chr> "female", "female", "female", "female", …
#> $ age                          <chr> "0-18", "0-18", "0-18", "0-18", "0-18", …
#> $ ccg_code                     <chr> "e38000007", "e38000044", "e38000074", "…
#> $ ccg_name                     <chr> "nhs_basildon_and_brentwood_ccg", "nhs_d…
#> $ count                        <int> 3, 3, 6, 1, 6, 2, 8, 2, 1, 9, 3, 3, 13, …
#> $ postcode                     <chr> "ss143hg", "dn45hz", "ha13aw", "tw33eb",…
#> $ nhs_region                   <chr> "East of England", "North East and Yorks…
#> $ day                          <int> 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, …
#> $ weekday                      <fct> rest_of_week, rest_of_week, rest_of_week…
#> $ `date <= first_date + 5 * 7` <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
#> $ pred                         <dbl> 8.497642, 8.497642, 8.497642, 8.497642, …
#> $ lower                        <dbl> -13.18078, -13.18078, -13.18078, -13.180…
#> $ upper                        <dbl> 30.17606, 30.17606, 30.17606, 30.17606, …
#> $ observed                     <int> 3, 3, 6, 1, 6, 2, 8, 2, 1, 9, 3, 3, 13, …
```

## Model selection

You can define a number of different regression models using a common
interface. Once defined you can use different strategies to select the
best-fitting/best-predicting model.

As an example we try to predict `hp` of the famous `mtcars` dataset. Of
course, this is just a toy example. Usually you would use the package to
predict counts data in a time series.

First we define some potential models:

``` r
stan_cache <- tempfile() # stan compile to c++ and we cache the code
models <- list(
  null = lm_model(hp ~ 1),
  glm_poisson = glm_model(hp ~ 1 + cyl + drat + wt + qsec + am, poisson),
  lm_complex = lm_model(hp ~ 1 + cyl + drat + wt + qsec + am),
  negbin_complex = glm_nb_model(hp ~ 1 + cyl + drat + wt + qsec + am),
  brms_complex = brms_model(
    hp ~ 1 + cyl + drat + wt + qsec + am,
    family = brms::negbinomial(),
    file = stan_cache
  )
)
```

Then we evaluate them using [N-Fold cross
validation](https://en.wikipedia.org/wiki/Cross-validation_\(statistics\)).

``` r
# we do CV and evaluate three loss function:
# Root-mean-squared error, the huber-loss and mean absolute error.
# The package works with `yardstick` by default.
out <- capture.output( # no log output in readme :)
  auto_select <- select_model(mtcars, models,
    method = evaluate_resampling,
    metrics = list(yardstick::rmse, yardstick::huber_loss, yardstick::mae)
  )
)
auto_select$leaderboard
#> # A tibble: 5 x 4
#>   model          huber_loss   mae  rmse
#>   <chr>               <dbl> <dbl> <dbl>
#> 1 brms_complex         18.2  18.7  18.7
#> 2 glm_poisson          21.2  21.7  21.7
#> 3 negbin_complex       22.8  23.3  23.3
#> 4 lm_complex           26.2  26.7  26.7
#> 5 null                 57.8  58.3  58.3
```
