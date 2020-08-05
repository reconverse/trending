
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/trending)](https://CRAN.R-project.org/package=trending)
<!-- badges: end -->

<br> **<span style="color: red;">Disclaimer</span>**

This package is a work in progress. Please reach out to the authors
before using.

``` r
library(incidence2)
library(outbreaks)
library(trending)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

# date parameters
first_date <- "2014-06-07"
last_date <- as.Date("2014-08-07")

# generate incidence object and add some predictive variables
inci <-
  ebola_sim_clean$linelist %>%
  incidence(date_index = date_of_onset,
            first_date = first_date,
            last_date = last_date + 21) %>%
  mutate(day = 1:nrow(.), weekday = weekdays(bin_date)) %>%
  mutate(weekday = case_when(
    weekday == "Monday"   ~ "monday",
    weekday == "Saturday" ~ "weekend",
    weekday == "Sunday"   ~ "weekend",
    TRUE                  ~ "rest of week"))
#> 5047 observations outside of [2014-06-07, 2014-08-28] were removed.

# split in to some fitting and prediction data
fitting_data <- filter(inci, bin_date <= last_date)
predict_data <- filter(inci, bin_date > last_date)

# model and predict
negbin <- glm_nb_model(count ~ day + weekday)
pred <- fit_and_predict(negbin, fitting_data, predict_data)
#> Warning in theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace =
#> control$trace > : iteration limit reached

#> Warning in theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace =
#> control$trace > : iteration limit reached

# plot
plot(pred, "bin_date", "count")
#> Warning: Removed 62 rows containing missing values (geom_point).
```

<img src="man/figures/README-unnamed-chunk-1-1.png" style="display: block; margin: auto;" />
