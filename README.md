
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

# Trending

*trending* aims to provides a coherent interface to several modelling
tools. Whilst it is useful in an interactive context, it’s main focus is
to provide an intuitive interface on which other packages can be
developed
(e.g. [*trendbreaker*](https://github.com/reconhub/trendbreaker)).

## Main features

  - **Model specification:** Interfaces to common models through
    intuitive functions; `lm_model()`, `glm_model()`, `glm_nb_model`\*
    and `brms_model`\*\*.

  - **Model fitting and prediction:** Once specified, models can be fit
    to data and generate confidence and prediction intervals for future
    data using `fit()` and `predict()`.

  - **Plotting functionality** A basic plotting method for trending
    model predictions.

\*  Requires [MASS](https://CRAN.R-project.org/package=MASS)  
\*\* Requires [brms](https://CRAN.R-project.org/package=brms)

## Example usage

### An individual model

``` r
library(outbreaks)  # for data
library(trending)   # for trend fitting
library(dplyr, warn.conflicts = FALSE)  # for data manipulation

# load data
data(covid19_england_nhscalls_2020)

# define a model
model  <- glm_nb_model(count ~ day + weekday)

# select 6 weeks of data (from a period when the prevalence was decreasing)
last_date <- as.Date("2020-05-28")
first_date <- last_date - 8*7
pathways_recent <-
  covid19_england_nhscalls_2020 %>%
  filter(date >= first_date, date <= last_date) %>%
  group_by(date, day, weekday) %>%
  summarise(count = sum(count), .groups = "drop")

# split data for fitting and prediction
dat <-
  pathways_recent %>%
  group_by(date <= first_date + 6*7) %>%
  group_split()

fitting_data <- dat[[2]]
pred_data <- select(dat[[1]], date, day, weekday)

fitted_model <- fit(model, fitting_data)

# confidence and prediction intervals
pred <- predict(fitted_model, pred_data)
glimpse(pred)
#> Rows: 14
#> Columns: 8
#> $ date       <date> 2020-05-15, 2020-05-16, 2020-05-17, 2020-05-18, 2020-05-1…
#> $ day        <int> 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71
#> $ weekday    <fct> rest_of_week, weekend, weekend, monday, rest_of_week, rest…
#> $ pred       <dbl> 12682, 10625, 10262, 13840, 11036, 10659, 10295, 9943, 833…
#> $ `lower-ci` <dbl> 11390, 9299, 8956, 11749, 9782, 9416, 9064, 8724, 7138, 68…
#> $ `upper-ci` <dbl> 14122, 12140, 11759, 16303, 12450, 12066, 11693, 11333, 97…
#> $ `lower-pi` <dbl> 8107, 6618, 6373, 8363, 6962, 6701, 6450, 6208, 5079, 4889…
#> $ `upper-pi` <dbl> 18870, 16223, 15714, 21784, 16638, 16124, 15626, 15145, 12…
plot(pred, "date", fitted_data = fitting_data, fitted_y = "count")
```

<img src="man/figures/README-unnamed-chunk-1-1.png" style="display: block; margin: auto;" />
