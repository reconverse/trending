
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
tools, alongside functions for model selection. Whilst it is useful in
an interactive context, it’s main focus is to provide an intuitive
interface on which other packages can be developed
(e.g. [*trendbreaker*](https://github.com/reconhub/trendbreaker)).

## Main features

  - **Model specification:** Interfaces to common models through
    intuitive functions; `lm_model()`, `glm_model()`, `glm_nb_model`\*
    and `brms_model`\*\*.

  - **Model fitting and prediction:** Once specified, models can be fit
    to data and generate prediction intervals for future data using
    `fit()` and `predict()`.

  - **Model evaluation and selection:** `evaluate_resampling()`,
    `evaluate_aic()`, `evaluate_models()` and `select_model()`.

\*  Requires [MASS](https://CRAN.R-project.org/package=MASS)  
\*\* Requires [brms](https://CRAN.R-project.org/package=brms)

## Example usage

### An individual model

``` r
library(trendbreaker)  # for data
library(trending)      # for trend fitting
#> 
#> Attaching package: 'trending'
#> The following objects are masked from 'package:trendbreaker':
#> 
#>     brms_model, evaluate_aic, evaluate_models, evaluate_resampling,
#>     get_family, get_formula, get_model, get_response, glm_model,
#>     glm_nb_model, lm_model, select_model
library(dplyr, warn.conflicts = FALSE)  # for data manipulation

# load data
data(nhs_pathways_covid19)

# define a model
model = glm_nb_model(count ~ day + weekday)

# select last 6 weeks of data and group
first_date <- max(nhs_pathways_covid19$date, na.rm = TRUE) - 8*7
pathways_recent <- 
  nhs_pathways_covid19 %>% 
  filter(date >= first_date) %>% 
  group_by(date, day, weekday) %>% 
  summarise(count = sum(count), .groups = "drop")

# split data for fitting and prediction
dat <- 
  pathways_recent %>%
  group_by(date <= first_date + 6*7) %>% 
  group_split()

fitting_data <- dat[[2]]
pred_data <- select(dat[[1]], date, day , weekday)

fitted_model <- fit(model, fitting_data)

# confidence and prediction intervals
pred <- predict(fitted_model, pred_data)
glimpse(pred)
#> Rows: 14
#> Columns: 8
#> $ date       <date> 2020-05-15, 2020-05-16, 2020-05-17, 2020-05-18, 2020-05-1…
#> $ day        <int> 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71
#> $ weekday    <fct> rest_of_week, weekend, weekend, monday, rest_of_week, rest…
#> $ pred       <dbl> 12682.369, 10624.994, 10261.995, 13839.651, 11036.030, 106…
#> $ `lower-ci` <dbl> 11389.656, 9298.919, 8955.498, 11748.777, 9782.324, 9416.3…
#> $ `upper-ci` <dbl> 14121.804, 12140.175, 11759.094, 16302.627, 12450.410, 120…
#> $ `lower-pi` <dbl> 8107, 6617, 6373, 8362, 6962, 6701, 6450, 6208, 5078, 4889…
#> $ `upper-pi` <dbl> 18871, 16224, 15715, 21784, 16638, 16124, 15627, 15145, 12…
plot(pred, "date", fitted_data = fitting_data, fitted_y = "count")
```

<img src="man/figures/README-unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

### Model selection

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
