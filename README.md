
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

This package is a work in progress. Version 0.1.0 has been released to
get wider feedback from the community. Please reach out to the authors
should you have any problems.

# Trending

*trending* aims to provides a coherent interface to several modelling
tools. Whilst it is useful in an interactive context, it’s main focus is
to provide an intuitive interface on which other packages can be
developed
(e.g. [*trendbreaker*](https://github.com/reconhub/trendbreaker)).

## Main features

  - **Model specification:** Interfaces to common models through
    intuitive functions; `lm_model()`, `glm_model()`, `glm_nb_model` and
    `brms_model`\*.

  - **Model fitting and prediction:** Once specified, models can be fit
    to data and generate confidence and prediction intervals for future
    data using `fit()` and `predict()`.

\*   Requires [brms](https://CRAN.R-project.org/package=brms)

## Installing the package

Once it is released on [CRAN](https://CRAN.R-project.org), you will be
able to install the stable version of the package with:

``` r
install.packages("trending")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("reconhub/trending", build_vignettes = TRUE)
```

# Resources

## Vignettes

An overview of *trending* is provided in the included vignette: \*
`vignette("Introduction", package = "trending")`

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue* system](https://github.com/reconhub/trending/issues). All other
questions should be posted on the **RECON** slack channel see
<https://www.repidemicsconsortium.org/forum/> for details on how to
join.

# Acknowledgements

  - Gavin Simpson; Our method to calculate prediction intervals follows
    one that he described on his
    [blog](https://fromthebottomoftheheap.net). \[1\] \[2\]

  - John Haman and Matthew Avery; Our implementation of prediction
    intervals was guided by their bootstrapped approach within the
    [ciTools](https://CRAN.R-project.org/package=ciTools) package.

<!-- end list -->

1.  <https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/>

2.  <https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-ii/>
