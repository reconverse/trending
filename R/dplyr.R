# The following functions are needed to make `trending_fit` and
# `trending_prediciton` objects work nicely with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan. The idea is to think to an object in terms of its invariants
# (structural information that must be true for an object to be of class
# `trending_estimate`). Where an operation breaks these # invariants a tibble
# is returned instead of a `trending_estimate` object.

# -------------------------------------------------------------------------

# To quote "This function is a data frame specific helper.  Currently we are
# recommended to copy in to our own package but it may eventually find it's way
# in to one of the tidy packages."
df_reconstruct <- function(x, to) {
  attrs <- attributes(to)
  attrs$names <- names(x) # Keep column and row names of `x`
  attrs$row.names <- .row_names_info(x, type = 0L)
  # Otherwise copy over attributes of `to`
  attributes(x) <- attrs
  x
}

# -------------------------------------------------------------------------

# "new_bare_tibble() is a small wrapper around tibble::new_tibble() that also
# forces extra attributes to be dropped through the use of
# vctrs::new_data_frame(). In the future, new_tibble() might have an option
# to do this directly." See:
new_bare_tibble <- function(x) {
  # Strips all attributes off `x` since `new_tibble()` currently doesn't
  x <- new_data_frame(x)
  new_tibble(x, nrow = nrow(x))
}

# -------------------------------------------------------------------------

#' Check whether trending_estimate object invariants hold
#'
#' @param x data.frame to have it's invariants checked
#' @param to `trending_fit` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
trending_estimate_can_reconstruct <- function(x, to) {
  x_names <- names(x)
  col_names <- c(
    attr(to, "response"),
    attr(to, "predictors"),
    attr(to, "estimate"),
    attr(to, "ci_names"),
    attr(to, "pi_names")
  )
  if (!all(col_names %in% x_names)) FALSE else TRUE
}

# -------------------------------------------------------------------------

#' Function to reconstruct object of trending_estimate class
#'
#' Once you have encoded the invariant logic into
#' trending_estimate_can_reconstruct, we need a second function that applies
#' that check and either performs the actual reconstruction, or falls back to a
#' bare tibble.
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
trending_estimate_reconstruct <- function(x, to) {
  if (trending_estimate_can_reconstruct(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}

# -------------------------------------------------------------------------

# Need to define a few base R methods to ensure things work as expected

# -------------------------------------------------------------------------

#' @export
`[.trending_estimate` <- function(x, i, j, ...) {
  out <- NextMethod()
  trending_estimate_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`[<-.trending_estimate` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  trending_estimate_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`names<-.trending_estimate` <- function(x, value) {

  current_names <- names(x)

  response_var <- attr(x, "response")
  response_index <- match(response_var, current_names)
  attr(x, "response") <- value[response_index]

  predictor_vars <- attr(x, "predictors")
  predictor_index <- match(predictor_vars, current_names)
  attr(x, "predictors") <- value[predictor_index]

  estimate_var <- attr(x, "estimate")
  estimate_index <- match(estimate_var, current_names)
  attr(x, "estimate") <- value[estimate_index]

  ci_name_vars <- attr(x, "ci_names")
  ci_name_index <- match(ci_name_vars, current_names)
  attr(x, "ci_names") <- value[ci_name_index]

  pi_name_vars <- attr(x, "pi_names")
  pi_name_index <- match(pi_name_vars, current_names)
  attr(x, "pi_names") <- value[pi_name_index]

  out <- NextMethod()
  trending_estimate_reconstruct(out, x)
}

# -------------------------------------------------------------------------

# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct_trending_estimate <- function(data, template) {
  trending_estimate_reconstruct(data, template)
}

