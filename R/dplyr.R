# The following functions are needed to make `trending_fit` and
# `trending_prediciton` objects work nicely with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan. The idea is to think to an object in terms of its invariants
# (structural information that must be true for an object to be of class
# `trending_fit` or `trending_prediction`). Where an operation breaks these
# invariants a tibble is returned instead of a `trending_fit` or
# `trending_prediction` object.

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

#' Check whether trending_fit object invariants hold
#'
#' @param x data.frame to have it's invariants checked
#' @param to `trending_fit` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
trending_fit_can_reconstruct <- function(x, to) {

  x_names <- names(x)

  fitted_model <- attr(to, "fitted_model")
  if (!(fitted_model %in% x_names)) return(FALSE)

  fitting_warnings <- attr(to, "fitting_warnings")
  if (!(fitting_warnings %in% x_names)) return(FALSE)

  fitting_errors <- attr(to, "fitting_errors")
  if (!(fitting_errors %in% x_names)) return(FALSE)

  TRUE
}

# -------------------------------------------------------------------------

#' Check whether trending_prediction object invariants hold
#'
#' @param x data.frame to have it's invariants checked
#' @param to `trending_prediction` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
trending_prediction_can_reconstruct <- function(x, to) {

  x_names <- names(x)

  output <- attr(to, "output")
  if (!(output %in% x_names)) return(FALSE)

  prediction_warnings <- attr(to, "prediction_warnings")
  if (!(prediction_warnings %in% x_names)) return(FALSE)

  prediction_errors <- attr(to, "prediction_errors")
  if (!(prediction_errors %in% x_names)) return(FALSE)

  TRUE
}

# -------------------------------------------------------------------------

#' Function to reconstruct object of trending_fit class
#'
#' Once you have encoded the invariant logic into
#' trending_fit_can_reconstruct, we need a second function that applies
#' that check and either performs the actual reconstruction, or falls back to a
#' bare tibble.
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
trending_fit_reconstruct <- function(x, to) {
  if (trending_fit_can_reconstruct(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}

# -------------------------------------------------------------------------

#' Function to reconstruct object of trending_prediction class
#'
#' Once you have encoded the invariant logic into
#' trending_prediction_can_reconstruct, we need a second function that applies
#' that check and either performs the actual reconstruction, or falls back to a
#' bare tibble.
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
trending_prediction_reconstruct <- function(x, to) {
  if (trending_prediction_can_reconstruct(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}

# Need to define a few base R methods to ensure things work as expected

# -------------------------------------------------------------------------

#' @export
`[.trending_fit` <- function(x, i, j, ...) {
  out <- NextMethod()
  trending_fit_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`[.trending_prediction` <- function(x, i, j, ...) {
  out <- NextMethod()
  trending_prediction_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`[<-.trending_fit` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  trending_fit_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`[<-.trending_prediction` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  trending_prediction_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`names<-.trending_fit` <- function(x, value) {
  current_names <- names(x)

  fm_var <- attr(x, "fitted_model")
  fm_index <- which(current_names %in% fm_var)
  attr(x, "fitted_model") <- value[fm_index]

  fw_var <- attr(x, "fitting_warnings")
  fw_index <- which(current_names %in% fw_var)
  attr(x, "fitting_warnings") <- value[fw_index]

  fe_var <- attr(x, "fitting_errors")
  fe_index <- which(current_names %in% fe_var)
  attr(x, "fitting_errors") <- value[fe_index]

  out <- NextMethod()
  trending_fit_reconstruct(out, x)
}

# -------------------------------------------------------------------------

#' @export
`names<-.trending_prediction` <- function(x, value) {
  current_names <- names(x)

  # This needs pulling in to a function at some point
  output_var <- attr(x, "output")
  output_index <- which(current_names %in% output_var)
  attr(x, "output") <- value[output_index]

  pw_var <- attr(x, "prediction_warnings")
  pw_index <- which(current_names %in% pw_var)
  attr(x, "prediction_warnings") <- value[pw_index]

  pe_var <- attr(x, "prediction_errors")
  pe_index <- which(current_names %in% pe_var)
  attr(x, "prediction_errors") <- value[pe_index]

  out <- NextMethod()
  trending_prediction_reconstruct(out, x)
}

# -------------------------------------------------------------------------

# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct_trending_fit <- function(data, template) {
  trending_fit_reconstruct(data, template)
}

# -------------------------------------------------------------------------

# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct_trending_prediction <- function(data, template) {
  trending_prediction_reconstruct(data, template)
}
