# The following functions are needed to make `trending_model_fit_list` objects
# work nicely with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan.  The idea is to think to an trending_model_fit_list object
# in terms of it's invariants (structural information that must be true for an
# object to be of class trending_model_fit_list). Where an operation breaks
# these invariants a tibble is returned instead of an trending_model_fit_list
# object.


#' Check whether trending_model_fit_list object invariants hold
#'
#' @param x data.frame to have it's invariants checked
#' @param to `trending_model_fit_list` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
trending_model_fit_list_can_reconstruct <- function(x, to) {
  
  x_names <- names(x)
  to_names <- names(to)

  # Must have same number of columns
  if (length(x_names) != length(to_names)) {
    return(FALSE)
  }

  # Column order doesn't matter
  x_names <- sort(x_names)
  to_names <- sort(to_names)

  # Names must be exactly the same
  if (!identical(x_names, to_names)) {
    return(FALSE)
  }

  # check underlying data is the same
  x_df <- vctrs::new_data_frame(x)
  to_df <- vctrs::new_data_frame(to)
  x_df <- x_df[, to_names, drop = FALSE]
  to_df <- to_df[, to_names, drop = FALSE]

  if(!identical(x_df, to_df)) {
    FALSE
  }
 
  TRUE
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Function to reconstruct object of trending_model_fit_list class
#'
#' Once you have encoded the invariant logic into
#' trending_model_fit_list_can_reconstruct, we need a second function that
#' applies that check and either performs the actual reconstruction, or falls
#' back to a bare tibble.
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
trending_model_fit_list_reconstruct <- function(x, to) {
  if (trending_model_fit_list_can_reconstruct(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
# This function is a data frame specific helper.  Currently we are recommended
# to copy in to our own package but it may evenutally find it's way in to one of
# the tidy packages. See:
# https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat
df_reconstruct <- function(x, to) {
  attrs <- attributes(to)

  # Keep column and row names of `x`
  attrs$names <- names(x)
  attrs$row.names <- .row_names_info(x, type = 0L)

  # Otherwise copy over attributes of `to`
  attributes(x) <- attrs
  x
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# new_bare_tibble() is a small wrapper around tibble::new_tibble() that also
# forces extra attributes to be dropped through the use of
# vctrs::new_data_frame(). In the future, new_tibble() might have an option
# to do this directly. See:
# https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat
new_bare_tibble <- function(x) {
  # Strips all attributes off `x` since `new_tibble()` currently doesn't
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x))
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Need to define a few base R methods to ensure things work as expected

#' @export
`[.trending_model_fit_list` <- function(x, i, j, ...) {
  out <- NextMethod()
  trending_model_fit_list_reconstruct(out, x)
}

#' @export
`[<-.trending_model_fit_list` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  trending_model_fit_list_reconstruct(out, x)
}

#' @export
`$<-.trending_model_fit_list` <- function(x, name, value) {
  out <- NextMethod()
  trending_model_fit_list_reconstruct(out, x)
}

#' @export
`names<-.trending_model_fit_list` <- function(x, value) {
  
  current_names <- names(x)

  nms_var <- attr(x, "nms")
  if (!is.null(nms_var)) {
    nms_index <- which(current_names %in% nms_var)
    attr(x, "nms") <- value[nms_index]
  }

  data_var <- attr(x, "data")
  data_index <- which(current_names %in% data_var)
  attr(x, "data") <- value[data_index]

  fitted_model_var <- attr(x, "fitted_model")
  fitted_model_index <- which(current_names %in% fitted_model_var)
  attr(x, "fitted_model") <- value[fitted_model_index]

  fitting_warnings_var <- attr(x, "fitting_warnings")
  fitting_warnings_index <- which(current_names %in% fitting_warnings_var)
  attr(x, "fitting_warnings") <- value[fitting_warnings_index]

  fitting_errors_var <- attr(x, "fitting_errors")
  fitting_errors_index <- which(current_names %in% fitting_errors_var)
  attr(x, "fitting_errors") <- value[fitting_errors_index]

  out <- NextMethod()
  trending_model_fit_list_reconstruct(out, x)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct_trending_model_fit_list <- function(data, template) {
  trending_model_fit_list_reconstruct(data, template)
}
# -------------------------------------------------------------------------
