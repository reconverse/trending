#' Error handling generics
#'
#' `is_ok()` and `ok()` are generics used to filter successful outcomes from
#'   those that errored or gave warnings.
#'
#' @author Tim Taylor
#'
#' @param x \R object
#' @param warnings Include results in output that triggered warnings but
#'   not errors.  Defaults to `FALSE`.
#' @param unnest Should the warning and error columns be dropped and just the
#'   outcome returned. Defaults to `TRUE`

#' @param ... Not currently used.
#'
#' @return
#'
#' * `ok()`: returns rows from an object that did not error (or optionally
#'   produced a warning).
#'
#' * `is_ok`: returns TRUE or FALSE depending on whether the call succeeded
#'   or errored (or optionally produced a warning)
#'
#' @export
ok <- function(x, warnings, ...) {
  UseMethod("ok")
}

#' @rdname ok
#' @aliases ok.default
#' @export
ok.default <- function(x, warnings, ...) {
  not_implemented(x)
}

#' @rdname ok
#' @aliases ok.trending_fit
#' @export
ok.trending_fit <- function(x, warnings = TRUE, unnest = TRUE, ...) {
  fmodel <- attr(x, "fitted_model")
  fitted_model <- x[[fmodel]]
  ok <- !is.null(fitted_model[[1]])
  if (!warnings) {
    fwarnings <- attr(x, "fitting_warnings")
    fitting_warnings <- x[[fwarnings]]
    ok <- ok && is.null(fitting_warnings[[1]])
  }
  out <- x[ok,]
  if (nrow(out) && unnest) {
    out <- fitted_model[[1]]
  }
  out
}

#' @rdname ok
#' @aliases ok.trending_prediction
#' @export
ok.trending_prediction <- function(x, warnings = TRUE, unnest = TRUE, ...) {
  output_var <- attr(x, "output")
  output <- x[[output_var]]
  ok <- !is.null(output[[1]])
  if (!warnings) {
    pwarnings <- attr(x, "prediction_warnings")
    prediction_warnings <- x[[pwarnings]]
    ok <- ok && is.null(output[[1]])
  }
  out <- x[ok]
  if (nrow(out) && unnest) {
    out <- output[[1]]
  }
  out
}

#' @rdname ok
#' @aliases is_ok
#' @export
is_ok <- function(x, warnings, ...) {
  UseMethod("is_ok")
}

#' @rdname ok
#' @aliases is_ok.default
#' @export
is_ok.default <- function(x, warnings, ...) {
  not_implemented(x)
}

#' @rdname ok
#' @aliases is_ok.trending_fit
#' @export
is_ok.trending_fit <- function(x, warnings = TRUE, ...) {
  ok <- ok.trending_fit(x, warnings = warnings, unnest = FALSE)
  if (nrow(ok)) TRUE else FALSE
}

#' @rdname ok
#' @aliases is_ok.trending_prediction
#' @export
is_ok.trending_prediction <- function(x, warnings = TRUE, ...) {
  ok <- ok.trending_prediction(x, warnings = warnings, unnest = FALSE)
  if (nrow(ok)) TRUE else FALSE
}
