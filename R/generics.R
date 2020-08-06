#' S3 generics for trendfit
#'
#' These are generic functions used by the *trendfit* package, mostly used for
#' accessing content of various objects. See `?trendfit-accessors` for methods
#' relating to `trendfit` objects, and `trendfit_model-accessors` for methods
#' relating to `trendfit_model` objects.
#'
#' @seealso [trendfit-accessors](trendfit-accessors),
#'   [trendfit_model-accessors](trendfit_model-accessors)
#'
#' @param x the object to access information from
#'
#' @param ... further arguments used in methods
#'
#' @param data a `data.frame` to be used for fitting the model.
#'
#' @param new_data a `data.frame` containing data for which predictions are to
#'   be derived.
#'
#' @rdname trendfit-generics
#' @aliases trendfit-generics
#' @export
get_model <- function(x, ...) {
  UseMethod("get_model", x)
}

#' @export
#' @rdname trendfit-generics
get_formula <- function(x, ...) {
  UseMethod("get_formula", x)
}


#' @export
#' @rdname trendfit-generics
get_response <- function(x, ...) {
  UseMethod("get_response", x)
}


#' @export
#' @rdname trendfit-generics
get_family <- function(x, ...) {
  UseMethod("get_family", x)
}


#' @export
#' @rdname trendfit-generics
fit <- function(x, data, ...) {
  UseMethod("fit", x)
}

#' @export
#' @rdname trendfit-generics
fit_and_predict <- function(x, data, new_data, ...) {
  UseMethod("fit_and_predict", x)
}
