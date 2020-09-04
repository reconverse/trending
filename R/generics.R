#' S3 generics for trending
#'
#' These are generic functions used by the *trending* package, mostly used for
#' accessing content of various objects. See `?trending-accessors` for methods
#' relating to `trending` objects, and `trending_model-accessors` for methods
#' relating to `trending_model` objects.
#'
#' @seealso [trending-accessors](trending-accessors),
#'   [trending_model-accessors](trending_model-accessors)
#'
#' @param x the object to access information from
#' @param ... further arguments used in methods
#' @param data a `data.frame` to be used for fitting the model.
#'
#' @rdname trending-generics
#' @aliases trending-generics
#' @export
get_model <- function(x, ...) {
  UseMethod("get_model", x)
}

#' @export
#' @rdname trending-generics
get_formula <- function(x, ...) {
  UseMethod("get_formula", x)
}


#' @export
#' @rdname trending-generics
get_response <- function(x, ...) {
  UseMethod("get_response", x)
}


#' @export
#' @rdname trending-generics
get_family <- function(x, ...) {
  UseMethod("get_family", x)
}


#' @export
#' @rdname trending-generics
fit <- function(x, data, ...) {
  UseMethod("fit", x)
}
