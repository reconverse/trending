#' Accessors for trending_model objects
#'
#' These functions can be used to access information stored in
#' [`trending_model`] objects. See details.
#' 
#' @param x Object of class [`trending_model_fit`] (i.e. the output from fitting
#'   a [`trending_model`]).
#' @param ... Not currently used by any methods.
#'
#' @details The following accessors are available:

#'
#' * `get_formula()`: get the formula used to model temporal trends;
#'
#' * `get_response()`: get the name of the response variable.
#'
#' @name trending_model_accessors
NULL


# ------------------------------------------------------------------------------
#' @export
#' @rdname trending_model_accessors
get_formula <- function(x, ...) {
  UseMethod("get_formula", x)
}

#' @rdname trending_model_accessors
#' @export
get_formula.trending_model <- function(x, ...) {
  as.list(environment(x$fit))$formula
}


# ------------------------------------------------------------------------------
#' @export
#' @rdname trending_model_accessors
get_response <- function(x, ...) {
  UseMethod("get_response", x)
}

#' @export
#' @rdname trending_model_accessors 
get_response.trending_model <- function(x, ...) {
  form <- get_formula(x)
  as.character(form)[2]
}



