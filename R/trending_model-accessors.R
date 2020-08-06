#' Accessors for trendfit_model objects
#'
#' These functions can be used to access information stored in `trendfit_model`
#' objects. See details.
#'
#' @details The following accessors are available:
#'
#' * `get_formula()`: get the formula used to model temporal trends
#'
#' * `get_response()`: get the name of the response variable
#'
#' * `get_family()`: get the model family, indicating the type of distribution
#'   assumed for the response variable
#'
#' @author Thibaut Jombart, Dirk Schumacher
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model`, or
#'   brms_model
#'
#' @param ... further arguments passed to other methods
#'
#' @aliases trendfit_model-accessors trendfit_model-class
#'
#' @export
#' @rdname trendfit_model-accessors
#' @aliases get_formula.trendfit_model
get_formula.trendfit_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  as.list(environment(x$fit))$formula
}


#' @export
#' @rdname trendfit_model-accessors
#' @aliases get_response.trendfit_model
get_response.trendfit_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  form <- get_formula(x)
  as.character(form)[2]
}


#' @export
#' @rdname trendfit_model-accessors
#' @aliases get_family.trendfit_model
get_family.trendfit_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  if (inherits(x, "trendfit_lm")) {
    "gaussian"
  } else {
    as.list(environment(x$fit))$family
  }
}


