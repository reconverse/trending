#' Accessors for trending_model_fit objects
#'
#' These functions can be used to access information stored in
#' `trending_model_fit` objects. See details.
#'
#' @details The following accessors are available:
#'
#' * `get_model()`: get the fitted model stored in the object
#'
#'
#' @author Thibaut Jombart, Dirk Schumacher
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model` or
#'   `brms_model`
#'
#' @param ... Not currently used
#'
#' @rdname trending_model_fit-accessors
#' @aliases get_model.trending_model_fit
#' @export
get_model.trending_model_fit <- function(x, ...) {
  x$fitted_model
}
