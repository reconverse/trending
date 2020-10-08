#' Accessors for trending_model_fit objects
#'
#' These functions can be used to access information stored in
#' `trending_model_fit` objects. See details.
#' 
#' @param x Object of class [`trending_model`] (i.e. the output of functions
#'   [lm_model()], [glm_model()], [glm_nb_model()], or [brms_model()]).
#' @param ... Not currently used
#'
#' @details The following accessors are available:
#' 
#' * `get_model()`: get the fitted model stored in the object
#'
#' @name trending_model_fit_accessors
NULL

#  -----------------------------------------------------------------------------
#' @rdname trending_model_fit_accessors
#' @export
get_model <- function(x, ...) {
  UseMethod("get_model", x)
}

#' @rdname trending_model_fit_accessors
#' @aliases get_model.trending_model_fit
#' @export
get_model.trending_model_fit <- function(x, ...) {
  x$fitted_model
}