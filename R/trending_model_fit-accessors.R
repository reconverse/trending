
#' Accessors for trendbreaker_model_fit objects
#'
#' These functions can be used to access information stored in
#' `trendbreaker_model_fit` objects. See details.
#'
#' @details The following accessors are available:
#'
#' * `get_model()`: get the fitted model stored in the object
#'
#' * `predict()`: get model predictions for user-provided data, including
#' average predictions and prediction intervals
#'
#' @author Thibaut Jombart, Dirk Schumacher
#'
#' @param x the output of functions `lm_model`, `glm_model`, or
#'   `glm_nb_model`
#'
#' @param ... further arguments passed to other methods
#'
#' @aliases trendbreaker_model_fit-accessors trendbreaker_model_fit-class

#' @export
#' @rdname trendbreaker_model_fit-accessors
#' @aliases get_model.trendbreaker_model_fit
get_model.trendbreaker_model_fit <- function(x, ...) {
  ellipsis::check_dots_empty()
  x$model
}



