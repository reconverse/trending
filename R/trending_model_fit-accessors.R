#' Accessors for trending_model_fit objects
#'
#' These functions can be used to access information stored in
#' `trending_model_fit` objects. See details.
#'
#' @details The following accessors are available:
#'
#' * `get_model()`: get the fitted model stored in the object
#'
#' * `predict()`: get model predictions for user-provided data, including
#'   average predictions and prediction intervals
#'
#' @author Thibaut Jombart, Dirk Schumacher
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model` or
#'   `brms_model`
#'
#' @param ... Not currently used
#'
#' @aliases trending_model_fit-accessors trending_model_fit-class

#' @export
#' @rdname trending_model_fit-accessors
#' @aliases get_model.trending_model_fit
get_model.trending_model_fit <- function(x, ...) {
  ellipsis::check_dots_empty()
  x$model
}

#' @export
#' @rdname trending_model_fit-accessors
#' @aliases predict.trending_model_fit
#' @param object an `trending_model_fit` object
#' @param new_data a `data.frame` containing data for which predictions are to be
#'   derived
#' @param alpha the alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived
predict.trending_model_fit <- function(object, new_data, alpha = 0.05, ...) {
  ellipsis::check_dots_empty()
  object$predict(newdata = new_data, alpha = alpha)
}

#' @export
#' @rdname trending_model_fit-accessors
#' @aliases confidence.trending_model_fit
#' @param object an `trending_model_fit` object
#' @param new_data a `data.frame` containing data for which predictions are to be
#'   derived
#' @param alpha the alpha threshold to be used for confidence intervals,
#'   defaulting to 0.05, i.e. 95% confidence intervals are derived
confidence.trending_model_fit <- function(object, new_data, alpha = 0.05, ...) {
  ellipsis::check_dots_empty()
  object$confidence(newdata = new_data, alpha = alpha)
}



