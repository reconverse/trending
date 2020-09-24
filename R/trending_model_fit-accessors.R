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
#' @param object a `trending_model_fit` object
#' @param new_data a `data.frame` containing data for which predictions are to
#'   be derived
#' @param alpha the alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived
predict.trending_model_fit <- function(object, new_data, alpha = 0.05, ...) {
  ellipsis::check_dots_empty()
  object$predict(newdata = new_data, alpha = alpha)
}


#' @export
#' @rdname trending_model_fit-accessors
#' @aliases predict.trending_model_fit
#' @param object a `trending_model_fit_list` object
#' @param new_data a `data.frame` containing data for which predictions are to
#'   be derived
#' @param alpha the alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived
predict.trending_model_fit_list <- function(object, new_data, alpha = 0.05, ...) {
  ellipsis::check_dots_empty()
  object <- combine_safe_results(object)
  if (missing(new_data)) {
    res <- purrr::transpose(purrr::map(object, purrr::safely(predict), alpha = alpha))
  } else {
    res <- purrr::transpose(purrr::map(object, purrr::safely(predict), new_data = new_data, alpha = alpha))
  }
  class(res) <- c("trending_model_prediction_list", class(res))
  res
}