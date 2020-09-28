#' Predict methods
#'
#' These functions can be used to generated estimated values and associated
#'   confidence/prediction intervals for trending_model_fit objects.
#' 
#' @param object A `trending_model_fit` or `trending_model_fit_list` object.
#' @param new_data A `data.frame` containing data for which predictions are to
#'   be derived.
#' @param alpha The alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived.
#' @param interval Which interval to add to the data.  Can be one of `ci`
#'   (confidence interval), `pi` (prediction interval), `both` (both intervals)
#'   or `none` (no intervals added).
#' @param simulate_pi Only used for glm models. Default FALSE. If TRUE then
#'   prediction intervals are generated using simulation. 
#' @param uncertain Only used for glm models.  Default TRUE.  If FALSE (and
#'   `simulate_pi` is also FALSE) uncertainty in the fitted paramaters is
#'   ignored when generating the prediction intervals.
#' @param ... Not currently used.
#'
#' @name trending_model_fit-prediction
NULL

#' @export
#' @rdname trending_model_fit-prediction
#' @aliases predict.trending_model_fit
predict.trending_model_fit <- function(object, 
                                       new_data,
                                       alpha = 0.05,
                                       interval = c("both", "ci", "pi", "none"),
                                       ...) {
  object$predict(newdata = new_data, alpha = alpha, interval = interval)
}


#' @export
#' @rdname trending_model_fit-prediction
#' @aliases predict.trending_model_fit_glm
predict.trending_model_fit_glm <- function(object,
                                           new_data,
                                           alpha = 0.05,
                                           interval = c("both", "ci", "pi", "none"),
                                           simulate_pi = FALSE,
                                           uncertain = TRUE,
                                           ...) {
  ellipsis::check_dots_empty()
  object$predict(newdata = new_data, 
                 alpha = alpha, 
                 interval = interval,
                 simulate_pi = simulate_pi,
                 uncertain = uncertain)
}



#' @export
#' @rdname trending_model_fit-prediction
#' @aliases predict.trending_model_fit_list
predict.trending_model_fit_list <- function(object, 
                                            new_data,
                                            alpha = 0.05,
                                            interval = c("both", "ci", "pi", "none"),
                                            simulate_pi = FALSE,
                                            uncertain = TRUE,
                                            ...) {
  ellipsis::check_dots_empty()
  object <- combine_safe_results(object)
  if (missing(new_data)) {
    res <- purrr::transpose(
      purrr::map(
        object, 
        purrr::safely(predict), 
        alpha = alpha,
        interval = interval, 
        simulate_pi = simulate_pi,
        uncertain = uncertain
      )
    )
  } else {
    res <- purrr::transpose(
      purrr::map(
        object,
        purrr::safely(predict),
        new_data = new_data,
        alpha = alpha,
        interval = interval,
        simulate_pi = simulate_pi,
        uncertain = uncertain
      )
    )
  }
  class(res) <- c("trending_model_prediction_list", class(res))
  res
}