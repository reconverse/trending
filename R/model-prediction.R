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
#' @param uncertain Only used for glm models.  Default TRUE.  If FALSE 
#'   uncertainty in the fitted paramaters is ignored when generating the
#'   prediction intervals.
#' @param ... Not currently used.
#'
#' @name trending_model_fit-prediction
NULL

safe_predict <- function(object, new_data, alpha, interval, uncertain, ...) {
  tryCatch(
    list(predict(object = object, new_data = new_data, alpha = alpha,
                 interval = interval, uncertain = uncertain, ...),
         NULL,
         NULL
        ),
    error = function(e) list(NULL, e, NULL),
    warning = function(w) list(NULL, NULL, w)
  )
}

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
                                           uncertain = TRUE,
                                           ...) {
  ellipsis::check_dots_empty()
  object$predict(newdata = new_data, 
                 alpha = alpha, 
                 interval = interval,
                 uncertain = uncertain)
}



#' @export
#' @rdname trending_model_fit-prediction
#' @aliases predict.trending_model_fit_list
predict.trending_model_fit_list <- function(object, 
                                            new_data,
                                            alpha = 0.05,
                                            interval = c("both", "ci", "pi", "none"),
                                            uncertain = TRUE,
                                            ...) {
  ellipsis::check_dots_empty()
  object <- combine_safe_results(object)
  if (missing(new_data)) {
    res <- base_transpose(
      lapply(
        object, 
        safe_predict, 
        alpha = alpha,
        interval = interval, 
        uncertain = uncertain
      )
    )
  } else {
    res <- base_transpose(
      lapply(
        object,
        safe_predict,
        new_data = new_data,
        alpha = alpha,
        interval = interval,
        uncertain = uncertain
      )
    )
  }
  names(res) <- c("output", "fitting_error", "fitting_warning")
  res
}