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
#' @param add_pi Add a prediction interval to the output. Default TRUE.
#' @param uncertain Only used for glm models.  Default TRUE.  If FALSE
#'   uncertainty in the fitted paramaters is ignored when generating the
#'   prediction intervals.
#' @param ... Not currently used.
#' 
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' 
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' 
#' fitted_poisson <- fit(poisson_model, dat)
#' fitted_list <- fit(list(poisson_model, negbin_model), dat)
#' 
#' predict(fitted_poisson)
#' predict(fitted_list)
#'
#' @name trending_model_fit-prediction
NULL

#' @export
#' @rdname trending_model_fit-prediction
#' @aliases predict.trending_model_fit
predict.trending_model_fit <- function(object,
                                       new_data,
                                       alpha = 0.05,
                                       add_pi = TRUE,
                                       uncertain = TRUE,
                                       ...) {
  object$predict(newdata = new_data, alpha = alpha, add_pi = add_pi, uncertain = uncertain)
}

#' @export
#' @rdname trending_model_fit-prediction
#' @aliases predict.trending_model_fit_list
predict.trending_model_fit_list <- function(object,
                                            new_data,
                                            alpha = 0.05,
                                            add_pi = TRUE,
                                            uncertain = TRUE,
                                            ...) {
  fitted_model_var <- attr(object, "fitted_model")
  if (missing(new_data)) {
    res <- lapply(
      object[[fitted_model_var]],
      safely(predict),
      alpha = alpha,
      add_pi = add_pi,
      uncertain = uncertain
    )
  } else {
    res <- lapply(
      object[[fitted_model_var]],
      safely(predict),
      new_data = new_data,
      alpha = alpha,
      add_pi = add_pi,
      uncertain = uncertain
    )
  }
  res <- transpose(res)
  res <- tibble::tibble(
    output = res[[1]],
    prediction_warnings = res[[2]],
    prediction_errors = res[[3]]
  )
  new_bare_tibble(cbind(object, res))
}
