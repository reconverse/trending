#' Fitting for trending_model objects
#'
#' [fit()] fits a model using the given data to obtain an object of type
#'   `trending_model_fit` or  `trending_model_fit_list`.
#'
#' @param x The output of functions `lm_model`, `glm_model`, `glm_nb_model`, or
#'   brms_model or a list of these objects.
#'
#' @param data A `data.frame` to be used to train the model.
#'
#' @param ... Additional arguments passed to underlying models.
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model`, or
#'   brms_model
#'
#' @name trending_model_fit
NULL


#' @export
#' @rdname trending_model_fit
#' @aliases fit.trending_model
fit.trending_model <- function(x, data, ...) {
    x$fit(data)
}

#' @export
#' @rdname trending_model_fit
#' @aliases fit.list trending_model_fit_list
fit.list <- function(x, data, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entrys should be `trending_model` objects")
  }
  res <- base_transpose(lapply(x, safe_fit, data, ...))
  names(res) <- c("fitted_trending_model", "fitting_error", "fitting_warning")
  class(res) <- c("trending_model_fit_list", class(res))
  res
}

# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #
safe_fit <- function(x, data, ...) {
  tryCatch(
    list(fit(x, data, ...), NULL, NULL),
    error = function(e) list(NULL, e, NULL),
    warning = function(w) list(NULL, NULL, w)
  )
}


model_fit <- function(model, formula) {
  out <- list(
    fitted_model = model,
    predict = function(newdata, alpha = 0.05, add_pi = TRUE, uncertain = TRUE) {
      
      # if no data given use the fitting data set
      if (missing(newdata)) {
        newdata <- model$model
      }

      result <- add_confidence_interval(model, newdata, alpha)
      if (add_pi) {
        result <- add_prediction_interval(model, result, alpha, uncertain)
      }
      result
    }
  )
  class(out) <- c("trending_model_fit", class(out))
  out
}