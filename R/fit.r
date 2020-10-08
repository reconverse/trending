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
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' 
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' 
#' fit(poisson_model, dat)
#' fit(list(poisson_model, negbin_model), dat)
#'
#' @export
fit <- function(x, data, ...) {
  UseMethod("fit", x)
}


#' @export
#' @rdname fit
#' @aliases fit.trending_model trending_model_fit
fit.trending_model <- function(x, data, ...) {
  x$fit(data)
}

#' @export
#' @rdname fit
#' @aliases trending_model_fit trending_model_fit_list
fit.list <- function(x, data, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entrys should be `trending_model` objects")
  }
  # res <- base_transpose(lapply(x, safe_fit, data, ...))
  res <- base_transpose(lapply(x, safely(fit), data, ...))
  names(res) <- c("fitted_trending_model", "fitting_warning", "fitting_error")
  class(res) <- c("trending_model_fit_list", class(res))
  res
}

# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #
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
