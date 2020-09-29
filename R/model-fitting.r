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
  if (!all(purrr::map_lgl(x, inherits, "trending_model"))) {
    stop("list entrys should be `trending_model` objects")
  }
  res <- purrr::transpose(purrr::map(x, purrr::safely(fit), data))
  class(res) <- c("trending_model_fit_list", class(res))
  res
}

# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #
lm_model_fit <- function(model, formula) {
  out <- list(
    model = model,
    predict = function(newdata,
                       alpha = 0.05,
                       interval = c("both", "ci", "pi", "none")) {

      interval <- match.arg(interval)

      # if no data given use the fitting data set
      if (missing(newdata)) {
        newdata <- model$model
      }

      # calculate prediction irrespective of intervals
      result <- cbind(newdata, pred = predict(model, newdata))

      # add confidence interval
      if (interval == "both" || interval == "ci") {
        result <- add_confidence_interval(
          model = model,
          data = result,
          alpha = alpha
        )
      }

      # add prediction interval
      if (interval == "both" || interval == "pi") {
        result <- add_prediction_interval(
          model = model,
          data = result,
          alpha = alpha
        )
      }
      class(result) <- c("trending_model_prediction", class(result))
      result
    }
  )
  class(out) <- c("trending_model_fit_lm", "trending_model_fit", class(out))
  out
}


glm_model_fit <- function(model, formula) {
  out <- list(
    model = model,
    predict = function(newdata,
                       alpha = 0.05,
                       interval = c("both", "ci", "pi", "none"),
                       simulate_pi = FALSE,
                       uncertain = TRUE) {

      interval <- match.arg(interval)

      # if no data given use the fitting data set
      if (missing(newdata)) {
        newdata <- model$model
      }

      # calculate prediction irrespective of intervals
      result <- cbind(newdata, pred = predict(model, newdata, type = "response"))

      # add confidence interval
      if (interval == "both" || interval == "ci") {
        result <- add_confidence_interval(
          model = model,
          data = result,
          alpha = alpha,
          simulate_pi = simulate_pi,
          uncertain = uncertain
        )
      }

      # add prediction interval
      if (interval == "both" || interval == "pi") {
        result <- add_prediction_interval(
          model = model,
          data = result,
          alpha = alpha,
          simulate_pi = simulate_pi,
          uncertain = uncertain
        )
      }

      class(result) <- c("trending_model_prediction", class(result))
      result
    }
  )
  class(out) <- c("trending_model_fit_glm", "trending_model_fit", class(out))
  out
}


brms_model_fit <- function(model, formula) {
  out <- list(
    model = model,
    predict = function(newdata,
                       alpha = 0.05,
                       interval = c("both", "ci", "pi", "none")) {

      interval <- match.arg(interval)

      # if no data given use the fitting data set
      if (missing(newdata)) {
        newdata <- model$data
      }

      # calculate prediction irrespective of intervals
      result <- cbind(newdata, pred = predict(model, newdata)[, 1])

      # add confidence interval
      if (interval == "both" || interval == "ci") {
        result <- add_confidence_interval(
          model = model,
          data = result,
          alpha = alpha
        )
      }

      # add prediction interval
      if (interval == "both" || interval == "pi") {
        result <- add_prediction_interval(
          model = model,
          data = result,
          alpha = alpha
        )
      }

      class(result) <- c("trending_model_prediction", class(result))
      result
    }
  )
  class(out) <- c("trending_model_fit_brms", "trending_model_fit", class(out))
  out
}
