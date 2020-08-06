#' @param object an `trending_model_fit` object
#' @param new_data a `data.frame` containing data for which predictions are to
#'   be derived.
#' @param alpha the alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived.
#' @param ... Not used.
#'
#' @return
#'   - `predict`: data.frame with prediction and prediction intervals.
#'
#' @aliases predict.trending_model_fit
#' @export
#' @rdname fitting_and_prediction
predict.trending_model_fit <- function(object, new_data, alpha = 0.05, ...) {
  ellipsis::check_dots_empty()
  object$predict(newdata = new_data, alpha = alpha)
}


# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #

add_prediction_interval <- function(model, data, alpha) {
  UseMethod("add_prediction_interval")
}


add_prediction_interval.default <- function(model, data, alpha) {
  suppressWarnings(
    ciTools::add_pi(
      tb = data,
      fit = model,
      alpha = alpha,
      names = c("lower", "upper")
    )
  )
}


add_prediction_interval.negbin <- function(model, data, alpha) {
  mu <- predict(model, newdata = data, type = "response")
  theta <- model$theta
  stopifnot(theta > 0)
  # this ignores the uncertainty around mu and theta
  dplyr::bind_cols(
    data,
    tibble::tibble(
      pred = mu,
      lower = stats::qnbinom(alpha / 2, mu = mu, size = theta),
      upper = stats::qnbinom(1 - alpha / 2, mu = mu, size = theta),
    )
  )
}


add_prediction_interval.brmsfit <- function(model, data, alpha) {
  fit <- predict(model, data)
  interval <- brms::predictive_interval(
    model,
    newdata = data,
    prob = 1 - alpha
  )
  dplyr::bind_cols(
    data,
    tibble::tibble(
      pred = fit[, 1],
      lower = interval[, 1],
      upper = interval[, 2]
    )
  )
}


model_fit <- function(model, formula) {
  out <- list(
    model = model,
    predict = function(newdata, alpha = 0.05) {
      suppressWarnings(
        suppressMessages(
          res <- add_prediction_interval(
            data = newdata,
            model = model,
            alpha = alpha
          )
        )
      )
      col_name <- as.character(formula[[2]])
      append_observed_column(res, res[[col_name]])
    }
  )
  class(out) <- c("trending_model_fit", class(out))
  out
}


append_observed_column <- function(data, value) {
  data[["observed"]] <- value
  data
}
