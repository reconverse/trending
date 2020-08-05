#' Train a model
#'
#' Train a model using data to obtain a
#' [`trending_model_fit`](trending_model_fit) object.
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model` or
#'   `brms_model`.
#' @param data a `data.frame` to be used to train the model.
#' @param ... further arguments passed to other methods: `lm` for `lm_model`,
#'   `glm` for `glm_model`, `MASS::glm_nb` for `glm_nb_model`, `brms::brm` for
#'   `brms_model`.
#' @export
#' @aliases train.trending_model
train.trending_model <- function(x, data, ...) {
  ellipsis::check_dots_used()
  x$train(data, ...)
}


# =============
# = INTERNALS =
# =============
train_glm = function(formula, family) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- stats::glm(formula = formula, family = family, data = data, ...)
    model_fit(model, formula)
  }
}


train_glm_nb = function(formula) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- MASS::glm.nb(formula = formula, data = data, ...)
    model_fit(model, formula)
  }
}

train_lm = function(formula) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- stats::lm(formula = formula, data = data, ...)
    model_fit(model, formula)
  }
}

train_brms = function(formula, family) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- brms::brm(
      formula = formula,
      data = data,
      family = family,
      ...
    )
    res <- list(
      model = model,
      predict = function(newdata, alpha = 0.05) {
        res <- add_prediction_interval(
          data = newdata,
          model = model,
          alpha = alpha
        )
        col_name <- as.character(formula[[2]])
        append_observed_column(res, res[[col_name]])
      }
    )
    class(res) <- c("trending_model_fit", class(res))
    res
  }
}

add_prediction_interval <- function(model, data, alpha) {
  UseMethod("add_prediction_interval")
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
