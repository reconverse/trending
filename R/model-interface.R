#' Modeling interface
#'
#' These functions wrappers around various modelling tools to ensure a
#' consistent input for *trending* functions. See details for available model
#' interfaces.
#'
#' @details The following interfaces are available:
#'
#' * `lm_model`: interface for linear models implemented in
#'   [`stats::lm`](stats::lm)
#'
#' * `glm_model`: interface for generalised linear models (GLMs) implemented in
#'   `[stats::glm](stats::glm)`
#'
#' * `glm_nb_model`: interface for negative binomial generalied linear models
#'   implemented in [`MASS::glm_nb`](MASS::glm_nb)
#'
#' * `brms_model`: interface for Bayesian regression models implemented in
#'   [`brms::brm`](brms::brm)
#'
#' @param formula the formula of the model, with the response variable on the
#'   left of a tilde symbol, and predictors on the right hand-side; variable
#'   names used in the formula will need to be matched by columns in the `data`
#'   input to other functions
#'
#' @param family the model family to be used for the response variable
#'
#' @param x an `trending_model` object
#'
#' @param ... further arguments passed to other methods: `lm` for `lm_model`,
#'   `glm` for `glm_model`, `MASS::glm_nb` for `glm_nb_model`, `brms::brm` for
#'   `brms_model`
#'
#' @return  A `trending_model` object (S3 class inheriting `list`), containing
#'   items which can be accessed by various accessors - see
#'   `?trending_model-accessors`
#'
#' @author Dirk Schumacher
#'
#' @aliases trending_model trending_models
#'
#' @export
#' @rdname trending_model
#' @aliases glm_model
glm_model <- function(formula, family) {

  if (!is.character(family)) {
    family <- deparse(substitute(family))
  }

  structure(
    eval(bquote(list(
      train = function(data, ...) {
        ellipsis::check_dots_used()
        model <- glm(formula = .(formula), family = .(family), data = data, ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_glm", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula) {
  check_suggests("MASS")
  structure(
    eval(bquote(list(
      train = function(data, ...) {
        ellipsis::check_dots_used()
        model <- MASS::glm.nb(formula = .(formula), data = data, ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_glm_nb", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases lm_model
lm_model <- function(formula) {
  ellipsis::check_dots_used()
  structure(
    eval(bquote(list(
      train = function(data, ...) {
        ellipsis::check_dots_used()
        model <- lm(formula = .(formula), data = data, ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_lm", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases brms_model
brms_model <- function(formula, family) {
  check_suggests("brms")
  ellipsis::check_dots_used()
  structure(
    eval(bquote(list(
      train = function(data, ...) {
        ellipsis::check_dots_used()
        model <- brms::brm(
          formula = .(formula),
          data = data,
          family = .(family),
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
    ))),
    class = c("trending_brms_nb", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases format.trending_model
format.trending_model <- function(x, ...) {
  paste0("Untrained trending model type: ", x[["model_class"]])
}



#' @export
#' @rdname trending_model
#' @aliases print.trending_model
print.trending_model <- function(x, ...) {
  cat(format(x, ...))
}





# =============
# = INTERNALS =
# =============
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
