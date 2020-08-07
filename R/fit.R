#' Fitting and prediction
#'
#' Fit a model using data to obtain a
#' [`trending_model_fit`](trending_model_fit) object.
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model` or
#'   `brms_model`.
#' @param data a `data.frame` to be used to fit the model.
#' @param ... further arguments passed to other methods: `lm` for `lm_model`,
#'   `glm` for `glm_model`, `MASS::glm_nb` for `glm_nb_model`, `brms::brm` for
#'   `brms_model`.
#'
#'  #' @return
#'   - `fit`: The fitted model.
#'
#' @aliases fit.trending_model
#' @export
#' @rdname fitting_and_prediction
fit.trending_model <- function(x, data, ...) {
  ellipsis::check_dots_used()
  x$fit(data, ...)
}


# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #

fit_glm = function(formula, family) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- glm(formula = formula, family = family, data = data, ...)
    model_fit(model, formula)
  }
}


fit_glm_nb = function(formula) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- MASS::glm.nb(formula = formula, data = data, ...)
    model_fit(model, formula)
  }
}


fit_lm = function(formula) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- lm(formula = formula, data = data, ...)
    model_fit(model, formula)
  }
}


fit_brms = function(formula, family) {
  function(data, ...) {
    ellipsis::check_dots_used()
    model <- brms::brm(formula = formula, data = data, family = family, ...)
    model_fit(model, formula)
  }
}
