#' Modeling interface
#'
#' These functions wrappers around various modelling tools to ensure a
#' consistent input for *trending* functions. See details for available model
#' interfaces.
#'
#' @details The following interfaces are available:
#'
#' * `lm_model`: interface for linear models implemented in
#'   [stats::lm()].
#'
#' * `glm_model`: interface for generalised linear models (GLMs) implemented in
#'   [stats::glm()].
#'
#' * `glm_nb_model`: interface for negative binomial generalied linear models
#'   implemented in [MASS::glm.nb].
#'
#' * `brms_model`: interface for Bayesian regression models implemented in
#'   [brms::brm].
#'
#' @param formula The formula of the model, with the response variable on the
#'   left of a tilde symbol, and predictors on the right hand-side; variable
#'   names used in the formula will need to be matched by columns in the `data`
#'   input to other functions.
#'
#' @param family The model family to be used for the response variable.
#'
#' @param ... Further arguments passed underlying models: `lm` for [lm_model()],
#'   `glm` for [glm_model()], [MASS::glm.nb()] for `glm_nb_model`, [brms::brm()]
#'   for `brms_model`.  Not used for `print` and `format`.
#'
#' @return  A `trending_model` object (S3 class inheriting `list`), containing
#'   items which can be accessed by various accessors - see
#'   `?trending_model-accessors`.
#'
#' @author Dirk Schumacher
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' 
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' 
#' @aliases trending_models
#' @name trending_model
NULL


#' @export
#' @rdname trending_model
#' @aliases glm_model
glm_model <- function(formula, family, ...) {
  structure(
    eval(bquote(list(
      model_class = "glm",
      fit = function(data) {
        model <- glm(formula = .(formula), family = .(family), data = data, ...)
        model_fit(model, data)
      }
    ))),
    class = c("trending_glm", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula, ...) {
  structure(
    eval(bquote(list(
      model_class = "MASS::glm.nb",
      fit = function(data) {
        model <- MASS::glm.nb(formula = .(formula), data = data, ...)
        model_fit(model, data)
      }
    ))),
    class = c("trending_glm_nb", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases lm_model
lm_model <- function(formula, ...) {
  structure(
    eval(bquote(list(
      model_class = "lm",
      fit = function(data) {
        model <- lm(formula = .(formula), data = data, ...)
        model_fit(model, data)
      }
    ))),
    class = c("trending_lm", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases brms_model
brms_model <- function(formula, family, ...) {
  check_suggests("brms")
  structure(
    eval(bquote(list(
      model_class = "brms",
      fit = function(data) {
        model <- brms::brm(formula = .(formula), data = data, family = .(family), ...)
        model_fit(model, data)
      }
    ))),
    class = c("trending_brms", "trending_model")
  )
}


# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #
model_fit <- function(model, data) {
  out <- list(
    fitted_model = model,
    predict = function(newdata, alpha = 0.05, add_pi = TRUE, uncertain = TRUE) {

      # if no data given use the fitting data set
      if (missing(newdata)) {
        newdata <- data[all.vars(formula(model))]
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