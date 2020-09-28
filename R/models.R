#' Modeling interface
#'
#' These functions wrappers around various modelling tools to ensure a
#' consistent input for *trending* functions. See details for available model
#' interfaces.
#'
#' @details The following interfaces are available:
#'
#' * `lm_model`: interface for linear models implemented in
#'   [`stats::lm`](stats::lm).
#'
#' * `glm_model`: interface for generalised linear models (GLMs) implemented in
#'   `[stats::glm](stats::glm)`.
#'
#' * `glm_nb_model`: interface for negative binomial generalied linear models
#'   implemented in [`MASS::glm_nb`](MASS::glm_nb).
#'
#' * `brms_model`: interface for Bayesian regression models implemented in
#'   [`brms::brm`](brms::brm).
#'
#' @param formula The formula of the model, with the response variable on the
#'   left of a tilde symbol, and predictors on the right hand-side; variable
#'   names used in the formula will need to be matched by columns in the `data`
#'   input to other functions.
#'
#' @param family The model family to be used for the response variable.
#'
#' @param x A `trending_model` or `trending_model_fit` object.
#'
#' @param ... Further arguments passed underlying models: `lm` for `lm_model`,
#'   `glm` for `glm_model`, `MASS::glm_nb` for `glm_nb_model`, `brms::brm` for
#'   `brms_model`.  Not used for `print` and `format`.
#'
#' @return  A `trending_model` object (S3 class inheriting `list`), containing
#'   items which can be accessed by various accessors - see
#'   `?trending_model-accessors`.
#'
#' @author Dirk Schumacher
#'
#' @aliases trending_models
#' @name trending_model
NULL


#' @export
#' @rdname trending_model
#' @aliases glm_model
glm_model <- function(formula, family, ...) {
  if (!is.character(family)) {
    family <- deparse(substitute(family))
  }
  structure(
    eval(bquote(list(
      model_class = "glm",
      fit = function(data) {
        model <- glm(formula = .(formula), family = .(family), data = data, ...)
        glm_model_fit(model, formula)
      }
    ))),
    class = c("trending_glm", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula, ...) {
  check_suggests("MASS")
  structure(
    eval(bquote(list(
      model_class = "MASS::glm.nb",
      fit = function(data) {
        model <- MASS::glm.nb(formula = .(formula), data = data, ...)
        glm_model_fit(model, formula)
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
        lm_model_fit(model, formula)
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
        brms_model_fit(model, formula)
      }
    ))),
    class = c("trending_brms", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases format.trending_model
format.trending_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  paste0("Untrained trending model type: ", x[["model_class"]])
}


#' @export
#' @rdname trending_model
#' @aliases print.trending_model
print.trending_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  cat(format(x))
}


#' @export
#' @rdname trending_model
#' @aliases format.trending_model_fit
format.trending_model_fit <- function(x, ...) {
  ellipsis::check_dots_empty()
  tmp <- append("Fitted trending model:\n", utils::capture.output(x$model))
  paste(tmp,  collapse = "\n")
}


#' @export
#' @rdname trending_model
#' @aliases print.trending_model_fit
print.trending_model_fit <- function(x, ...) {
  ellipsis::check_dots_empty()
  cat(format(x))
}
