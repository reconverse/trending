#' Modeling interface
#'
#' These functions wrappers around various modelling tools to ensure a
#' consistent input for *trendfit* functions. See details for available model
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
#' @param x an `trendfit_model` object
#'
#' @param ... Unused at this time.
#'
#' @return  A `trendfit_model` object (S3 class inheriting `list`), containing
#'   items which can be accessed by various accessors - see
#'   `?trendfit_model-accessors`
#'
#' @author Dirk Schumacher
#'
#' @aliases trendfit_model trendfit_models
#'
#' @export
#' @rdname trendfit_model
#' @aliases glm_model
glm_model <- function(formula, family) {

  if (!is.character(family)) {
    family <- deparse(substitute(family))
  }

  structure(
    eval(bquote(
      list(fit = fit_glm(formula = .(formula), family = .(family)))
    )),
    class = c("trendfit_glm", "trendfit_model")
  )
}


#' @export
#' @rdname trendfit_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula) {
  check_suggests("MASS")
  structure(
    eval(bquote(
      list(fit = fit_glm_nb(formula = .(formula)))
    )),
    class = c("trendfit_glm_nb", "trendfit_model")
  )
}


#' @export
#' @rdname trendfit_model
#' @aliases lm_model
lm_model <- function(formula) {
  structure(
    eval(bquote(
      list(fit = fit_lm(formula = .(formula)))
    )),
    class = c("trendfit_lm", "trendfit_model")
  )
}



#' @export
#' @rdname trendfit_model
#' @aliases brms_model
brms_model <- function(formula, family) {
  check_suggests("brms")
  structure(
    eval(bquote(
      list(fit = fit_brms(formula = .(formula), family = .(family)))
    )),
    class = c("trendfit_brms_nb", "trendfit_model")
  )
}



#' @export
#' @rdname trendfit_model
#' @aliases format.trendfit_model
format.trendfit_model <- function(x, ...) {
  paste0("Untrained trendfit model type: ", x[["model_class"]])
}



#' @export
#' @rdname trendfit_model
#' @aliases print.trendfit_model
print.trendfit_model <- function(x, ...) {
  cat(format(x, ...))
}
