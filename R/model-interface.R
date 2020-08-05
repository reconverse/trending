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
#' @param ... Unused at this time.
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
    eval(bquote(
      list(train = train_glm(formula = .(formula), family = .(family)))
    )),
    class = c("trending_glm", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula) {
  check_suggests("MASS")
  structure(
    eval(bquote(
      list(train = train_glm_nb(formula = .(formula)))
    )),
    class = c("trending_glm_nb", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases lm_model
lm_model <- function(formula) {
  structure(
    eval(bquote(
      list(train = train_lm(formula = .(formula)))
    )),
    class = c("trending_lm", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases brms_model
brms_model <- function(formula, family) {
  check_suggests("brms")
  structure(
    eval(bquote(
      list(train = train_brms(formula = .(formula), family = .(family)))
    )),
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
