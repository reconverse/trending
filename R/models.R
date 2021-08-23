#' Modeling interface
#'
#' These functions wrap various modelling tools to ensure a consistent input
#'   for *trending* functions. They work by capturing the underlying model call
#'   and decoupling it from the data specification. This makes it easy to use
#'   the same underlying model specification and fitting procedure across
#'   different data sets. See details for available model interfaces.
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
#'   implemented in [MASS::glm.nb()].
#'
#' * `brm_model`: interface for Bayesian regression models implemented in
#'   [brms::brm()].
#'
#' These interfaces will accept the same inputs as the underlying model
#' functions but do not require, nor will they accept, a `data` argument.
#' Fitting is handled instead by the [fit()] generic and associated methods.
#'
#' @param formula The formula of the model, with the response variable on the
#'   left of a tilde symbol, and predictors on the right hand-side; variable
#'   names used in the formula will need to be matched by columns in the `data`
#'   input to other functions.
#' @param family Link function to be used for the glm model.
#' @param ... Further arguments passed to the underlying models with the
#'   exception of `data`.
#'
#' @return  A `trending_model` object.
#'
#' @author Tim Taylor
#'
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

# -------------------------------------------------------------------------

#' @export
#' @rdname trending_model
#' @aliases lm_model
lm_model <- function(formula, ...) {
  stopifnot(inherits(formula, "formula"))
  args <- as.list(substitute(list(...))[-1L])
  args$formula = as.call(formula)
  trending_model("lm", args)
}

# -------------------------------------------------------------------------

#' @export
#' @rdname trending_model
#' @aliases glm_model
glm_model <- function(formula, family = gaussian, ...) {
  stopifnot(inherits(formula, "formula"))
  args <- as.list(substitute(list(...))[-1L])
  args$formula = formula
  args$family = substitute(family)
  trending_model("glm", args)
}

# -------------------------------------------------------------------------

#' @export
#' @rdname trending_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula, ...) {
  stopifnot(inherits(formula, "formula"))
  args <- as.list(substitute(list(...))[-1L])
  args$formula = formula
  trending_model("glm.nb", args)
}

# -------------------------------------------------------------------------

#' @export
#' @rdname trending_model
#' @aliases brm_model
brm_model <- function(formula, ...) {
  stopifnot(inherits(formula, "formula"))
  check_suggests("brms")
  args <- as.list(substitute(list(...))[-1L])
  args$formula = formula
  trending_model("brm", args)
}

# -------------------------------------------------------------------------

#' @export
format.trending_model <- function(x, ...) {
  attributes(x) <- NULL
  c("Untrained trending model:", paste("   ", format(x)))
}

# -------------------------------------------------------------------------

#' @export
print.trending_model <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

trending_model <- function(model_name, args) {
  cls <- c(
    sprintf("%s_trending_model", substitute(model_name)),
    "trending_model"
  )

  qfun <- bquote(
    function(formula, ...) { # formula argument ensures printing of formula first
      if (getRversion() >= "4.1.0") {
        nms <- ...names()
      } else {
        dots <- match.call(expand.dots = FALSE)$`...`
        nms <- names(dots)
      }
      if ("data" %in% nms && length(nms)) {
        stop("'data' should not be passed as argument", call. = FALSE)
      }
      out <- match.call()
      out[[1]] <- as.name(.(model_name))
      structure(out, class = c(.(cls), class(out)))
    }
  )

  do.call(eval(qfun), args)
}
