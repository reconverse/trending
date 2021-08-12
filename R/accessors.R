#' Accessors generics
#'
#' These gen can be used to access information stored in
#'   [`trending_model`], [`trending_fit`][fit.trending_model()] and
#'   [`trending_predict`][predict.trending_fit()] objects.
#'
#' @param x An \R object.
#' @param ... Not currently used by any methods.
#'
#' @returns
#'
#' * `get_formula()`: the formula used to model temporal trends.
#'
#' * `get_response()`: the response variable of the underlying model.
#'
#' * `get_fitted_model()`: The underlying fitted model.
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#'
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' fitted_poisson <- fit(poisson_model, dat)
#' get_response(fitted_poisson)
#' get_formula(fitted_poisson)
#' get_fitted_model(fitted_poisson)
#'
#' @name accessors
NULL

# -------------------------------------------------------------------------

#' @export
#' @rdname accessors
get_fitted_model <- function(x, ...) {
  UseMethod("get_fitted_model")
}

# -------------------------------------------------------------------------

#' @rdname accessors
#' @aliases get_fitted_model.default
#' @export
get_fitted_model.default <- function(x, ...) {
  not_implemented(x)
}

# -------------------------------------------------------------------------

#' @rdname accessors
#' @aliases get_fitted_model.default
#' @export
get_fitted_model.trending_fit <- function(x, ...) {
  f <- attr(x, "fitted_model")
  x[[f]][[1]]
}

# -------------------------------------------------------------------------

#' @export
#' @rdname accessors
get_formula <- function(x, ...) {
  UseMethod("get_formula")
}

# -------------------------------------------------------------------------

#' @rdname accessors
#' @aliases get_formula.default
#' @export
get_formula.default <- function(x, ...) {
  not_implemented(x)
}

# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_formula.trending_model <- function(x, ...) {
  x$formula
}

# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_formula.trending_fit <- function(x, ...) {
  model <- get_fitted_model.trending_fit(x)
  model$call$formula
}

# -------------------------------------------------------------------------

#' @export
#' @rdname accessors
get_response <- function(x, ...) {
  UseMethod("get_response")
}

# -------------------------------------------------------------------------

#' @rdname accessors
#' @aliases get_response.default
#' @export
get_response.default <- function(x, ...) {
  not_implemented(x)
}

# -------------------------------------------------------------------------

#' @export
#' @rdname accessors
get_response.trending_model <- function(x, ...) {
  formula <- get_formula.trending_model(x)
  as.character(formula)[2]
}

# -------------------------------------------------------------------------

#' @export
#' @rdname accessors
get_response.trending_fit <- function(x, ...) {
  model <- get_fitted_model.trending_fit(x)
  formula <- model$call$formula
  as.character(formula)[2]
}

