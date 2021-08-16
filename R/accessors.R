#' Accessors generics
#'
#' Generics for accessing model information.
#'
#' Methods are provided for  [`trending_model`],
#'   [`trending_fit`][fit.trending_model()] and
#'   [`trending_predict`][predict.trending_fit()] objects.
#'
#' @param x An \R object.
#' @param ... Not currently used by any methods.
#'
#' @returns
#'
#' * `get_result()`: the captured output.
#'
#' * `get_warnings()`: the captured warnings.
#'
#' * `get_errors()`: the captured warnings.
#'
#' * `get_fitted_data`: The underlying data used to fit the model.
#'
#' * `get_fitted_model()`: The underlying fitted model.
#'
#' * `get_prediction()`: The resulting prediction.
#'
#' * `get_formula()`: the formula used to model temporal trends.
#'
#' * `get_response()`: the response variable of the underlying model.
#'
#' * `get_predictors()`: the predictor variable(s) of the underlying model.
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' fitted_poisson <- fit(poisson_model, dat)
#'
#' get_fitted_model(fitted_poisson)
#' get_formula(fitted_poisson)
#' get_response(fitted_poisson)
#' get_predictors(fitted_poisson)
#'
#' @name accessors
NULL

# -------------------------------------------------------------------------

#' @export
#' @aliases get_result
#' @rdname accessors
get_result <- function(x, ...) UseMethod("get_result")

#' @rdname accessors
#' @aliases get_result.default
#' @export
get_result.default <- function(x, ...) not_implemented(x)

#' @rdname accessors
#' @aliases get_result.trending_fit
#' @export
get_result.trending_fit <- function(x, ...) x$result

#' @export
#' @aliases get_result.trending_predict
#' @rdname accessors
get_result.trending_predict <- get_result.trending_fit

# -------------------------------------------------------------------------

#' @export
#' @aliases get_warnings
#' @rdname accessors
get_warnings <- function(x, ...) UseMethod("get_warnings")

#' @rdname accessors
#' @aliases get_warnings.default
#' @export
get_warnings.default <- function(x, ...) not_implemented(x)

#' @rdname accessors
#' @aliases get_warnings.trending_fit
#' @export
get_warnings.trending_fit <- function(x, ...) x$warnings

#' @export
#' @aliases get_warnings.trending_predict
#' @rdname accessors
get_warnings.trending_predict <- get_warnings.trending_fit

# -------------------------------------------------------------------------

#' @export
#' @aliases get_errors
#' @rdname accessors
get_errors <- function(x, ...) UseMethod("get_errors")

#' @rdname accessors
#' @aliases get_errors.default
#' @export
get_errors.default <- function(x, ...) not_implemented(x)

#' @rdname accessors
#' @aliases get_errors.trending_fit
#' @export
get_errors.trending_fit <- function(x, ...) x$errors

#' @export
#' @aliases get_errors.trending_predict
#' @rdname accessors
get_errors.trending_predict <- get_errors.trending_fit

# -------------------------------------------------------------------------

#' @export
#' @aliases get_fitted_data
#' @rdname accessors
get_fitted_data <- function(x, ...) UseMethod("get_fitted_data")

#' @rdname accessors
#' @aliases get_fitted_data.default
#' @export
get_fitted_data.default <- function(x, ...) not_implemented(x)

#' @rdname accessors
#' @aliases get_fitted_model.default
#' @export
get_fitted_data.trending_fit <- function(x, ...) {
  model <- get_fitted_model.trending_fit(x)
  res <- model$model
  attr(res, "terms") <- NULL
  res
}

# -------------------------------------------------------------------------

#' @export
#' @aliases get_fitted_model
#' @rdname accessors
get_fitted_model <- function(x, ...) UseMethod("get_fitted_model")

#' @rdname accessors
#' @aliases get_fitted_model.default
#' @export
get_fitted_model.default <- function(x, ...) not_implemented(x)

#' @rdname accessors
#' @aliases get_fitted_model.default
#' @export
get_fitted_model.trending_fit <- function(x, ...) x$result

# -------------------------------------------------------------------------

#' @export
#' @aliases get_formula
#' @rdname accessors
get_formula <- function(x, ...) UseMethod("get_formula")

#' @rdname accessors
#' @aliases get_formula.default
#' @export
get_formula.default <- function(x, ...) not_implemented(x)

#' @rdname accessors
#' @aliases get_formula.tranding_model
#' @export
get_formula.trending_model <- function(x, ...) x$formula

#' @rdname accessors
#' @aliases get_formula.tranding_fit
#' @export
get_formula.trending_fit <- function(x, ...) x$result$call$formula

# -------------------------------------------------------------------------

#' @export
#' @aliases get_response
#' @rdname accessors
get_response <- function(x, ...) UseMethod("get_response")

#' @rdname accessors
#' @aliases get_response.default
#' @export
get_response.default <- function(x, ...) not_implemented(x)

#' @export
#' @aliases get_response.trending_model
#' @rdname accessors
get_response.trending_model <- function(x, ...) {
  formula <- get_formula.trending_model(x)
  as.character(formula)[2]
}

#' @export
#' @aliases get_response.trending_fit
#' @rdname accessors
get_response.trending_fit <- function(x, ...) {
  formula <- get_formula.trending_fit(x)
  as.character(formula)[2]
}

# -------------------------------------------------------------------------

#' @export
#' @aliases get_predictors
#' @rdname accessors
get_predictors <- function(x, ...) UseMethod("get_predictors")

#' @rdname accessors
#' @aliases get_predictors.default
#' @export
get_predictors.default <- function(x, ...) not_implemented(x)

#' @export
#' @aliases get_predictors.trending_model
#' @rdname accessors
get_predictors.trending_model <- function(x, ...) {
  formula <- get_formula.trending_model(x)
  vars <- all.vars(formula)
  response <- get_response.trending_model(x)
  vars[!vars %in% response]
}

#' @export
#' @aliases get_predictors.trending_fit
#' @rdname accessors
get_predictors.trending_fit <- function(x, ...) {
  formula <- get_formula.trending_fit(x)
  vars <- all.vars(formula)
  response <- get_response.trending_fit(x)
  vars[!vars %in% response]
}

