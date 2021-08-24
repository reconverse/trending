#' Accessors generics
#'
#' Generics for accessing model information.
#'
#' Methods are provided for  `trending_model`, `trending_fit`,
#'   `trending_fit_tbl`, `trending_predict`, and `trending_predict_tbl` objects.
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

#' @export
#' @aliases get_result.trending_fit_tbl
#' @rdname accessors
get_result.trending_fit_tbl <- function(x, ...) x[[attr(x, "result")]]

#' @export
#' @aliases get_result.trending_predict_tbl
#' @rdname accessors
get_result.trending_predict_tbl <- get_result.trending_fit_tbl

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

#' @export
#' @aliases get_warnings.trending_fit_tbl
#' @rdname accessors
get_warnings.trending_fit_tbl <- function(x, ...) x[[attr(x, "warnings")]]

#' @export
#' @aliases get_warnings.trending_predict_tbl
#' @rdname accessors
get_warnings.trending_predict_tbl <- get_warnings.trending_fit_tbl

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

#' @export
#' @aliases get_errors.trending_fit_tbl
#' @rdname accessors
get_errors.trending_fit_tbl <- function(x, ...) x[[attr(x, "errors")]]

#' @export
#' @aliases get_errors.trending_predict_tbl
#' @rdname accessors
get_errors.trending_predict_tbl <- get_errors.trending_fit_tbl


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
#' @aliases get_fitted_model.trending_fit
#' @export
get_fitted_model.trending_fit <- get_result.trending_fit

#' @rdname accessors
#' @aliases get_fitted_model.trending_fit_tbl
#' @export
get_fitted_model.trending_fit_tbl <- get_result.trending_fit_tbl

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
#' @aliases get_fitted_model.trending_fit
#' @export
get_fitted_data.trending_fit <- function(x, ...) {
  model <- get_fitted_model.trending_fit(x)
  res <- if (inherits(model, "brmsfit")) model$data else model$model
  attr(res, "data_name") <- NULL
  attr(res, "terms") <- NULL
  res
}

#' @rdname accessors
#' @aliases get_fitted_model.trending_fit_tbl
#' @export
get_fitted_data.trending_fit_tbl <- function(x, ...) {
  models <- get_fitted_model(x)
  lapply(
    models,
    function(x) {
      res <- if (inherits(x, "brmsfit")) x$data else x$model
      attr(res, "data_name") <- NULL
      attr(res, "terms") <- NULL
      res
    }
  )
}

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
get_formula.trending_fit <- function(x, ...) {
  res <- get_fitted_model.trending_fit(x)
  if (inherits(res, "brmsfit")) res$formula else res$call$formula
}

#' @rdname accessors
#' @aliases get_formula.trending_fit_tbl
#' @export
get_formula.trending_fit_tbl <- function(x, ...) {
  models <- get_fitted_model(x)
  lapply(
    models,
    function(m) if (inherits(m, "brmsfit")) m$formula else m$call$formula
  )
}

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
  formula <- get_formula(x)
  as.character(formula)[2]
}

#' @export
#' @aliases get_response.trending_fit
#' @rdname accessors
get_response.trending_fit <- function(x, ...) {
  formula <- get_formula(x)
  if (inherits(formula, "brmsformula")) formula <- formula$formula
  as.character(formula)[2]
}

#' @export
#' @aliases get_response.trending_fit_tbl
#' @rdname accessors
get_response.trending_fit_tbl <- function(x, ...) {
  formula <- get_formula(x)
  lapply(
    formula,
    function(x) {
      if (inherits(x, "brmsformula")) x <- x$formula
      as.character(x)[2]
    }
  )
}

#' @export
#' @aliases get_response.trending_prediction
#' @rdname accessors
get_response.trending_prediction <- function(x, ...) {
  attr(x, "response")
}


#' @export
#' @aliases get_response.trending_predict
#' @rdname accessors
get_response.trending_predict <- function(x, ...) {
  result <- get_result(x)
  get_response(result)
}

#' @export
#' @aliases get_response.trending_predict_tbl
#' @rdname accessors
get_response.trending_predict_tbl <- function(x, ...) {
  result <- get_result(x)
  lapply(result, get_response)
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
  formula <- get_formula(x)
  if (inherits(formula, "brmsformula")) formula <- formula$formula
  vars <- all.vars(formula)
  response <- get_response(x)
  vars[!vars %in% response]
}

#' @export
#' @aliases get_predictors.trending_fit
#' @rdname accessors
get_predictors.trending_fit <- get_predictors.trending_model

#' @export
#' @aliases get_predictors.trending_fit_tbl
#' @rdname accessors
get_predictors.trending_fit_tbl <- function(x, ...) {
  formulas <- get_formula(x)
  vars <- lapply(
    formulas,
    function(x) {
      if (inherits(x, "brmsformula")) x <- x$formula
      all.vars(x)
    }
  )
  response <- get_response(x)
  .mapply(function(x, y) x[!x %in% y], dots = list(x = vars, y = response), MoreArgs = NULL)
}

#' @export
#' @aliases get_predictors.trending_prediction
#' @rdname accessors
get_predictors.trending_prediction <- function(x, ...) {
  attr(x, "predictors")
}

#' @export
#' @aliases get_predictors.trending_predict
#' @rdname accessors
get_predictors.trending_predict <- function(x, ...) {
  result <- get_result(x)
  get_predictors(result)
}

#' @export
#' @aliases get_predictors.trending_predict_tbl
#' @rdname accessors
get_predictors.trending_predict_tbl <- function(x, ...) {
  result <- get_result(x)
  lapply(result, get_predictors)
}

# -------------------------------------------------------------------------

#' @export
#' @aliases get_estimate
#' @rdname accessors
get_estimate <- function(x, ...) UseMethod("get_estimate")

#' @rdname accessors
#' @aliases get_estimate.default
#' @export
get_estimate.default <- function(x, ...) not_implemented(x)

#' @export
#' @aliases get_estimate.trending_prediction
#' @rdname accessors
get_estimate.trending_prediction <- function(x, ...) {
  attr(x, "estimate")
}

#' @export
#' @aliases get_estimate.trending_predict
#' @rdname accessors
get_estimate.trending_predict <- function(x, ...) {
  result <- get_result(x)
  get_estimate(result)
}

#' @export
#' @aliases get_estimate.trending_predict_tbl
#' @rdname accessors
get_estimate.trending_predict_tbl <- function(x, ...) {
  result <- get_result(x)
  vapply(result, get_estimate, character(1))
}
