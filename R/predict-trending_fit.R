#' Predict method for trending_fit object
#'
#' Adds estimated values and associated confidence and/or prediction intervals
#' to trending_fit objects.
#'
#' @param object A [`trending_fit`][fit.trending_model()] object.
#' @param new_data A `data.frame` containing data for which estimates are to be
#'   derived. If missing, the model frame from the fit data will be used.
#' @inheritParams predict.trending_model
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' fitted_poisson <- fit(poisson_model, dat)
#' predict(fitted_poisson)
#' predict(fitted_poisson, as_tibble = FALSE)
#'
#' @returns If `as_tibble = FALSE`, a `trending_predict` object, which is a list
#'   subclass, with entries:
#'
#'   - result: the input data frame with additional estimates and, optionally,
#'     confidence and or prediction intervals. `NULL` if the associated
#'     `predict` method fails.
#'   - warnings: any warnings generated during prediction.
#'   - errors: any errors generated during prediction.
#'
#'   If `as_tibble = TRUE`, a `trending_predict_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row per model and columns
#'   'result', 'warnings' and 'errors' with contents as above.
#'
#' @author Tim Taylor
#' @seealso [predict.trending_fit_tbl()] and [predict.trending_model()]
#' @export
predict.trending_fit <- function(
    object,
    new_data,
    name = "estimate",
    alpha = 0.05,
    add_ci = TRUE,
    ci_names = c("lower_ci", "upper_ci"),
    add_pi = TRUE,
    pi_names = c("lower_pi", "upper_pi"),
    simulate_pi = FALSE,
    sims = 2000,
    uncertain = TRUE,
    as_tibble = TRUE,
    ...
) {

  # ensure that we have a model to use for predictions
  fitted_model <- get_fitted_model(object)

  # if no data supplied we use the model data
  if (missing(new_data)) new_data <- get_fitted_data(object)

  # check for name clashes with the existing input
  check_names(
    new_data = new_data,
    name = name,
    add_ci = add_ci,
    ci_names = ci_names,
    add_pi = add_pi,
    pi_names = pi_names
  )

  res <- predict_individual(
    fitted_model,
    new_data = new_data,
    response = get_response.trending_fit(object),
    predictors = get_predictors.trending_fit(object),
    name = name,
    alpha = alpha,
    add_ci = add_ci,
    ci_names = ci_names,
    add_pi = add_pi,
    pi_names = pi_names,
    simulate_pi = simulate_pi,
    sims = sims,
    uncertain = uncertain
  )

  if (as_tibble) {
    res <- tibble(
      result = list(res[[1]]),
      warnings = list(res[[2]]),
      errors = list(res[[3]])
    )
    res <- new_trending_predict_tbl(res)
  } else {
    res <- new_trending_predict(res)
  }
  res
}
