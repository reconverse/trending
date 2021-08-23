#' Predict method for trending_fit object
#'
#' Adds estimated values and associated confidence and/or prediction intervals
#' to trending_fit objects.
#'
#' @param object A [`trending_fit`][fit.trending_model()] object.
#' @param new_data A `data.frame` containing data for which estimates are to be
#'   derived. If missing, the model frame from the fit data will be used.
#' @param name Character vector of length one giving the name to use for the
#'   calculated estimate.
#' @param alpha The alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived.
#' @param add_ci Should a confidence interval be added to the output.
#'   Default TRUE.
#' @param ci_names Names to use for the resulting confidence intervals.
#' @param add_pi Should a prediction interval be added to the output.
#'   Default TRUE.
#' @param pi_names Names to use for the resulting prediction intervals.
#' @param simulate_pi Should the prediction intervals for glm models be
#'   simulated. If TRUE, default, `predict()` uses the [ciTools::add_pi()]
#'   function to generate the intervals.
#' @param sims The number of simulations to run when simulating prediction
#'   intervals for a glm model.
#' @param uncertain Only used for glm models and when `simulate_pi = FALSE`.
#'   Default TRUE.  If FALSE uncertainty in the fitted parameters is ignored
#'   when generating the parametric prediction intervals.
#' @param as_tibble Should the output be converted to a tibble subclass.
#' @param ... Not currently used.
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' fitted_poisson <- fit(poisson_model, dat)
#' predict(fitted_poisson)
#' predict(fitted_poisson, as_tibble = TRUE)
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
#' @seealso [predict.trending_fit_list()], [predict.trending_fit_tbl()] and
#'   [predict.trending_model()]
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
    as_tibble = FALSE,
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
