#' Predict method for trending_model objects
#'
#' Adds estimated values and associated confidence and/or prediction intervals
#' to data based on trending_model fit.
#'
#' @param object A [`trending_model`] object.
#' @param data A `data.frame` containing data to which the model is to be fit
#'   and estimates derived.
#' @inheritParams predict.trending_fit
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
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' predict(poisson_model, dat)
#' predict(poisson_model, dat, as_tibble = TRUE)
#'
#' @author Tim Taylor
#' @seealso [predict.trending_fit()], [predict.trending_fit_list()] and
#'   [predict.trending_fit_tbl()]
#' @export
predict.trending_model <- function(
  object,
  data,
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
  tmp <- fit.trending_model(object, data)
  predict(
    tmp,
    new_data = data,
    name = name,
    alpha = alpha,
    add_ci = add_ci,
    ci_names = ci_names,
    add_pi = add_pi,
    pi_names = pi_names,
    simulate_pi = simulate_pi,
    sims = sims,
    uncertain = uncertain,
    as_tibble = as_tibble
  )
}
