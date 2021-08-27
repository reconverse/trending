#' Predict method for trending_model objects
#'
#' Adds estimated values and associated confidence and/or prediction intervals
#' to data based on trending_model fit.
#'
#' @param object A list of [`trending_model`] objects.
#' @inheritParams predict.trending_model
#'
#' @returns A `trending_predict_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row per model and columns:
#'
#'   - result: the input data frame with additional estimates and, optionally,
#'     confidence and or prediction intervals. `NULL` if the associated
#'     `predict` method fails.
#'   - warnings: any warnings generated during prediction.
#'   - errors: any errors generated during prediction.
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' predict(list(poisson_model, negbin_model), dat)
#' predict(list(pm = poisson_model, nm = negbin_model), dat)
#'
#' @author Tim Taylor
#' @seealso [predict.trending_model()], [predict.trending_fit()],
#'   [predict.trending_fit_tbl()],
#' @export
predict.list <- function(
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
  ...
) {

  tmp <- bquote(fit.list(object, .(data)))
  tmp <- eval(tmp)

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
    uncertain = uncertain
  )
}
