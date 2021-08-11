#' Fit a trending model
#'
#' `fit` will return the output from fitting the model to the specified
#'   data.
#'
#' @param object A [`trending_model`] object (i.e. the output from function
#'   `lm_model`, `glm_model`, `glm.nb_model`, or `brm_model`).
#' @param data A data frame containing the data to fit.
#' @param ... Not currently used.
#'
#' @return A `trending_fit` object which is a subclass of
#'   [tibble][tibble::tibble()] with columns:
#'
#'   - fitted_model: the resulting fit from calling the underlying model
#'     directly, i.e.
#'
#'       - `lm_model`: a fitted model object of class [`lm`][stats::lm()]
#'
#'       - `glm_model`: a fitted model object of class [`glm`][stats::glm()]
#'
#'       - `glm.nb_model`: a fitted model object of class [`negbin`][MASS::glm.nb()]
#'
#'       - `brm_model`: An object of class [`brmsfit`][brms::brm()]
#'
#'     `NULL` if fitting fails.
#'
#'   - fitting_warnings: any warnings generated during fitting
#'
#'   - fitting_errors: any errors generated during fitting
#'
#'
#' @author Tim Taylor
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#'
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm.nb_model(y ~ x)
#'
#' fit(poisson_model, dat)
#' fit(negbin_model, dat)
#'
#' @export
fit.trending_model <- function(object, data, ...) {
  object[["data"]] <- substitute(data)
  env = parent.frame()
  fit_internal(object, env)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

fit_internal <- function(x, env, catch) {
  if (inherits(x, "brm_trending_model")) {
    env <- c(env, brm = brms::brm)
  }
  f <- make_catcher(eval)
  res <- f(x, env)
  res <- tibble(
    fitted_model = list(res[[1]]),
    fitting_warnings = list(res[[2]]),
    fitting_errors = list(res[[3]])
  )

  res <- new_tibble(
    res,
    fitted_model = "fitted_model",
    fitting_warnings = "fitting_warnings",
    fitting_errors = "fitting_errors",
    nrow = nrow(res),
    class = "trending_fit"
  )
  validate_tibble(res)
}
