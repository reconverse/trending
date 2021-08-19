#' Fit a trending model
#'
#' `fit` will return the output from fitting the model to the specified
#'   data.
#'
#' @param object A [`trending_model`] object (i.e. the output from function
#'   `lm_model`, `glm_model`, `glm_nb_model`, or `brm_model`).
#' @param data A data frame containing the data to fit.
#' @param as_tibble Should the output be converted to a tibble subclass.
#' @param ... Not currently used.
#'
#' @return If `as_tibble = FALSE`, a `trending_fit` object which is a list
#'   subclass with entries:
#'
#'   - result: the resulting fit from calling the underlying model
#'     directly, i.e.
#'
#'       - `lm_model`: a fitted model object of class [`lm`][stats::lm()]
#'
#'       - `glm_model`: a fitted model object of class [`glm`][stats::glm()]
#'
#'       - `glm_nb_model`: a fitted model object of class [`negbin`][MASS::glm.nb()]
#'
#'       - `brm_model`: An object of class [`brmsfit`][brms::brm()]
#'
#'     `NULL` if fitting fails.
#'
#'   - warnings: any warnings generated during fitting
#'
#'   - errors: any errors generated during fitting
#'
#'   If `as_tibble = TRUE`, a `trending_fit_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row and columns 'result',
#'   'warnings' and 'errors' with contents as above.
#'
#'
#' @author Tim Taylor
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#'
#' poisson_model <- glm_model(y ~ x , family = poisson)
#' negbin_model <- glm_nb_model(y ~ x)
#'
#' fit(poisson_model, dat)
#' fit(negbin_model, dat)
#'
#' @export
fit.trending_model <- function(object, data, as_tibble = FALSE, ...) {
  object[["data"]] <- substitute(data)
  env = parent.frame()
  if (inherits(object, "brm_trending_model")) env <- list(env, brm = brms::brm)
  f <- make_catcher(eval)
  res <- f(object, env)
  res <- structure(res, class = c("trending_fit", class(res)))
  if (as_tibble) {
    res <- list(res)
    res <- trending_fit_list_to_tibble(res)
  }
  res
}
