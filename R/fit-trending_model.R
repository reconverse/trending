#' Fit method for trending_model objects
#'
#' Fits the specified model to the input data
#'
#' @inheritParams fit
#' @param as_tibble Should the output be converted to a tibble subclass.
#' @param ... Not currently used.
#'
#' @return If `as_tibble = FALSE`, then a `trending_fit` object is returned.
#'   This is a list subclass with entries:
#'
#'   - result: the resulting fit from calling the underlying model
#'     directly, i.e.
#'
#'       - `lm_model`: a fitted model object of class [`lm`][stats::lm()]
#'       - `glm_model`: a fitted model object of class [`glm`][stats::glm()]
#'       - `glm_nb_model`: a fitted model object of class [`negbin`][MASS::glm.nb()]
#'       - `brm_model`: An object of class [`brmsfit`][brms::brm()]
#'
#'     `NULL` if fitting fails.
#'
#'   - warnings: any warnings generated during fitting
#'   - errors: any errors generated during fitting
#'
#'   If `as_tibble = TRUE`, a `trending_fit_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row for each model and
#'   columns 'result', 'warnings' and 'errors' with contents as above.
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
#' fit(list(poisson_model, negbin_model), dat)
#' fit(list(pm = poisson_model, nm = negbin_model), dat)
#'
#' @author Tim Taylor
#' @seealso [fit.list()]
#' @export
fit.trending_model <- function(x, data, as_tibble = TRUE, ...) {
  x[["data"]] <- substitute(data)
  envir = parent.frame()
  if (inherits(x, "brm_trending_model")) envir$brm <- brms::brm
  if (inherits(x, "glm.nb_trending_model")) envir$glm.nb <-  MASS::glm.nb
  f <- make_catcher(eval)
  res <- f(x, envir)
  if (as_tibble) {
    res <- tibble(
      result = list(res[[1]]),
      warnings = list(res[[2]]),
      errors = list(res[[3]])
    )
    res <- new_trending_fit_tbl(res)
  } else {
    res <- new_trending_fit(res)
  }
  res
}

