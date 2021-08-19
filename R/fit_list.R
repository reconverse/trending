#' Fit a list of trending models
#'
#' `fit_list()` will return the output from fitting a list of models to the
#'   specified data.
#'
#' @param models A list of [`trending_model`] object (i.e. a list of the output
#'   from functions `lm_model`, `glm_model`, `glm_nb_model`, or `brm_model`).
#' @param data A data frame containing the data to fit.
#' @param as_tibble Should the output be converted to a tibble subclass.
#'
#' @return If `as_tibble = TRUE` a `trending_fit_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with with one row for each fitted
#'   model and columns:
#'
#'   - model_name (optional - only if input `models` is a named list)
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
#'   If `as_tibble = FALSE`, a `trending_fit_list` where each entry is a
#'   `trending_fit` object.
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
#' fit_list(list(poisson_model, negbin_model), dat)
#' fit_list(list(pm = poisson_model, nm = negbin_model), dat)
#'
#' @export
fit_list<- function(models, data, as_tibble = FALSE) {
  stopifnot(is.list(models))
  if (!all(vapply(models, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  qfun <- bquote(lapply(models, fit, data = .(substitute(data))))
  res <- eval(qfun)
  nms <- names(models)
  if (!is.null(nms)) names(res) <- nms
  class(res) <- c("trending_fit_list", class(res))
  if (as_tibble) res <- trending_fit_list_to_tibble(res)
  res
}

trending_fit_list_to_tibble <- function(x, ...) {
  res <- lapply(seq_along(x[[1]]), function(i) lapply(x, "[[", i))
  res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
  nms <- names(x)
  model_name <- NULL
  if (!is.null(nms)) {
    res <- cbind(tibble(model_name = nms), res)
    model_name <- "model_name"
  }
  res <- new_tibble(
    res,
    model_name = model_name,
    result = "result",
    warnings = "warnings",
    errors = "errors",
    nrow = nrow(res),
    class = "trending_fit_tbl"
  )
  validate_tibble(res)
}
