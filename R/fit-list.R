#' Fit method list object
#'
#' Fits a list of trending_model objects to the given input data.
#'
#' @param x A list of trending_model objects#'
#' @inheritParams fit.trending_model
#'
#' @return If `as_tibble = FALSE`, then a `trending_fit_list` object is
#'   returned. This is a list subclass with entries:
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
#' poisson_model <- glm_model(y ~ x , family = poisson)
#' negbin_model <- glm_nb_model(y ~ x)
#'
#' fit(list(poisson_model, negbin_model), dat)
#' fit(list(pm = poisson_model, nm = negbin_model), dat)
#'
#' @author Tim Taylor
#' @seealso [fit.trending_model()]
#' @export
fit.list <- function(x, data, as_tibble = TRUE, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  qfun <- bquote(lapply(x, fit, data = .(substitute(data))))
  res <- eval(qfun)
  nms <- names(x)
  if (!is.null(nms)) names(res) <- nms
  if (as_tibble) {
    res <- lapply(seq_along(res[[1]]), function(i) lapply(res, "[[", i))
    res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
    model_name <- NULL
    if (!is.null(nms)) {
      res <- cbind(tibble(model_name = nms), res)
      model_name <- "model_name"
    }
    res <- new_trending_fit_tbl(res, model_name = model_name)
  } else {
    res <- new_trending_fit_list(res)
  }
  res
}
