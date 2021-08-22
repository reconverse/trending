#' Fit generics
#'
#' `fit()` is a generic to fit a model with methods included for both
#'   `trending_model` objects as well as a `list` of these objects.
#'   data.
#'
#' @param x An \R object
#' @param data A data frame containing the data to fit.
#' @param as_tibble Should the output be converted to a tibble subclass.
#' @param ... Not currently used.
#'
#' @return For individual `trending_model` inputs and with `as_tibble = FALSE`,
#'   then a `trending_fit` object is returned. This is a list subclass with
#'   entries:
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
#'   For a list of trending_models, `trending_fit_list` where each entry is a
#'   `trending_fit` object.
#'
#'   If `as_tibble = TRUE`, a `trending_fit_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row for each model and
#'   columns 'result', 'warnings' and 'errors' with contents as above.
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
#' fit(list(poisson_model, negbin_model), dat)
#' fit(list(pm = poisson_model, nm = negbin_model), dat)
#'
#' @export
fit <- function(x, ...) UseMethod("fit")

#' @rdname fit
#' @aliases fit.default
#' @export
fit.default <- function(x, ...) not_implemented(x)

#' @rdname fit
#' @aliases fit.trending_model
#' @export
fit.trending_model <- function(x, data, as_tibble = FALSE, ...) {
  x[["data"]] <- substitute(data)
  env = parent.frame()
  if (inherits(x, "brm_trending_model")) env <- list(env, brm = brms::brm)
  if (inherits(x, "glm.nb_trending_model")) env <- list(env, glm.nb = MASS::glm.nb)
  f <- make_catcher(eval)
  res <- f(x, env)
  res <- structure(res, class = c("trending_fit", class(res)))
  if (as_tibble) {
    res <- list(res)
    res <- trending_fit_list_to_tibble(res)
  }
  res
}

#' @rdname fit
#' @aliases fit.list
#' @export
fit.list <- function(x, data, as_tibble = FALSE, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entries should be `trending_model` objects", call. = FALSE)
  }
  qfun <- bquote(lapply(x, fit, data = .(substitute(data))))
  res <- eval(qfun)
  nms <- names(x)
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
