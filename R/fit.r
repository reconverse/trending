#' Fitting for trending_model objects
#'
#' [fit()] fits a model using the given data to obtain an object of type
#'   `trending_model_fit` or  `trending_model_fit_list`.
#'
#' @param x The output of functions `lm_model`, `glm_model`, `glm_nb_model`, or
#'   brms_model or a list of these objects.
#'
#' @param data A `data.frame` to be used to train the model.
#'
#' @param ... Additional arguments passed to underlying models.
#' 
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' 
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' 
#' fit(poisson_model, dat)
#' fit(list(poisson_model, negbin_model), dat)
#'
#' @export
fit <- function(x, data, ...) {
  UseMethod("fit", x)
}


#' @export
#' @rdname fit
#' @aliases fit.trending_model trending_model_fit
fit.trending_model <- function(x, data, ...) {
  x$fit(data)
}

#' @export
#' @rdname fit
#' @aliases trending_model_fit trending_model_fit_list
fit.list <- function(x, data, ...) {
  if (!all(vapply(x, inherits, logical(1), "trending_model"))) {
    stop("list entrys should be `trending_model` objects", call. = FALSE)
  }
  res <- transpose(lapply(x, safely(fit), data, ...))
  nms <- names(x)
  if (is.null(nms)) {
    out <- tibble::tibble(
      data = list(data),
      fitted_model = res[[1]],
      fitting_warnings = res[[2]],
      fitting_errors = res[[3]]
    )
    nm_var = NULL
  } else {
    out <- tibble::tibble(
      model_name = nms,
      data = list(data),
      fitted_model = res[[1]],
      fitting_warnings = res[[2]],
      fitting_errors = res[[3]]
    )
    nm_var = "model_name"
  }
  
  out <- tibble::new_tibble(
    out,
    model_name <- nm_var,
    data = "data",
    fitted_model = "fitted_model",
    fitting_warnings = "fitting_warnings",
    fitting_errors = "fitting_errors",
    nrow = nrow(out),
    class = "trending_model_fit_list"
  )
  tibble::validate_tibble(out)
}

