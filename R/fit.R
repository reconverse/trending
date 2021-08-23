#' Fit generic
#'
#' `fit()` is a generic to fit a specified model.
#'
#' @param x An \R object
#' @param data A data frame containing the data to fit.
#' @param ... Arguments passed to underlying methods.
#'
#' @return The value returned depends on the class of the input argument.
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
#' @seealso [fit.trending_model()] and [fit.list()]
#' @export
fit <- function(x, data, ...) UseMethod("fit")

#' @rdname fit
#' @aliases fit.default
#' @export
fit.default <- function(x, data, ...) not_implemented(x)
