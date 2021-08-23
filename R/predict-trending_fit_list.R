#' Predict method for trending_fit_list object
#'
#' Adds estimated values and associated confidence and/or prediction intervals
#' to trending_fit_list objects.
#'
#' @param object A [`trending_fit_list`][fit.list()] object.
#' @inheritParams predict.trending_fit
#'
#'
#' @returns If `as_tibble = FALSE`, a `trending_predict_list` object, which is a
#'   list subclass, with entries made up of
#'   [`trending_predict`][predict.trending_fit()] objects corresponding to each
#'   individual fitted model. These nested entries in turn have entries:
#'
#'   - result: the input data frame with additional estimates and, optionally,
#'     confidence and or prediction intervals. `NULL` if the associated
#'     `predict` method fails.
#'
#'   - warnings: any warnings generated during prediction.
#'
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
#' negbin_model <- glm_nb_model(y ~ x)
#' fitted_list <- fit(list(poisson_model, negbin_model), dat)
#' fitted_list_named <- fit(list(p = poisson_model, n = negbin_model), dat)
#'
#' predict(fitted_list)
#' predict(fitted_list, as_tibble = TRUE)
#'
#' @author Tim Taylor
#' @seealso [predict.trending_fit()] and [predict.trending_fit_tbl()]
#' @export
predict.trending_fit_list <- function(
  object,
  new_data,
  name = "estimate",
  alpha = 0.05,
  add_ci = TRUE,
  ci_names = c("lower_ci", "upper_ci"),
  add_pi = TRUE,
  pi_names = c("lower_pi", "upper_pi"),
  simulate_pi = FALSE,
  sims = 2000,
  uncertain = TRUE,
  as_tibble = TRUE,
  ...
) {

  # if no data supplied we use the model data
  fitted_models <- get_fitted_model(object)
  if (missing(new_data)) {
    ok <- vapply(fitted_models, function(x) !is.null(x), logical(1))
    new_data <- if (!any(ok)) NULL else get_fitted_data(object)[ok][[1]] # OK as data will be same for all
  }

  res <- lapply(
    object,
    predict.trending_fit,
    new_data <- new_data,
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

  nms <- attr(object, "model_name")
  if (!is.null(nms)) names(res) <- object[[nms]]

  if (as_tibble) {
    res <- lapply(seq_along(res[[1]]), function(i) lapply(res, "[[", i))
    res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
    model_name <- NULL
    if (!is.null(nms)) {
      res <- cbind(tibble(model_name = nms), res)
      model_name <- "model_name"
    }
    res <- new_trending_predict_tbl(res, model_name = model_name)
  } else {
    res <- new_trending_predict_list(res)
  }
  res
}
