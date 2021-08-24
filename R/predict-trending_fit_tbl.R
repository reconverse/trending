#' Predict method for trending_fit_tbl object
#'
#' Adds estimated values and associated confidence and/or prediction intervals
#' to trending_fit_tbl objects.
#'
#' @param object A [`trending_fit_tbl`][fit.list()] object.
#' @inheritParams predict.trending_fit
#'
#' @returns a `trending_predict_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row per model and columns
#'   'result', 'warnings' and 'errors' with contents as above.:
#'
#'   - result: the input data frame with additional estimates and, optionally,
#'     confidence and or prediction intervals. `NULL` if the associated
#'     `predict` method fails.
#'
#'   - warnings: any warnings generated during prediction.
#'
#'   - errors: any errors generated during prediction.
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#' fitted_tbl <- fit(list(poisson_model, negbin_model), dat)
#'
#' predict(fitted_tbl)
#'
#' @author Tim Taylor
#' @seealso [predict.trending_fit()], [predict.trending_fit_tbl()] and
#'   [predict.trending_model()]
#' @export
predict.trending_fit_tbl <- function(
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
  ...
) {
  # if no data supplied we use the model data
  fitted_models <- get_fitted_model(object)
  if (missing(new_data)) {
    ok <- vapply(fitted_models, function(x) !is.null(x), logical(1))
    new_data <- if (!any(ok)) NULL else get_fitted_data(object)[ok][[1]] # OK as data will be same for all
  }

  res <- .mapply(
    FUN = predict_individual,
    dots = list(
      model = fitted_models,
      response = get_response(object),
      predictors = get_predictors(object)
    ),
    MoreArgs = list(
      new_data = new_data,
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
  )

  nms_var <- attr(object, "model_name")
  if (!is.null(nms_var)) names(res) <- object[[nms_var]]
  res <- lapply(seq_along(res[[1]]), function(i) lapply(res, "[[", i))
  res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
  model_name <- NULL
  if (!is.null(nms_var)) {
    res <- cbind(tibble(model_name = object[[nms_var]]), res)
    model_name <- nms_var
  }
  new_trending_predict_tbl(res, model_name = model_name)
}
