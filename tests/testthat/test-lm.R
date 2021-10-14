test_that("lm_model", {

  # setup
  model <- lm_model(hp ~ cyl)
  fit <- fit(model, mtcars, as_tibble = FALSE)
  fit_tbl <- fit(model, mtcars)
  expected_model <- lm(hp ~ cyl, data = mtcars)
  pred <- predict(fit, mtcars, as_tibble = FALSE)           # prediction with new data
  pred_tbl <- predict(fit, mtcars)                          # prediction as tibble from list
  pred2 <- predict(fit, add_pi = FALSE, as_tibble = FALSE)  # prediction with no new data or pi
  pred2_tbl <- predict(fit_tbl, add_pi = FALSE)             # prediction tibble
  pred_from_model <- predict(model, mtcars, as_tibble = FALSE)
  pred_from_model_tbl <- predict(model, mtcars)

  # test printing
  expect_snapshot(lm_model(count ~ day, na.action = na.exclude))

  # test model fitting - note we need to ignore the call as we have a similar
  # issue to the update function discussed in
  # https://stat.ethz.ch/pipermail/r-help/2021-August/471811.html
  fitted_model <- get_fitted_model(fit)
  fitted_model_tbl <- get_fitted_model(fit_tbl)
  expect_equal(fitted_model, fitted_model_tbl[[1]])
  no_call <- function(x) x[setdiff(names(x), "call")]
  expect_equal(no_call(fitted_model), no_call(expected_model), ignore_function_env = TRUE)
  expect_identical(names(fit), c("result", "warnings", "errors"))
  expect_identical(names(fit_tbl), c("result", "warnings", "errors"))
  expect_s3_class(fit_tbl, "tbl_df")

  # test accessors
  expect_identical(get_result(fit), fitted_model)
  expect_null(get_warnings(fit))
  expect_null(get_errors(fit))
  expect_identical(get_fitted_data(fit), mtcars[c("hp", "cyl")])
  expect_identical(get_formula(model), hp ~ cyl)
  expect_identical(get_formula(fit), hp ~ cyl)
  expect_identical(get_response(model), "hp")
  expect_identical(get_response(fit), "hp")
  expect_identical(get_response(pred), "hp")
  expect_identical(get_response(pred_tbl), list("hp"))
  expect_identical(get_predictors(model), "cyl")
  expect_identical(get_predictors(fit), "cyl")
  expect_identical(get_predictors(pred), "cyl")
  expect_identical(get_predictors(pred_tbl), list("cyl"))
  expect_identical(get_estimate(pred), "estimate")
  expect_identical(get_estimate(pred_tbl), "estimate")

  # test prediction
  expect_identical(names(pred), c("result", "warnings", "errors"))
  expect_identical(names(pred_tbl), c("result", "warnings", "errors"))
  expect_identical(names(pred2), c("result", "warnings", "errors"))
  expect_identical(names(pred2_tbl), c("result", "warnings", "errors"))
  expect_identical(pred, pred_from_model)
  expect_identical(pred_tbl, pred_from_model_tbl)

  # test prediction accessors
  expect_identical(
    names(get_result(pred)),
    c(names(mtcars), "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
  )
  expect_identical(
    names(get_result(pred_tbl)[[1]]),
    c(names(mtcars), "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
  )
  expect_identical(
    names(get_result(pred2)),
    c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
  )
  expect_identical(
    names(get_result(pred2_tbl)[[1]]),
    c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
  )
  expect_null(get_warnings(pred))
  expect_null(get_warnings(pred_tbl)[[1]])
  expect_null(get_warnings(pred2))
  expect_null(get_warnings(pred2_tbl)[[1]])
  expect_null(get_errors(pred))
  expect_null(get_errors(pred_tbl)[[1]])
  expect_null(get_errors(pred2))
  expect_null(get_errors(pred2_tbl)[[1]])
})
