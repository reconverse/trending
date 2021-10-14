test_that("glm_model", {

  # setup
  model <- glm_model(hp ~ cyl, family = poisson)
  fit <- fit(model, mtcars, as_tibble = FALSE)
  model_with_nonexistant <- glm_model(hp ~ bob, family = poisson)
  fit_with_error <- fit(model_with_nonexistant, mtcars, as_tibble = FALSE)
  pred_with_error <- predict(fit_with_error, as_tibble = FALSE)
  fit_tbl <- fit(model, mtcars)
  expected_model <- glm(hp ~ cyl, data = mtcars, family = poisson)
  pred <- predict(fit, add_ci = FALSE, simulate_pi = TRUE, as_tibble = FALSE)
  pred_tbl <- predict(fit, mtcars, add_ci = FALSE, simulate_pi = TRUE)  # prediction with new data and as tibble from list
  pred2 <- predict(fit, add_pi = FALSE, as_tibble = FALSE)              # prediction with no new data or pi
  pred2_tbl <- predict(fit_tbl, add_pi = FALSE)                         # prediction from tibble
  pred_from_model <- predict(model, mtcars, as_tibble = FALSE)
  pred_from_model_tbl <- predict(model, mtcars)

  # test printing
  expect_snapshot(glm_model(count ~ day, na.action = na.exclude))

  # test model fitting - note we need to ignore the call as we have a similar
  # issue to the update function discussed in
  # https://stat.ethz.ch/pipermail/r-help/2021-August/471811.html
  fitted_model <- get_fitted_model(fit)
  fitted_model_tbl <- get_fitted_model(fit_tbl)
  expect_equal(fitted_model, fitted_model_tbl[[1]], ignore_function_env = TRUE)
  no_call <- function(x) x[setdiff(names(x), "call")]
  expect_equal(no_call(fitted_model), no_call(expected_model), ignore_function_env = TRUE)
  expect_identical(names(fit), c("result", "warnings", "errors"))
  expect_identical(names(fit_tbl), c("result", "warnings", "errors"))
  expect_s3_class(fit_tbl, "tbl_df")

  # test fit accessors
  expect_type(get_errors(fit_with_error), "character")
  expect_null(get_result(fit_with_error))
  expect_identical(get_result(fit), fitted_model)
  expect_null(get_warnings(fit))
  expect_null(get_errors(fit))
  expect_identical(get_fitted_data(fit), mtcars[c("hp", "cyl")])
  expect_identical(get_formula(model), hp ~ cyl)
  expect_identical(get_formula(fit), hp ~ cyl)
  expect_identical(get_response(model), "hp")
  expect_identical(get_response(fit), "hp")
  expect_identical(get_predictors(model), "cyl")
  expect_identical(get_predictors(fit), "cyl")

  # test prediction
  expect_identical(names(pred), c("result", "warnings", "errors"))
  expect_identical(names(pred_tbl), c("result", "warnings", "errors"))
  expect_identical(names(pred2), c("result", "warnings", "errors"))
  expect_identical(names(pred2_tbl), c("result", "warnings", "errors"))
  expect_identical(predict(fit, mtcars, as_tibble = FALSE), pred_from_model)
  expect_identical(predict(fit_tbl, mtcars), pred_from_model_tbl)

  # test prediction accessors
  expect_type(get_errors(pred_with_error), "character")
  expect_null(get_result(pred_with_error))
  expect_identical(
    names(get_result(pred)),
    c("hp", "cyl", "estimate", "lower_pi", "upper_pi")
  )
  expect_identical(
    names(get_result(pred_tbl)[[1]]),
    c(names(mtcars), "estimate", "lower_pi", "upper_pi")
  )
  expect_identical(
    names(get_result(pred2)),
    c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
  )
  expect_identical(
    names(get_result(pred2_tbl)[[1]]),
    c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
  )
  expect_type(get_warnings(pred), "character")
  expect_type(get_warnings(pred_tbl)[[1]], "character")
  expect_null(get_warnings(pred2))
  expect_null(get_warnings(pred2_tbl)[[1]])
  expect_null(get_errors(pred))
  expect_null(get_errors(pred_tbl)[[1]])
  expect_null(get_errors(pred2))
  expect_null(get_errors(pred2_tbl)[[1]])

})
