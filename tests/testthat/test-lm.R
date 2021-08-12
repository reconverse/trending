test_that("lm_model", {

  # setup
  model <- lm_model(hp ~ cyl)
  fit <- fit(model, mtcars)
  fitted_model <- get_fitted_model(fit)
  expected_model <- lm(hp ~ cyl, data = mtcars)

  # test printing
  expect_snapshot(lm_model(count ~ day, na.action = na.exclude))

  # test model fitting - note we need to ignore the call as we have a similar
  # issue to the update function discussed in
  # https://stat.ethz.ch/pipermail/r-help/2021-August/471811.html
  no_call <- function(x) noCall <- function(x) x[setdiff(names(x), "call")]
  expect_equal(no_call(fitted_model), no_call(expected_model), ignore_function_env = TRUE)

  # test accessors
  expect_true(inherits(fitted_model, "lm"))
  expect_identical(get_formula(model), hp ~ cyl)
  expect_identical(get_formula(fit), hp ~ cyl)
  expect_identical(get_response(model), "hp")
  expect_identical(get_response(fit), "hp")

  # test prediction with new data
  pred <- predict(fit, mtcars)
  ok_pred <- ok(pred, unnest = TRUE)
  expect_identical(names(pred), c("output", "prediction_warnings", "prediction_errors"))
  expect_identical(
    names(ok_pred),
    c(names(mtcars), "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
  )

  # test prediction with no new data
  pred2 <- predict(fit, add_pi = FALSE)
  ok_pred2 <- ok(pred2, unnest = TRUE)
  expect_identical(names(pred2), c("output", "prediction_warnings", "prediction_errors"))
  expect_identical(names(ok_pred2), c("hp", "cyl", "estimate", "lower_ci", "upper_ci"))


})
