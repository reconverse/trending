test_that("glm_nb_model", {

  # setup
  model <- glm_nb_model(hp ~ cyl)
  fit <- fit(model, mtcars)
  fitted_model <- get_fitted_model(fit)
  fitted_data <- get_fitted_data(fit)
  expected_model <- glm.nb(hp ~ cyl, data = mtcars)

  # test printing
  expect_snapshot(glm_nb_model(count ~ day, na.action = na.exclude))

  # test model fitting - note we need to ignore the call as we have a similar
  # issue to the update function discussed in
  # https://stat.ethz.ch/pipermail/r-help/2021-August/471811.html
  no_call <- function(x) noCall <- function(x) x[setdiff(names(x), "call")]
  expect_equal(no_call(fitted_model), no_call(expected_model), ignore_function_env = TRUE)

  # test accessors
  expect_true(inherits(fitted_model, "negbin"))
  expect_identical(fitted_data, mtcars[(c("hp", "cyl"))])
  expect_identical(get_formula(model), hp ~ cyl)
  expect_identical(get_formula(fit), hp ~ cyl)
  expect_identical(get_response(model), "hp")
  expect_identical(get_response(fit), "hp")

  # test prediction with new data
  pred <- predict(fit, mtcars, add_ci = FALSE, add_pi = FALSE)
  expect_identical(names(pred), c("result", "warnings", "errors"))
  expect_identical(
    names(pred$result),
    c(names(mtcars), "estimate")
  )

  # test prediction with no new data
  pred2 <- predict(fit, add_ci = FALSE, add_pi = FALSE)
  expect_identical(names(pred2), c("result", "warnings", "errors"))
  expect_identical(names(pred2$result), c("hp", "cyl", "estimate"))
})
