test_that("printing", {
  x <- 1:10
  y <- 2 * x + 3
  dat <- data.frame(x, y)
  model_lm <- lm_model(y ~ x)
  fitted_model <- fit(model_lm, dat)

  expect_output(print(model_lm), "Untrained trending model type: lm")
  expect_output(print(fitted_model), "Fitted trending model:\n")
  expect_output(print(fitted_model), "lm")
})
