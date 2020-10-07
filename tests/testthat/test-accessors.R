test_that("trending_model accessors", {
  model_lm <- lm_model(y ~ x)
  model_glm <- glm_model(y ~ x, family = poisson)

  expect_identical(get_formula(model_lm), y ~ x)
  expect_identical(get_response(model_lm), "y")
})

test_that("trending_model_fit accessors", {
  x <- 1:10
  y <- 2 * x + 3
  dat <- data.frame(x, y)
  model_lm <- lm_model(y ~ x)
  fitted_model <- fit(model_lm, dat)
  getmod <- get_model(fitted_model)
  expected_coefficients <- c(`(Intercept)` = 3, x = 2)

  expect_equal(getmod$coefficients, expected_coefficients)
})
