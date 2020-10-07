test_that("lm_model", {
  model <- lm_model(hp ~ cyl)
  suppressWarnings(fit <- fit(model, mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  suppressWarnings(pred2 <- predict(fit))

  nms <- c(names(mtcars), c("estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi"))
  nms2 <- c("hp", "cyl", c("estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi"))

  expect_true(inherits(fit$fitted_model, "lm"))
  expect_true(inherits(fit, "trending_model_fit"))
  expect_identical(names(pred), nms)
  expect_identical(names(pred2), nms2)
})

test_that("glm_model", {
  model <- glm_model(hp ~ cyl, family = poisson)
  suppressWarnings(fit <- fit(model, mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  suppressWarnings(pred2 <- predict(fit))

  nms <- c(names(mtcars), c("estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi"))
  nms2 <- c("hp", "cyl", c("estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi"))

  expect_true(inherits(fit$fitted_model, "glm"))
  expect_true(inherits(fit, "trending_model_fit"))
  expect_identical(names(pred), nms)
  expect_identical(names(pred2), nms2)
})

test_that("glm_nb_model", {
  model <- glm_nb_model(hp ~ cyl)
  suppressWarnings(fit <- fit(model, mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  suppressWarnings(pred2 <- predict(fit))

  nms <- c(names(mtcars), c("estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi"))
  nms2 <- c("hp", "cyl", c("estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi"))

  expect_true(inherits(fit$fitted_model, "negbin"))
  expect_true(inherits(fit, "trending_model_fit"))
  expect_identical(names(pred), nms)
  expect_identical(names(pred2), nms2)
})

test_that("list models", {
  model <- glm_model(hp ~ cyl, family = poisson)
  model2 <- glm_nb_model(hp ~ cyl)

  suppressWarnings(fit <- fit(list(model, model2), mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  suppressWarnings(pred2 <- predict(fit))

  expect_true(inherits(fit, "trending_model_fit_list"))
  expect_true(inherits(pred, "list"))
  expect_true(inherits(pred2, "list"))

  expect_error(
    fit(list(model, "bob"), mtcars),
    "list entrys should be `trending_model` objects"
  )
})

# test_that("brms", {
#   skip_on_cran()
#   model <- brms_model(hp ~ cyl, family = brms::negbinomial())
#   suppressWarnings(fit <- fit(model, mtcars))
#   suppressWarnings(pred <- predict(fit, mtcars))
#   nms <- c(names(mtcars), "estimate", "lower", "upper", "observed")
#
#   expect_true(inherits(fit$model, "brmsfit"))
#   expect_true(inherits(fit, "trending_model_fit"))
#   expect_identical(names(pred), nms)
# })
