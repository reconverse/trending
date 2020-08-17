test_that("lm_model", {
  model <- lm_model(hp ~ cyl)
  suppressWarnings(fit <- fit(model, mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  nms <- c(names(mtcars), "pred", "lower", "upper", "observed")

  expect_true(inherits(fit$model, "lm"))
  expect_true(inherits(fit, "trending_model_fit"))
  expect_identical(names(pred), nms)
})

test_that("glm_model", {
  model <- glm_model(hp ~ cyl, family = poisson)
  suppressWarnings(fit <- fit(model, mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  nms <- c(names(mtcars), "pred", "lower", "upper", "observed")

  expect_true(inherits(fit$model, "glm"))
  expect_true(inherits(fit, "trending_model_fit"))
  expect_identical(names(pred), nms)
})

test_that("glm_nb_model", {
  model <- glm_nb_model(hp ~ cyl)
  suppressWarnings(fit <- fit(model, mtcars))
  suppressWarnings(pred <- predict(fit, mtcars))
  nms <- c(names(mtcars), "pred", "lower", "upper", "observed")

  expect_true(inherits(fit$model, "negbin"))
  expect_true(inherits(fit, "trending_model_fit"))
  expect_identical(names(pred), nms)
})

# test_that("brms", {
#   skip_on_cran()
#   model <- brms_model(hp ~ cyl, family = brms::negbinomial())
#   suppressWarnings(fit <- fit(model, mtcars))
#   suppressWarnings(pred <- predict(fit, mtcars))
#   nms <- c(names(mtcars), "pred", "lower", "upper", "observed")
#
#   expect_true(inherits(fit$model, "brmsfit"))
#   expect_true(inherits(fit, "trending_model_fit"))
#   expect_identical(names(pred), nms)
# })

