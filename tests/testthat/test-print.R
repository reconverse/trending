test_that("trending_model printing", {
  model_lm <- lm_model(y ~ x)
  expect_output(print(model_lm), "Untrained trending model type: lm")
})
