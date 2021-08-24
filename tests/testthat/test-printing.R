test_that("printing works", {
  l <- lm_model(hp ~ cyl)
  nb <- glm_nb_model(hp ~ cyl)
  fit <- fit(list(l=l,nb=nb), mtcars)
  pred <- predict(fit)
  expect_snapshot(print(fit))
  expect_snapshot(print(pred))
  expect_snapshot(print(pred$result[[1]]))
})
