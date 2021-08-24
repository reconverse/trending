test_that("functions error as expected", {

  # data as argument
  expect_error(lm_model(hp ~ cyl, data = mtcars))
  expect_error(glm_model(hp ~ cyl, data = mtcars))
  expect_error(glm_nb_model(hp ~ cyl, data = mtcars))
  expect_error(brm_model(hp ~ cyl, data = mtcars))

  # incorrect argument types
  expect_error(lm_model("bob"))
  expect_error(fit("bob"))
  expect_error(fit(list("bob")))
  expect_error(get_result("bob"))
  expect_error(get_warnings("bob"))
  expect_error(get_errors("bob"))
  expect_error(get_fitted_data("bob"))
  expect_error(get_fitted_model("bob"))
  expect_error(get_formula("bob"))
  expect_error(get_response("bob"))
  expect_error(get_predictors("bob"))
})
