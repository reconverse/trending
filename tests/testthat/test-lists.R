test_that("lm_model", {

  # setup
  l <- lm_model(hp ~ cyl)
  nb <- glm_nb_model(hp ~ cyl)
  lfit <- fit(l, mtcars)
  lpred <- predict(lfit)
  lfit_tbl <- fit(l, mtcars, as_tibble = TRUE)
  lpred_tbl <- predict(lfit, mtcars, as_tibble = TRUE)
  nbfit <- fit(nb, mtcars)
  nbpred <- predict(nbfit)
  nbfit_tbl <- fit(nb, mtcars, as_tibble = TRUE)
  nbpred_tbl <- predict(nbfit_tbl, mtcars)
  list_fit <- fit(list(l, nb), mtcars)
  list_fit_nmd <- fit(list(l=l, nb=nb), mtcars)
  list_pred <- predict(list_fit)
  list_pred_nmd <- predict(list_fit_nmd, mtcars)
  list_pred_from_model <- predict(list(l=l, nb=nb), mtcars)

  expect_equal(get_warnings(list_fit), list(NULL, NULL))
  expect_equal(get_warnings(list_pred_from_model), list(l=NULL, nb=NULL))
  expect_equal(get_errors(list_fit), list(NULL, NULL))
  expect_equal(get_errors(list_pred_from_model), list(l=NULL, nb=NULL))
  expect_equal(list_pred_nmd, list_pred_from_model)

  expect_equal(
    list(lfit$result, nbfit$result),
    get_fitted_model(list_fit),
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )

  expect_equal(names(list_fit_nmd), c("model_name", "result", "warnings", "errors"))

  expect_equal(list_fit_nmd$model_name, c("l", "nb"))

  expect_equal(
    list(lpred$result, nbpred$result),
    get_result(list_pred),
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )

  expect_equal(
    list(lpred_tbl$result[[1]], nbpred_tbl$result[[1]]),
    setNames(get_result(list_pred_nmd), NULL),
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )

  expect_equal(names(list_pred_nmd), c("model_name", "result", "warnings", "errors"))

  expect_equal(list_pred_nmd$model_name, c("l", "nb"))

})