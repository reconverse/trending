test_that("brm_model", {

  testthat::skip_if_not_installed("brms")
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  # setup
  model <- brm_model(hp ~ cyl, family = poisson)
  fit <- fit(model, mtcars, as_tibble = FALSE)
  fit_tbl <- fit(model, mtcars)
  pred <- predict(fit, mtcars, as_tibble = FALSE)           # prediction with new data
  pred_tbl <- predict(fit, mtcars)                          # prediction as tibble from list
  pred2 <- predict(fit, add_pi = FALSE, as_tibble = FALSE)  # prediction with no new data or pi
  pred2_tbl <- predict(fit_tbl, add_pi = FALSE)             # prediction from tibble

  # test printing
  expect_snapshot(brm_model(count ~ day, na.action = na.exclude))

  # test model fitting
  fitted_model <- get_fitted_model(fit)
  fitted_model_tbl <- get_fitted_model(fit_tbl)
  expect_true(inherits(fitted_model, "brmsfit"))
  expect_true(inherits(fitted_model_tbl[[1]], "brmsfit"))
  expect_identical(names(fit), c("result", "warnings", "errors"))
  expect_identical(names(fit_tbl), c("result", "warnings", "errors"))
  expect_s3_class(fit_tbl, "tbl_df")

  # test fit accessors
  expect_identical(get_result(fit), fitted_model)
  expect_null(get_warnings(fit))
  expect_null(get_errors(fit))

  ## note: converting to matrix as a rogue attribute $drop_unused_levels poses problems
  expect_identical(as.matrix(get_fitted_data(fit)), as.matrix(mtcars[c("hp", "cyl")]))
  expect_identical(get_formula(model), hp ~ cyl)
  fml <- get_formula(fit)$formula
  attributes(fml) <- NULL
  expect_identical(as.formula(fml), hp ~ cyl)
  expect_identical(get_response(model), "hp")
  expect_identical(get_response(fit), "hp")
  expect_identical(get_predictors(model), "cyl")
  expect_identical(get_predictors(fit), "cyl")

  # test prediction
  expect_identical(names(pred), c("result", "warnings", "errors"))
  expect_identical(names(pred_tbl), c("result", "warnings", "errors"))
  expect_identical(names(pred2), c("result", "warnings", "errors"))
  expect_identical(names(pred2_tbl), c("result", "warnings", "errors"))

  # test prediction accessors
  expect_identical(
    names(get_result(pred)),
    c(names(mtcars), "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
  )
  expect_identical(
    names(get_result(pred_tbl)[[1]]),
    c(names(mtcars), "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
  )
  expect_identical(
    names(get_result(pred2)),
    c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
  )
  expect_identical(
    names(get_result(pred2_tbl)[[1]]),
    c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
  )
  expect_null(get_warnings(pred))
  expect_null(get_warnings(pred_tbl)[[1]])
  expect_null(get_warnings(pred2))
  expect_null(get_warnings(pred2_tbl)[[1]])
  expect_null(get_errors(pred))
  expect_null(get_errors(pred_tbl)[[1]])
  expect_null(get_errors(pred2))
  expect_null(get_errors(pred2_tbl)[[1]])

})
