test_that("invariants hold with dplyr operations", {

  skip_if_not_installed("dplyr")

  # setup
  model1 <- lm_model(hp ~ cyl)
  model2 <- glm_model(hp ~ cyl, family = poisson)
  fit_tbl <- fit(list(model1, model2), mtcars)
  pred_tbl <- predict(fit_tbl)
  estimate <- get_result(pred_tbl)[[1]]
  restimate <- estimate
  names(restimate)[1] <- "bob"
  fit_tbl2 <- fit_tbl
  names(fit_tbl2)[1] <- "bob"
  pred_tbl2 <- pred_tbl
  names(pred_tbl2)[1] <- "bob"
  fit_tbl_nmd <- fit(list(m1=model1, m2=model2), mtcars)
  pred_tbl_nmd <- predict(list(m1=model1, m2=model2), mtcars)

  all_fit_tbl <- dplyr::select(fit_tbl, dplyr::everything())
  all_pred_tbl <- dplyr::select(pred_tbl, dplyr::everything())
  all_estimate <- dplyr::select(estimate, dplyr::everything())
  row_fit_tbl <- dplyr::slice_head(fit_tbl, n = 1)
  row_pred_tbl <- dplyr::slice_head(pred_tbl, n = 1)
  row_estimate <- dplyr::slice_head(estimate, n = 1)
  col_fit_tbl <- dplyr::select(fit_tbl, result)
  col_fit_tbl_nmd <- dplyr::select(fit_tbl_nmd, warnings)
  col_fit_tbl_nmd2 <- dplyr::select(fit_tbl_nmd, errors)
  col_pred_tbl <- dplyr::select(pred_tbl, result)
  col_pred_tbl_nmd <- dplyr::select(pred_tbl_nmd, warnings)
  col_pred_tbl_nmd2 <- dplyr::select(pred_tbl_nmd, errors)
  col_estimate <- dplyr::select(estimate, "hp")

  expect_s3_class(all_fit_tbl, "trending_fit_tbl")
  expect_s3_class(row_fit_tbl, "trending_fit_tbl")
  expect_s3_class(all_pred_tbl, "trending_predict_tbl")
  expect_s3_class(row_estimate, "trending_prediction")
  expect_s3_class(all_estimate, "trending_prediction")
  expect_s3_class(row_pred_tbl, "trending_predict_tbl")
  expect_s3_class(restimate, "trending_prediction")
  expect_s3_class(fit_tbl2, "trending_fit_tbl")
  expect_s3_class(pred_tbl2, "trending_predict_tbl")
  expect_false(inherits(col_fit_tbl, "trending_fit_tbl"))
  expect_false(inherits(col_fit_tbl_nmd, "trending_fit_tbl"))
  expect_false(inherits(col_fit_tbl_nmd2, "trending_fit_tbl"))
  expect_false(inherits(col_pred_tbl, "trending_pred_tbl"))
  expect_false(inherits(col_pred_tbl_nmd, "trending_predict_tbl"))
  expect_false(inherits(col_pred_tbl_nmd2, "trending_predict_tbl"))
  expect_false(inherits(col_estimate, "trending_prediction"))


})
