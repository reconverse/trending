# test_that("lm_model", {
#
#   # setup
#   model <- lm_model(hp ~ cyl)
#   fit <- fit(model, mtcars)
#
#   # test accessors
#   fitted_model <- get_fitted_model(fit)
#   expect_true(inherits(fitted_model, "lm"))
#   expect_identical(get_formula(model), hp ~ cyl)
#   expect_identical(get_formula(fitted_model), hp ~ cyl)
#   expect_identical(get_response(model), "hp")
#   expect_identical(get_response(fitted_model), "hp")
#
#   # test prediction with new data
#   pred <- predict(fit, mtcars, add_pi = TRUE)
#   expect_identical(
#     names(pred),
#     c(names(mtcars), "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
#   )
#
#   # test prediction with no new data
#   pred2 <- pred(fit)
#   expect_identical(
#     names(pred2),
#     c("hp", "cyl", "estimate", "lower_ci", "upper_ci")
#   )
# })

# -------------------------------------------------------------------------

# test_that("glm_model", {
#   model <- glm_model(hp ~ cyl, family = poisson)
#   fit <- fit_model(model, mtcars)
#   fitted_model <- get_fitted_model(fit)
#   pred <- add_estimate(fit, mtcars)
#   suppressWarnings(pred2 <- add_estimate(fit, add_pi = TRUE))
#
#   expect_true(inherits(fitted_model, "glm"))
#
#   expect_identical(
#     names(pred),
#     c(names(mtcars), "estimate", "lower_ci", "upper_ci")
#   )
#   expect_identical(
#     names(pred2),
#     c("hp", "cyl", "estimate", "lower_ci", "upper_ci", "lower_pi", "upper_pi")
#   )
# })
#
# # -------------------------------------------------------------------------
#
# test_that("glm.nb_model", {
#   model <- glm.nb_model(hp ~ cyl)
#   fit <- fit_model(model, mtcars)
#   pred <- add_estimate(fit, mtcars, add_ci = FALSE)
#   pred2 <- add_estimate(fit, add_ci = FALSE, add_pi = TRUE, pi_names = c("l", "u"))
#
#   expect_true(inherits(fit, "negbin"))
#   expect_identical(names(pred), c(names(mtcars), "estimate"))
#   expect_identical(names(pred2), c("hp", "cyl", "estimate", "l", "u"))
# })
#
# # -------------------------------------------------------------------------
#
# test_that("list models", {
#   model <- glm_model(hp ~ cyl, family = poisson)
#   model2 <- glm.nb_model(hp ~ cyl)
#
#   fit <- fit_model(list(model, model2), mtcars)
#   pred <- add_estimate(fit, mtcars)
#   pred2 <- add_estimate(fit)
#
#   expect_true(inherits(fit, "trending_fit"))
#   expect_true(inherits(pred, "tbl_df"))
#   expect_true(inherits(pred2, "tbl_df"))
#   expect_identical(
#     names(pred),
#     c("output",
#       "fitted_model",
#       "prediction_data",
#       "fitting_warnings",
#       "fitting_errors",
#       "prediction_warnings",
#       "prediction_errors"
#     )
#   )
#
#   expect_error(
#     fit_model(list(model, "bob"), mtcars),
#     "list entrys should be `trending_model` objects"
#   )
# })
