
test_that("evaluate_aic", {


    x <- 1:10
    y <- 2*x + 3
    dat <- data.frame(x, y)
    model_lm <- lm_model(y ~ x)
    res <- evaluate_aic(model_lm, dat)

    expect_equal(colnames(res), c("metric", "score"))
    expect_equal(res$metric, "aic")
  })

