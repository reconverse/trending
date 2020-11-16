#' @importFrom stats fitted setNames family qnorm qt
NULL

# Generic ----------------------------------------------------------------------
add_confidence_interval <- function(model, data, alpha, ...) {
  UseMethod("add_confidence_interval")
}


# default method ---------------------------------------------------------------
add_confidence_interval.default <- function(model, data, alpha, ...) {
  stop("Cannot add confidence interval for this model type")
}


# linear model method ----------------------------------------------------------
add_confidence_interval.lm <- function(model, data, alpha, ...) {
  ci <- as.data.frame(
    predict(model, data, interval = "confidence", level = 1 - alpha)
  )
  ci <- setNames(ci, c("estimate", "lower_ci", "upper_ci"))
  cbind(data, ci)
}


# glm model method -------------------------------------------------------------
add_confidence_interval.glm <- function(model, data, alpha, ...) {
  out <- predict(model, data, se.fit = TRUE, type = "link")
  if (family(model)$family %in% c("binomial", "poisson")) {
    critical_val <- qnorm(p = 1 - alpha / 2, mean = 0, sd = 1)
  } else {
    critical_val <- qt(p = 1 - alpha / 2, df = model$df.residual)
  }
  # use the above output to generate a confidence interval
  inverse_link <- family(model)$linkinv
  pred <- inverse_link(out$fit)
  upper <- inverse_link(out$fit + critical_val * out$se.fit)
  lower <- inverse_link(out$fit - critical_val * out$se.fit)

  # account for a decreasing link function
  if (model$family$link %in% c("inverse", "1/mu^2")) {
    upr <- lower
    lower <- upper
    upper <- upr
  }
  # add the interval to the original data
  cbind(
    data,
    data.frame(estimate = pred, lower_ci = lower, upper_ci = upper)
  )
}


# brms model method ------------------------------------------------------------
add_confidence_interval.brmsfit <- function(model, data, alpha, ...) {
  ci <- fitted(model, data, probs = c(alpha / 2, 1 - alpha / 2))[-2]
  ci <- setNames(ci, c("estimate", "lower_ci", "upper_ci"))
  cbind(data, ci)
}