# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #


# Generics ---------------------------------------------------------------------
add_confidence_interval <- function(model, data, alpha, ...) {
  UseMethod("add_confidence_interval")
}

add_prediction_interval <- function(model, data, alpha, ...) {
  UseMethod("add_prediction_interval")
}


# default methods --------------------------------------------------------------
add_confidence_interval.default <- function(model, data, alpha, ...) {
  stop("Cannot add confidence interval for this model type")
}

add_prediction_interval.default <- function(model, data, alpha, ...) {
  stop("Cannot add prediction interval for this model type")
}


# linear model methods ---------------------------------------------------------
add_confidence_interval.lm <- function(model, data, alpha, ...) {
  ci <- as.data.frame(
    predict(model, data, interval = "confidence", level = 1 - alpha)
  )
  ci <- setNames(ci, c("estimate", "lower_ci", "upper_ci"))
  cbind(data, ci)
}

add_prediction_interval.lm <- function(model, data, alpha, ...) {
  pi <- predict(model, data, interval = "prediction", level = 1 - alpha)
  pi <- setNames(as.data.frame(pi)[, -1], c("lower_pi", "upper_pi"))
  cbind(data, pi)
}


# glm model methods ------------------------------------------------------------
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


add_prediction_interval.glm <- function(model, data, alpha, uncertain, ...) {
  out <- predict(model, data, se.fit = TRUE, type = "link")
  pred <- data$estimate
  lower_ci <- data$lower_ci
  upper_ci <- data$upper_ci

  # prediction intervals
  fam <- family(model)$family
  if (uncertain) {
    lower <- lower_ci
    upper <- upper_ci
  } else {
    lower <- pred
    upper <- pred
  }
  if (inherits(model, "negbin")) {
    theta <- model$theta
    setheta <- model$SE.theta
    data$lower_pi <- qnbinom(alpha / 2, mu = lower, size = theta)
    data$upper_pi <- qnbinom(1 - alpha / 2, mu = upper, size = theta)
  } else if (fam == "poisson") {
    data$lower_pi <- qpois(alpha / 2, lambda = lower)
    data$upper_pi <- qpois(1 - alpha / 2, lambda = upper)
  } else if (fam == "quasipoisson") {
    overdispersion <- summary(model)$dispersion
    data$lower_pi <- qnbinom(alpha / 2, mu = lower, size = lower / (overdispersion - 1))
    data$upper_pi <- qnbinom(1 - alpha / 2, mu = upper, size = upper / (overdispersion - 1))
  } else if (fam == "gamma") {
    overdispersion <- summary(model)$dispersion
    data$lower_pi <- qgamma(alpha / 2, shape = 1 / overdispersion, rate = 1 / (lower * overdispersion))
    data$upper_pi <- qgamma(1 - alpha / 2, shape = 1 / overdispersion, rate = 1 / (upper * overdispersion))
  } else if (fam == "binomial") {
    data$lower_pi <- qbinom(alpha / 2, size = model$prior.weights, prob = lower / model$prior.weights)
    data$upper_pi <- qbinom(1 - alpha / 2, size = model$prior.weights, prob = upper / model$prior.weights)
  } else if (fam == "gaussian") {
    sigma_sq <- summary(model)$dispersion
    se_terms <- out$se.fit
    t_quant <- qt(p = alpha / 2, df = model$df.residual, lower.tail = FALSE)
    se_global <- sqrt(sigma_sq + se_terms^2)
    data$lower_pi <- pred - t_quant * se_global
    data$upper_pi <- pred + t_quant * se_global
  } else {
    stop("Unsupported glm family type")
  }

  # corrections for extremes
  if (isTRUE(all.equal(alpha, 0))) {
    data$lower_pi <- ifelse(is.infinite(lower_ci), -Inf, data$lower_pi)
    data$upper_pi <- ifelse(is.infinite(upper_ci), Inf, data$upper_pi)
    if (inherits(model, "negbin")) {
      data$lower_pi <- ifelse(is.nan(data$lower_pi), 0, data$lower_pi)
    }
  }
  data
}


# brms model methods -----------------------------------------------------------
add_confidence_interval.brmsfit <- function(model, data, alpha, ...) {
  ci <- fitted(model, data, probs = c(alpha / 2, 1 - alpha / 2))[-2]
  ci <- setNames(ci, c("estimate", "lower_ci", "upper_ci"))
  cbind(data, ci)
}


add_prediction_interval.brmsfit <- function(model, data, alpha, ...) {
  pi <- brms::predictive_interval(model, newdata = data, prob = 1 - alpha)
  pi <- setNames(as.data.frame(pi)[, 1:2], c("lower_pi", "upper_pi"))
  cbind(data, pi)
}