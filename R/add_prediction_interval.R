#' @importFrom stats setNames family qnbinom qpois qgamma qbinom qt
NULL

# Generic ----------------------------------------------------------------------
add_prediction_interval <- function(model, data, alpha, ...) {
  UseMethod("add_prediction_interval")
}


# default method ---------------------------------------------------------------
add_prediction_interval.default <- function(model, data, alpha, ...) {
  stop("Cannot add prediction interval for this model type")
}


# linear model method ----------------------------------------------------------
add_prediction_interval.lm <- function(model, data, alpha, ...) {
  pi <- predict(model, data, interval = "prediction", level = 1 - alpha)
  pi <- setNames(as.data.frame(pi)[, -1], c("lower_pi", "upper_pi"))
  cbind(data, pi)
}


# glm model method -------------------------------------------------------------
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


# brms model method ------------------------------------------------------------
add_prediction_interval.brmsfit <- function(model, data, alpha, ...) {
  pi <- brms::predictive_interval(model, newdata = data, prob = 1 - alpha)
  pi <- setNames(as.data.frame(pi)[, 1:2], c("lower_pi", "upper_pi"))
  cbind(data, pi)
}