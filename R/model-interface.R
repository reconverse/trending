#' Modeling interface
#'
#' These functions wrappers around various modelling tools to ensure a
#' consistent input for *trending* functions. See details for available model
#' interfaces.
#'
#' @details The following interfaces are available:
#'
#' * `lm_model`: interface for linear models implemented in
#'   [`stats::lm`](stats::lm)
#'
#' * `glm_model`: interface for generalised linear models (GLMs) implemented in
#'   `[stats::glm](stats::glm)`
#'
#' * `glm_nb_model`: interface for negative binomial generalied linear models
#'   implemented in [`MASS::glm_nb`](MASS::glm_nb)
#'
#' * `brms_model`: interface for Bayesian regression models implemented in
#'   [`brms::brm`](brms::brm)
#'
#' @param formula the formula of the model, with the response variable on the
#'   left of a tilde symbol, and predictors on the right hand-side; variable
#'   names used in the formula will need to be matched by columns in the `data`
#'   input to other functions
#'
#' @param family the model family to be used for the response variable
#'
#' @param x either a `trending_model` or `trending_model_fit` object
#'
#' @param ... further arguments passed to other methods: `lm` for `lm_model`,
#'   `glm` for `glm_model`, `MASS::glm_nb` for `glm_nb_model`, `brms::brm` for
#'   `brms_model`.  Not used for `print` and `format`.
#'
#' @return  A `trending_model` object (S3 class inheriting `list`), containing
#'   items which can be accessed by various accessors - see
#'   `?trending_model-accessors`
#'
#' @author Dirk Schumacher
#'
#' @aliases trending_model trending_models trending_model_fit
#'
#' @export
#' @rdname trending_model
#' @aliases glm_model
glm_model <- function(formula, family, ...) {
  if (!is.character(family)) {
    family <- deparse(substitute(family))
  }
  structure(
    eval(bquote(list(
      model_class = "glm",
      fit = function(data) {
        model <- glm(formula = .(formula), family = .(family), data = data, ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_glm", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases glm_nb_model
glm_nb_model <- function(formula, ...) {
  check_suggests("MASS")
  structure(
    eval(bquote(list(
      model_class = "MASS::glm.nb",
      fit = function(data) {
        model <- MASS::glm.nb(formula = .(formula), data = data, ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_glm_nb", "trending_model")
  )
}


#' @export
#' @rdname trending_model
#' @aliases lm_model
lm_model <- function(formula, ...) {
  structure(
    eval(bquote(list(
      model_class = "lm",
      fit = function(data) {
        model <- lm(formula = .(formula), data = data, ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_lm", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases brms_model
brms_model <- function(formula, family, ...) {
  check_suggests("brms")
  structure(
    eval(bquote(list(
      model_class = "brms",
      fit = function(data) {
        model <- brms::brm(formula = .(formula), data = data, family = .(family), ...)
        model_fit(model, formula)
      }
    ))),
    class = c("trending_glm", "trending_model")
  )
}



#' @export
#' @rdname trending_model
#' @aliases format.trending_model
format.trending_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  paste0("Untrained trending model type: ", x[["model_class"]])
}



#' @export
#' @rdname trending_model
#' @aliases print.trending_model
print.trending_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  cat(format(x))
}



#' @export
#' @rdname trending_model
#' @aliases format.trending_model_fit
format.trending_model_fit <- function(x, ...) {
  ellipsis::check_dots_empty()
  tmp <- append("Fitted trending model:\n", utils::capture.output(x$model))
  paste(tmp,  collapse = "\n")
}



#' @export
#' @rdname trending_model
#' @aliases print.trending_model_fit
print.trending_model_fit <- function(x, ...) {
  ellipsis::check_dots_empty()
  cat(format(x))
}

# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #

add_intervals <- function(model, data, alpha) {
  UseMethod("add_intervals")
}


add_intervals.default <- function(model, data, alpha) {
  stop("Cannot add intervals for this model type")
}

add_intervals.lm <- function(model, data, alpha) {
  ci <- predict(model, data, interval = "confidence", level = 1 - alpha)
  pi <- predict(model, data, interval = "prediction", level = 1 - alpha)
  intervals <- cbind(as.data.frame(ci), as.data.frame(pi)[,-1])
  colnames(intervals) <- c("pred", "lower-ci", "upper-ci", "lower-pi", "upper-pi")
  dplyr::bind_cols(data, intervals)
}


add_intervals.glm <- function(model, data, alpha = 0.05, uncertain = TRUE) {

  # confidence intervals (~ from ciTools)
  out <- predict(model, data, se.fit = TRUE, type = "link")
  if (family(model)$family %in% c("binomial", "poisson"))
    crit_val <- qnorm(p = 1 - alpha/2, mean = 0, sd = 1)
  else
    crit_val <- qt(p = 1 - alpha/2, df = model$df.residual)
  inverselink <- family(model)$linkinv
  pred <- inverselink(out$fit)
  upper <- inverselink(out$fit + crit_val *  out$se.fit)
  lower <- inverselink(out$fit - crit_val * out$se.fit)
  if (model$family$link %in% c("inverse", "1/mu^2")) {
    ## need to do something like this for any decreasing link
    ## function.
    upr <- lower
    lower <- upper
    upper <- upr
  }
  data$pred <- pred
  data$`lower-ci` <- lower
  data$`upper-ci` <- upper



  # prediction intervals
  fam <- family(model)$family
  if (uncertain) {
    lower <- data$`lower-ci`
    upper <- data$`upper-ci`
  } else {
    lower <- data$pred
    upper <- data$pred
  }
  if (inherits(model, "negbin")) {
    theta <- model$theta
    setheta <- model$SE.theta
    data$`lower-pi` = qnbinom(alpha / 2, mu = lower, size = theta)
    data$`upper-pi` <- qnbinom(1 - alpha / 2, mu = upper, size = theta)
  } else if (fam == "poisson") {
    data$`lower-pi` <- qpois(alpha / 2, lambda = lower)
    data$`upper-pi` <- qpois(1 - alpha / 2, lambda = upper)
  } else if (fam == "quasipoisson") {
    overdispersion <- summary(fit)$dispersion
    data$`lower-pi` <- qnbinom(alpha / 2, mu = lower, size = lower / (overdispersion - 1))
    data$`upper-pi` <- qnbinom(1 - alpha / 2, mu = upper, size = upper / (overdispersion - 1))
  } else if (fam == "gamma") {
    overdispersion <- summary(fit)$dispersion
    data$`lower-pi` <- qgamma(alpha / 2, shape = 1 / overdispersion, rate = 1 / (lower * overdispersion))
    data$`upper-pi` <- qgamma(1 - alpha / 2, shape = 1 / overdispersion, rate = 1 / (upper * overdispersion))
  } else if (fam == "binomial") {
    data$`lower-pi` <- qbinom(alpha / 2, size = model$prior.weights, prob = lower / model$prior.weights)
    data$`upper-pi` <- qbinom(1 - alpha / 2, size = model$prior.weights, prob = upper / model$prior.weights)
  } else if (fam == "gaussian") {
    sigma_sq <- summary(model)$dispersion
    se_terms <- out$se.fit
    t_quant <- qt(p = alpha / 2, df = model$df.residual, lower.tail = FALSE)
    se_global <- sqrt(sigma_sq + se_terms^2)
    data$`lower-pi` <- data$fitted - t_quant * se_global
    data$`upper-pi` <- data$fitted + t_quant * se_global
  } else {
    stop("Unsupported glm family type")
  }

  # corrections for extremes
  if (isTRUE(all.equal(alpha, 0))) {
    data$`lower-pi` <- ifelse(is.infinite(data$`lower-ci`), -Inf, data$`lower-pi`)
    data$`upper-pi` <- ifelse(is.infinite(data$`upper-ci`), Inf, data$`upper-pi`)
    if (inherits(model, "negbin")) {
      data$`lower-pi` <- ifelse(is.nan(data$`lower-pi`), 0, data$`lower-pi`)
    }
  }
  tibble::as_tibble(data)
}


add_intervals.brmsfit <- function(model, data, alpha) {
  fit <- predict(model, data)
  interval <- brms::predictive_interval(
    model,
    newdata = data,
    prob = 1 - alpha
  )
  dat <- dplyr::bind_cols(
    data,
    tibble::tibble(

      pred = fit[, 1],
      `lower-pi` = interval[, 1],
      `upper-pi` = interval[, 2]
    )
  )

  fit <- stats::fitted(model, data, probs = c(alpha/2, 1 - alpha/2))
  colnames(fit)[3:4] <- c("lower-ci", "upper-ci")
  cbind(dat, fit)

}

model_fit <- function(model, formula) {
  out <- list(
    model = model,
    predict = function(newdata, alpha = 0.05) {
      suppressMessages(
        res <- add_intervals(
          data = newdata,
          model = model,
          alpha = alpha)
      )
      col_name <- as.character(formula[[2]])
      res <- append_observed_column(res, res[[col_name]])
      class(res) <- c("trending_model_prediction", class(res))
      res
    }

  )
  class(out) <- c("trending_model_fit", class(out))
  out
}


append_observed_column <- function(data, value) {
  data[["observed"]] <- value
  data
}


