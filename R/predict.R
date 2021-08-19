#' Predict method for trending_fit objects
#'
#' `predict()` adds estimated values and associated confidence and/or
#'   prediction intervals for various fitted models.
#'
#' @param object A [`trending_fit`][fit.trending_model()],
#'    [`trending_fit_list`][fit_list()] or `trending_fit_tbl` object.
#' @param new_data A `data.frame` containing data for which estimates are to be
#'   derived. If missing, the model frame from the fit data will be used.
#' @param name Character vector of length one giving the name to use for the
#'   calculated estimate.
#' @param alpha The alpha threshold to be used for prediction intervals,
#'   defaulting to 0.05, i.e. 95% prediction intervals are derived.
#' @param add_ci Should a confidence interval be added to the output.
#'   Default TRUE.
#' @param ci_names Names to use for the resulting confidence intervals.
#' @param add_pi Should a prediction interval be added to the output.
#'   Default TRUE.
#' @param pi_names Names to use for the resulting prediction intervals.
#' @param simulate_pi Should the prediction intervals for glm models be
#'   simulated. If TRUE, default, `predict()` uses the [ciTools::add_pi()]
#'   function to generate the intervals.
#' @param sims The number of simulations to run when simulating prediction
#'   intervals for a glm model.
#' @param uncertain Only used for glm models and when `simulate_pi = FALSE`.
#'   Default TRUE.  If FALSE uncertainty in the fitted parameters is ignored
#'   when generating the parametric prediction intervals.
#' @param as_tibble Should the output be converted to a tibble subclass.
#' @param ... Not currently used.
#'
#' @examples
#' x = rnorm(100, mean = 0)
#' y = rpois(n = 100, lambda = exp(1.5 + 0.5*x))
#' dat <- data.frame(x = x, y = y)
#'
#' poisson_model <- glm_model(y ~ x , family = "poisson")
#' negbin_model <- glm_nb_model(y ~ x)
#'
#' fitted_poisson <- fit(poisson_model, dat)
#' fitted_negbin <- fit(negbin_model, dat)
#' fitted_list <- fit_list(list(poisson_model, negbin_model), dat)
#' fitted_list_named <- fit_list(list(p = poisson_model, n = negbin_model), dat)
#'
#' predict(fitted_poisson)
#' predict(fitted_list)
#' predict(fitted_list, as_tibble = TRUE)
#' predict(fitted_list_named)
#' predict(fitted_list_named, as_tibble = TRUE)
#'
#' @returns For [`trending_fit`][fit.trending_model()] inputs, if
#'   `as_tibble = FALSE`, a `trending_predict` object, which is a list subclass,
#'   with entries:
#'
#'   - result: the input data frame with additional estimates and, optionally,
#'     confidence and or prediction intervals. `NULL` if the associated
#'     `predict` method fails.
#'
#'   - warnings: any warnings generated during prediction.
#'
#'   - errors: any errors generated during prediction.
#'
#'   If `as_tibble = TRUE`, a `trending_predict_tbl` object which is a
#'   [`tibble`][tibble::tibble()] subclass with one row and columns 'result',
#'   'warnings' and 'errors' with contents as above.
#'
#'   For [`trending_fit_list`][fit_list()] or [`trending_fit_tbl`][fit_list()]
#'   inputs, if `as_tibble = FALSE`, a `trending_fit_list` where each entry is a
#'   `trending_fit` object. If `as_tibble = TRUE` a `trending_predict_tbl`
#'   object with each row corresponding to the output of one model as above.
#'
#' @export
predict.trending_fit <- function(object, new_data, name = "estimate", alpha = 0.05,
                                 add_ci = TRUE, ci_names = c("lower_ci", "upper_ci"),
                                 add_pi = TRUE, pi_names = c("lower_pi", "upper_pi"),
                                 simulate_pi = FALSE, sims = 2000,
                                 uncertain = TRUE, as_tibble = FALSE, ...) {

  # ensure that we have a model to use for predictions
  fitted_model <- get_fitted_model.trending_fit(object)

  # if no data supplied we use the model data
  if (missing(new_data)) new_data <- get_fitted_data(object)

  # check for name clashes with the existing input
  check_names(
    new_data = new_data,
    name = name,
    add_ci = add_ci,
    ci_names = ci_names,
    add_pi = add_pi,
    pi_names = pi_names
  )

  res <- predict_individual(
    fitted_model,
    new_data = new_data,
    response = get_response.trending_fit(object),
    predictors = get_predictors.trending_fit(object),
    name = name,
    alpha = alpha,
    add_ci = add_ci,
    ci_names = ci_names,
    add_pi = add_pi,
    pi_names = pi_names,
    simulate_pi = simulate_pi,
    sims = sims,
    uncertain = uncertain
  )

  if (as_tibble) {
    res <- list(res)
    res <- trending_fit_list_to_tibble(res)
  }
  res
}

#' @rdname predict.trending_fit
#' @aliases predict.trending_fit_list
#' @export
predict.trending_fit_list <- function(object, new_data, name = "estimate", alpha = 0.05,
                                      add_ci = TRUE, ci_names = c("lower_ci", "upper_ci"),
                                      add_pi = TRUE, pi_names = c("lower_pi", "upper_pi"),
                                      simulate_pi = FALSE, sims = 2000, uncertain = TRUE,
                                      as_tibble = FALSE, ...) {

  # if no data supplied we use the model data
  fitted_models <- get_fitted_model(object)
  if (missing(new_data)) {
    ok <- vapply(fitted_models, function(x) !is.null(x), logical(1))
    new_data <- if (!any(ok)) NULL else get_fitted_data(object)[ok][[1]] # OK as data will be same for all
  }

  res <- .mapply(
    FUN = predict_individual,
    dots = list(
      model = fitted_models,
      response = get_response(object),
      predictors = get_predictors(object)
      ),
    MoreArgs = list(
      new_data = new_data,
      name = name,
      alpha = alpha,
      add_ci = add_ci,
      ci_names = ci_names,
      add_pi = add_pi,
      pi_names = pi_names,
      simulate_pi = simulate_pi,
      sims = sims,
      uncertain = uncertain
    )
  )

  nms <- attr(object, "model_name")
  if (!is.null(nms)) names(res) <- object[[nms]]
  class(res) <- c("trending_predict_list", class(res))
  if (as_tibble) res <- trending_predict_list_to_tibble(res)
  res
}

#' @rdname predict.trending_fit
#' @aliases predict.trending_fit_tbl
#' @export
predict.trending_fit_tbl <- predict.trending_fit_list

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

predict_individual <- function(model, new_data, response, predictors,
                               name = "estimate", alpha = 0.05,
                               add_ci = TRUE, ci_names = c("lower_ci", "upper_ci"),
                               add_pi = TRUE, pi_names = c("lower_pi", "upper_pi"),
                               simulate_pi = FALSE, sims = 2000, uncertain = TRUE) {

  # wrap add_estimate to catch warnings and errors
  fun <- make_catcher(add_prediction)

  out <- fun(
    model,
    new_data = new_data,
    name = name,
    alpha = alpha,
    add_ci = add_ci,
    ci_names = ci_names,
    add_pi = add_pi,
    pi_names = pi_names,
    simulate_pi = simulate_pi,
    sims = sims,
    uncertain = uncertain
  )

  # only save attributes that are used
  if (!add_ci) ci_names <- NULL
  if (!add_pi) pi_names <- NULL

  # make result a subclass of tibble
  result <- out$result
  if (!is.null(result)) {
    result <- new_tibble(
      result,
      response = response,
      predictors = predictors,
      estimate = name,
      ci_names = ci_names,
      pi_names = pi_names,
      nrow = nrow(result),
      class = "trending_estimate"
    )
    out$result <- result
  }

  res <- structure(out, class = c("trending_predict", class(out)))
}

# this is written like a generic but avoids unnecessary method exports
add_prediction <- function(object, new_data, ...) {
  switch(
    class(object)[1],
    lm = add_prediction_lm(object = object, new_data = new_data, ...),
    glm = add_prediction_glm(object = object, new_data = new_data, ...),
    negbin = add_prediction_glm(object = object, new_data = new_data, ...),
    brmsfit = add_prediction_brmsfit(object = object, new_data = new_data, ...),
    not_implemented(object, call. = TRUE)
  )
}

trending_predict_list_to_tibble <- function(x, ...) {
  res <- lapply(seq_along(x[[1]]), function(i) lapply(x, "[[", i))
  res <- tibble(result = res[[1]], warnings = res[[2]], errors = res[[3]])
  nms <- names(x)
  model_name <- NULL
  if (!is.null(nms)) {
    res <- cbind(tibble(model_name = nms), res)
    model_name <- "model_name"
  }
  res <- new_tibble(
    res,
    model_name = model_name,
    result = "result",
    warnings = "warnings",
    errors = "errors",
    nrow = nrow(res),
    class = "trending_predict_tbl"
  )
  validate_tibble(res)
}

# -------------------------------------------------------------------------

add_prediction_lm <- function(object, new_data, name, alpha, add_ci, ci_names,
                              add_pi, pi_names, ...) {

  # add confidence intervals using ciTools::add_ci
  out <- add_ci(
    df = new_data,
    fit = object,
    alpha = alpha,
    names = ci_names,
    yhatName = name
  )

  # optionally remove confidence intervals (leaves the estimate)
  if (!add_ci) out[ci_names] <- NULL

  # optionally add prediction intervals using ciTools::add_pi
  if (add_pi) {
    out <- add_pi(
      df = out,
      fit = object,
      alpha = alpha,
      names = pi_names,
      yhatName = name
    )
  }

  # ensure result is tibble
  as_tibble(out)
}

# -------------------------------------------------------------------------

add_prediction_glm <- function(object, new_data, name, alpha, add_ci, ci_names,
                               add_pi, pi_names, simulate_pi, sims, uncertain,
                               ...) {

  # add confidence intervals using ciTools::add_ci
  out <- add_ci(
    df = new_data,
    fit = object,
    alpha = alpha,
    names = ci_names,
    yhatName = name
  )

  # optionally add prediction intervals
  if (add_pi && simulate_pi) {         # simulated intervals use ciTools::add_pi
    resp <- as.character(formula(object)[2])
    if (is.null(new_data[[resp]])) {   # Hack to fix bug in ciTools
      out[[resp]] <- out[[name]]
      out <- add_pi(
        df = out,
        fit = object,
        alpha = alpha,
        names = pi_names,
        yhatName = "predpred",
        nSims = sims
      )
      out[[resp]] <- NULL
    } else {
      out <- add_pi(
        df = out,
        fit = object,
        alpha = alpha,
        names = pi_names,
        yhatName = "predpred",
        nSims = sims
      )
    }
    out$predpred <- NULL
  } else if (add_pi) { # "analytic" intervals

    pred <- out[[name]]
    if (uncertain) {
      lower_ci <- out[[ci_names[1]]]
      upper_ci <- out[[ci_names[2]]]
    } else {
      lower_ci <- pred
      upper_ci <- pred
    }
    lower_pi_nm <- pi_names[1]
    upper_pi_nm <- pi_names[2]

    # prediction intervals
    fam <- family(object)$family

    if (inherits(object, "negbin")) {
      theta <- object$theta
      setheta <- object$SE.theta
      out[[lower_pi_nm]] <- qnbinom(alpha / 2, mu = lower_ci, size = theta)
      out[[upper_pi_nm]] <- qnbinom(1 - alpha / 2, mu = upper_ci, size = theta)
    } else if (fam == "poisson") {
      out[[lower_pi_nm]] <- qpois(alpha / 2, lambda = lower_ci)
      out[[upper_pi_nm]] <- qpois(1 - alpha / 2, lambda = upper_ci)
    } else if (fam == "quasipoisson") {
      overdispersion <- summary(object)$dispersion
      out[[lower_pi_nm]] <- qnbinom(alpha / 2, mu = lower_ci, size = lower_ci / (overdispersion - 1))
      out[[upper_pi_nm]] <- qnbinom(1 - alpha / 2, mu = upper_ci, size = upper_ci / (overdispersion - 1))
    } else if (fam == "gamma") {
      overdispersion <- summary(object)$dispersion
      out[[lower_pi_nm]] <- qgamma(alpha / 2, shape = 1 / overdispersion, rate = 1 / (lower_ci * overdispersion))
      out[[upper_pi_nm]] <- qgamma(1 - alpha / 2, shape = 1 / overdispersion, rate = 1 / (upper_ci * overdispersion))
    } else if (fam == "binomial") {
      out[[lower_pi_nm]] <- qbinom(alpha / 2, size = object$prior.weights, prob = lower_ci / object$prior.weights)
      out[[upper_pi_nm]] <- qbinom(1 - alpha / 2, size = object$prior.weights, prob = upper_ci / object$prior.weights)
    } else if (fam == "gaussian") {
      sigma_sq <- summary(object)$dispersion
      tmp <- predict(object, out, se.fit = TRUE, type = "link")
      se_terms <- tmp$se.fit
      t_quant <- qt(p = alpha / 2, df = object$df.residual, lower.tail = FALSE)
      se_global <- sqrt(sigma_sq + se_terms^2)
      out[[lower_pi_nm]] <- pred - t_quant * se_global
      out[[upper_pi_nm]] <- pred + t_quant * se_global
    } else {
      stop("Unsupported glm family type")
    }

    # corrections for extremes
    if (isTRUE(all.equal(alpha, 0))) {
      out[[lower_pi_nm]] <- ifelse(is.infinite(lower_ci), -Inf, out[[lower_pi_nm]])
      out[[upper_pi_nm]] <- ifelse(is.infinite(upper_ci), Inf, out[[upper_pi_nm]])
      if (inherits(object, "negbin")) {
        out[[lower_pi_nm]] <- ifelse(is.nan(out[[lower_pi_nm]]), 0, out[[lower_pi_nm]])
      }
    }

  }

  # optionally remove confidence intervals (leaves the estimate)
  if (!add_ci) out[ci_names] <- NULL

  # ensure result is tibble
  as_tibble(out)
}

# -------------------------------------------------------------------------

add_prediction_brmsfit <- function(object, new_data, name, alpha, add_ci,
                                   ci_names, add_pi, pi_names, ...) {

  # add confidence intervals
  ci <- fitted(object, new_data, probs = c(alpha / 2, 1 - alpha / 2))[,-2]
  ci <- as.data.frame(ci)
  ci <- setNames(ci, c(name, ci_names))
  out <- cbind(new_data, ci)

  # optionally remove confidence intervals (leaves the estimate)
  if (!add_ci) out[ci_names] <- NULL

  # optionally add prediction intervals using brms::predictive_interval
  if (add_pi) {
    pi <- brms::predictive_interval(object, newdata = new_data, prob = 1 - alpha)
    pi <- setNames(as.data.frame(pi)[, 1:2], pi_names)
    out <- cbind(out, pi)
  }

  # ensure result is tibble
  as_tibble(out)
}

# -------------------------------------------------------------------------

check_names <- function(new_data, name, add_ci, ci_names, add_pi, pi_names) {
  nms <- names(new_data)
  if (name %in% nms) {
    fmt <- c(
      'Please provide an alternative value for `name`:\n',
      '       column named "%s" already present in `new_data`.'
    )
    stop(sprintf(fmt, name), call. = FALSE)
  }

  ci_present <- ci_names[ci_names %in% nms]
  if (add_ci && length(ci_present)) {
    fmt <- c(
      'Please provide alternative value for `ci_names`:\n',
      '       columns named "%s", already present in `new_data`.'
    )
    msg <- sprintf(fmt, paste(ci_present, collapse = ", "))
    stop(msg, call. = FALSE)
  }

  pi_present <- pi_names[pi_names %in% nms]
  if (add_pi && length(pi_present)) {
    fmt <- c(
      'Please provide alternative value for `pi_names`:\n',
      '       columns named "%s", already present in `new_data`.'
    )
    msg <- sprintf(fmt, paste(pi_present, collapse = ", "))
    stop(msg, call. = FALSE)
  }
}

#' @export
print.trending_estimate <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export
format.trending_estimate <- function(x, ...) {
  header <- sprintf(
    "<trending_estimate> %s x %s",
    formatC(nrow(x), big.mark = ","),
    formatC(ncol(x), big.mark = ",")
  )
  header <- pillar::style_subtle(header)
  body <- format(tibble::as_tibble(x, ...))[-1]
  c(header, body)
}

