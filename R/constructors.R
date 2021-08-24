new_trending_fit <- function(x) {
  structure(x, class = c("trending_fit", class(x)))
}

# -------------------------------------------------------------------------

new_trending_fit_tbl <- function(
    x,
    model_name = NULL,
    result = "result",
    warnings = "warnings",
    errors = "errors"
) {
  new_tibble(
    x,
    model_name = model_name,
    result = result,
    warnings = warnings,
    errors = errors,
    nrow = nrow(x),
    class = "trending_fit_tbl"
  )
}

# -------------------------------------------------------------------------

new_trending_predict <- function(x) {
  structure(x, class = c("trending_predict", class(x)))
}

# -------------------------------------------------------------------------

new_trending_predict_tbl <- function(
  x,
  model_name = NULL,
  result = "result",
  warnings = "warnings",
  errors = "errors"
) {
  stopifnot(
    is.data.frame(x),
    is.null(model_name) || is.character(model_name),
    is.character(result),
    is.character(warnings),
    is.character(errors)
  )
  new_tibble(
    x,
    model_name = model_name,
    result = result,
    warnings = warnings,
    errors = errors,
    nrow = nrow(x),
    class = "trending_predict_tbl"
  )
}

# -------------------------------------------------------------------------
