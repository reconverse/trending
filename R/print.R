format.trending_model <- function(x, ...) {
  paste0("Untrained trending model type: ", x[["model_class"]])
}


print.trending_model <- function(x, ...) {
  cat(format(x))
}


format.trending_model_fit <- function(x, ...) {
  tmp <- append(
    "Fitted trending model:",
    utils::capture.output(x$fitted_model)
  )
  paste(tmp, collapse = "\n")
}


print.trending_model_fit <- function(x, ...) {
  cat(format(x))
}