check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Suggested package '%s' not present.", package)
    stop(msg, call. = FALSE)
  }
}

combine_safe_results <- function(x) {
  out <- vector("list", length(x[[1]]))
  ok_index <- vapply(x[[1]], function(x) !is.null(x), logical(1))
  out[ok_index] <- x[[1]][ok_index]

  error_index <- vapply(x[[2]], function(x) !is.null(x), logical(1))
  out[error_index] <- x[[2]][error_index]

  out[!(ok_index | error_index)] <- x[[3]][!(ok_index | error_index)]
  out
}

base_transpose <- function(l) {
  lapply(seq_along(l[[1]]), function(x) lapply(l, "[[", x))
}
