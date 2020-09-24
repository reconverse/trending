check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Suggested package '%s' not present.", package)
    stop(msg, call. = FALSE)
  }
}

combine_safe_results <- function(x) {
  out <- vector("list", length(x[[1]]))
  ok_index <- purrr::map_lgl(x[[1]], ~!is.null(.x))
  out[ok_index] <- x[[1]][ok_index]
  out[!ok_index] <- x[[2]][!ok_index]
  out
}