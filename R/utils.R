glustop <- function(..., .sep = "", .envir = parent.frame()) {
  stop(glue::glue(..., .sep = .sep, .envir = .envir), call. = FALSE)
}

check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    glustop("Suggested package '{package}' not present.")
  }
}
