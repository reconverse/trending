check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Suggested package '%s' not present.", package)
    stop(msg, call. = FALSE)
  }
}

# -------------------------------------------------------------------------

not_implemented <- function(x, call. = FALSE) {
  stop(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", ")),
    call. = call.
  )
}

# -------------------------------------------------------------------------

make_catcher <- function(fun) {
  function(...) {

    # create variables in environment to store output
    warn <- err <- NULL
    env <- environment()

    # define handlers
    warning_handler <- function(w) {
      assign("warn", c(warn, conditionMessage(w)), env, inherits = TRUE)
      invokeRestart("muffleWarning")
    }

    error_handler <- function(e) {
      assign("err", conditionMessage(e), env, inherits = TRUE)
      NULL
    }

    # capture output
    res <- withCallingHandlers(
      tryCatch(fun(...), error = error_handler),
      warning = warning_handler
    )

    list(result = res, warnings = warn, errors = err)
  }
}
