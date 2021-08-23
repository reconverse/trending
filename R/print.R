#' @export
format.trending_fit_tbl <- function(x, ...) {
  header <- sprintf(
    "<trending_fit_tbl> %s x %s",
    formatC(nrow(x), big.mark = ","),
    formatC(ncol(x), big.mark = ",")
  )
  header <- pillar::style_subtle(header)
  body <- format(tibble::as_tibble(x, ...))[-1]
  c(header, body)
}

# -------------------------------------------------------------------------

#' @export
print.trending_fit_tbl <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

# -------------------------------------------------------------------------

#' @export
format.trending_predict_tbl <- function(x, ...) {
  header <- sprintf(
    "<trending_predict_tbl> %s x %s",
    formatC(nrow(x), big.mark = ","),
    formatC(ncol(x), big.mark = ",")
  )
  header <- pillar::style_subtle(header)
  body <- format(tibble::as_tibble(x, ...))[-1]
  c(header, body)
}

# -------------------------------------------------------------------------

#' @export
print.trending_predict_tbl <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

# -------------------------------------------------------------------------

#' @export
print.trending_prediction <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

# -------------------------------------------------------------------------

#' @export
format.trending_prediction <- function(x, ...) {
  header <- sprintf(
    "<trending_prediction> %s x %s",
    formatC(nrow(x), big.mark = ","),
    formatC(ncol(x), big.mark = ",")
  )
  header <- pillar::style_subtle(header)
  body <- format(tibble::as_tibble(x, ...))[-1]
  c(header, body)
}
