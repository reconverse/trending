# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_estimate",
    method = dplyr_reconstruct_trending_estimate
  )
  invisible()
}
# nocov end
