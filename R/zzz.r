# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_fit",
    method = dplyr_reconstruct_trending_fit
  )
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_prediction",
    method = dplyr_reconstruct_trending_prediction
  )
  invisible()
}
# nocov end
