# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_prediction",
    method = dplyr_reconstruct_trending_prediction
  )
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_fit_tbl",
    method = dplyr_reconstruct_trending_fit_tbl
  )
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "trending_predict_tbl",
    method = dplyr_reconstruct_trending_predict_tbl
  )
  invisible()
}
# nocov end
