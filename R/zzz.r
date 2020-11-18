.onLoad <- function(...) {
  vctrs::s3_register(
      "dplyr::dplyr_reconstruct",
      "trending",
      method = dplyr_reconstruct_trending_model_fit_list
  )
}
