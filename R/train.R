#' Train a model
#'
#' Train a model using data to obtain a
#' [`trending_model_fit`](trending_model_fit) object.
#'
#' @param data a `data.frame` to be used to train the model
#' @export
#' @aliases train.trending_model
train.trending_model <- function(x, data, ...) {
  ellipsis::check_dots_empty()
  x$train(data)
}
