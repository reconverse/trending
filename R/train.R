#' Train a model
#'
#' Train a model using data to obtain a
#' [`trending_model_fit`](trending_model_fit) object.
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model` or
#'   `brms_model`.
#' @param data a `data.frame` to be used to train the model.
#' @param ... Not currently used.
#' @export
#' @aliases train.trending_model
train.trending_model <- function(x, data, ...) {
  ellipsis::check_dots_empty()
  x$train(data)
}
