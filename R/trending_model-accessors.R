#' Accessors for trending_model objects
#'
#' These functions can be used to access information stored in `trending_model`
#' objects. See details.
#'
#' @details The following accessors are available:
#'
#' * `get_formula()`: get the formula used to model temporal trends
#'
#' * `get_response()`: get the name of the response variable
#'
#' @author Thibaut Jombart, Dirk Schumacher
#'
#' @param x the output of functions `lm_model`, `glm_model`, `glm_nb_model`, or
#'   brms_model
#'
#' @param ... further arguments passed to other methods
#'
#' @aliases trending_model-accessors trending_model-class
#'
#' @export
#' @rdname trending_model-accessors
#' @aliases get_formula.trending_model
get_formula.trending_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  as.list(environment(x$fit))$formula
}


#' @export
#' @rdname trending_model-accessors
#' @aliases get_response.trending_model
get_response.trending_model <- function(x, ...) {
  ellipsis::check_dots_empty()
  form <- get_formula(x)
  as.character(form)[2]
}

<<<<<<< HEAD

=======
>>>>>>> ciTools
