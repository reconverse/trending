#' Plot trending prediction intervals
#'
#' Temporary plotting function for experimenting.
#'
#' @param x A `trending_model_prediction` object.
#' @param date_axis Variable in x representing the date axis.
#' @param predicted_y Variable in x representing the predicted counts.
#' @param lower_y Variable in x representing the lower prediction interval.
#' @param upper_y Variable in x representing the upper prediction interval.
#' @param fitted_data The data used to fit the plot (optional).
#' @param fitted_y The count variable of the fitting data.
#' @param ... Not currently used.
#'
#' @export
plot.trending_model_prediction <- function(x, date_axis, predicted_y = "pred",
                                           lower_y = "lower", upper_y = "upper",
                                           fitted_data = NULL, fitted_y = NULL,
                                           ...) {

    g <-
      ggplot2::ggplot(x, ggplot2::aes(x = .data[[date_axis]])) +
      ggplot2::theme_bw() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_y]],
                                        ymax = .data[[upper_y]]),
                           fill = "#BBB67E",
                           alpha = 0.4) +
      ggplot2::geom_point(ggplot2::aes(y = .data[[predicted_y]])) +
      ggplot2::xlab("")

    if (!is.null(fitted_data)) {
      last_date <- as.integer(max(fitted_data[[date_axis]]))
      first_date <- as.integer(min(x[[date_axis]]))
      x <- dplyr::bind_rows(fitted_data, x)
      g <-
        g +
        ggplot2::geom_vline(xintercept = (first_date + last_date) / 2,
                            linetype = 2) +
        ggplot2::geom_col(ggplot2::aes(y = .data$count),
                          data = fitted_data,
                          fill = "#0077BB",
                          alpha = 0.7,
                          color = "white")
    }

    g
}
