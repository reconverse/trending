#' Plot trending prediction intervals
#'
#' Temporary plotting function for experimenting.
#'
#' @param x A `trending_model_prediction` object.
#' @param date_axis Variable in x representing the date axis.
#' @param counts Variable in x representing the counts (predicted or estimated).
#' @param plot_ci Should confidence intervals be plotted (`TRUE`/`FALSE`)
#' @param plot_pi Should prediction intervals be plotted (`TRUE`/`FALSE`)
#' @param lower_ci Variable in x representing the lower confidence interval.
#' @param upper_ci Variable in x representing the upper confidence interval.
#' @param lower_pi Variable in x representing the lower prediction interval.
#' @param upper_pi Variable in x representing the upper prediction interval.
#' @param fitted_data The data used to fit the plot (optional).
#' @param fitted_y The count variable of the fitting data.
#' @param ... Not currently used.
#'
#' @export
plot.trending_model_prediction <- function(x, date_axis, counts = "pred",
                                           plot_ci = TRUE,
                                           plot_pi = TRUE,
                                           lower_ci = "lower-ci",
                                           upper_ci = "upper-ci",
                                           lower_pi = "lower-pi",
                                           upper_pi = "upper-pi",
                                           fitted_data = NULL, 
                                           fitted_y = NULL,
                                           ...) {

    g <-
      ggplot2::ggplot(x, ggplot2::aes(x = .data[[date_axis]])) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(ggplot2::aes(y = .data[[counts]])) +
      ggplot2::xlab("") +
      ggplot2::ylab("")

    if (plot_ci) {
      g <-
        g +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_ci]],
                                          ymax = .data[[upper_ci]]),
                            alpha = 0.2)
    }

    if (plot_pi) {
      g <-
        g +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_pi]],
                                        ymax = .data[[upper_pi]]),
                           fill = "#BBB67E",
                           alpha = 0.4)
    }

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
