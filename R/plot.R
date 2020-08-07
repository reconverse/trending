#' Plot
#'
#' Temporary plotting function for experimenting.
#'
#' @param x data.frame.
#' @param date_axis Variable in x representing the date axis.
#' @param known_y Variable in x representing the known counts.
#' @param predicted_y Variable in x representing the predicted counts.
#' @param lower_y Variable in x representing the lower prediction interval.
#' @param upper_y Variable in x representing the upper prediction interval.
#' @param facets Variables to facet by.
#' @param ... Not currently used.
#'
#' @export
plotplot <- function(x, date_axis, known_y, predicted_y = "pred",
                     lower_y = "lower", upper_y = "upper", facets = NULL, ...) {

  known_dat <- x[is.na(x[[predicted_y]]), ]
  pred_dat <- x[!is.na(x[[predicted_y]]), ]

  last_date <- as.integer(max(known_dat[[date_axis]]))
  first_date <- as.integer(min(pred_dat[[date_axis]]))

  (first_date - last_date) / 2

  g <-  ggplot2::ggplot(x, ggplot2::aes(x = .data[[date_axis]])) +
    ggplot2::theme_bw() +
    ggplot2::geom_col(ggplot2::aes(y = .data[[known_y]]),
                      fill = "#0077BB",
                      alpha = 0.7,
                      color = "white") +
    ggplot2::geom_ribbon(ggplot2::aes(x = .data[[date_axis]],
                                      ymin = .data[[lower_y]],
                                      ymax = .data[[upper_y]]),
                         fill = "#BBB67E",
                         alpha = 0.4) +
    ggplot2::geom_point(ggplot2::aes(x = .data[[date_axis]],
                                     y = .data[[predicted_y]])) +
    ggplot2::geom_vline(xintercept = (first_date + last_date) / 2, linetype = 2) +
    ggplot2::xlab("")

  if (!is.null(facets)) {
    g + ggplot2::facet_wrap(ggplot2::vars(!!!syms(facets)))
    } else {
      g
    }
}
