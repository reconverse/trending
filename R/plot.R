#' @export
plot.fitted_and_predicted <- function(dat, x_axis, known_y, predicted_y = "pred",
                                 lower_y = "lower", upper_y = "upper") {
  known_dat <- dat[is.na(dat[[predicted_y]]), ]
  pred_dat <- dat[!is.na(dat[[predicted_y]]), ]

  last_date <- as.integer(max(known_dat[[x_axis]]))
  first_date <- as.integer(min(pred_dat[[x_axis]]))

  (first_date - last_date) / 2

  ggplot2::ggplot(dat, ggplot2::aes(x = .data[[x_axis]])) +
    ggplot2::theme_bw() +
    ggplot2::geom_col(ggplot2::aes(y = .data[[known_y]]),
                      fill = "#0077BB",
                      alpha = 0.7,
                      color = "white") +
    ggplot2::geom_ribbon(ggplot2::aes(x = .data[[x_axis]],
                                      ymin = .data[[lower_y]],
                                      ymax = .data[[upper_y]]),
                         fill = "#BBB67E",
                         alpha = 0.4) +
    ggplot2::geom_point(ggplot2::aes(x = .data[[x_axis]],
                                     y = .data[[predicted_y]])) +
    ggplot2::geom_vline(xintercept = (first_date + last_date) / 2, linetype = 2) +
    ggplot2::xlab("")

}
