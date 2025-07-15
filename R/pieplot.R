#' Make a pie chart using ggplot2
#'
#' @description
#' The function \code{pie_maker} creates a pie chart using
#' \code{ggplot2}. It takes two vectors of values and optionally a
#' second vector of values to create a pie chart with two layers.
#' @param x1 A numeric vector containing the values for the first
#'     layer of the pie chart.
#' @param cols A character vector containing the colors for the
#'     slices of the pie chart.
#' @param x2 An optional numeric vector containing the values for the
#'     second layer of the pie chart.
#' @param label An optional character string to be used as the title
#' @param ... Currently not used.
#' @examples
#' pieplot(x1 = c(143, 459),
#'   cols = c("#44aa99", "#117733"))
#'
#' pieplot(x1 = c(61 - 9, 9, 593, 3595 - 593),
#'   cols = c("#bbbbbb", "#44aa99", "#117733", "#dddddd"),
#'   x2 = c(61, 3595))
#'
#' @return ggplot object
#' @import ggplot2
#'
#' @export


pieplot <- function(x1, cols, x2 = NULL, label = NULL, ...) {
  pcolours <- ymin <- ymax <- NULL
  df <- data.frame(
    pvalues = x1,
    pcolours = as.factor(seq_along(cols))
  )
  cs <- cumsum(x1)
  df$ymin <- c(0, cs[seq_along(cs) - 1])
  df$ymax <- cs
  if (is.null(x2)) {
    df2 <- df
  } else {
    cs <- cumsum(x2)
    df2 <- data.frame(
      pvalues = x2,
      ymin = c(0, cs[seq_along(cs) - 1]),
      ymax = cs
    )
  }

  p <- ggplot() +
    geom_rect(data = df, aes(fill = pcolours,
                             ymin = ymin, ymax = ymax, xmin = 0, xmax = 3)) +
    scale_fill_manual(values = cols) +
    geom_rect(data = df2, aes(ymin = ymin, ymax = ymax,
                              xmin = 0, xmax = 3),
              alpha = 0, colour = "black") +
    xlim(c(0, 3)) +
    theme_minimal() +
    theme(aspect.ratio = 1) +
    coord_polar(theta = "y") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
  if (!is.null(label)) {
    p <- p + labs(title = label)
  }

  return(p)
}
