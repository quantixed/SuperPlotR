#' Make a pie chart using ggplot2
#'
#' @description
#' The function \code{pie_maker} creates a pie chart using \code{ggplot2}.
#' It takes two vectors of values and optionally a
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
#' pieplot(x1 = c(123, 456),
#'   cols = c("#44aa99", "#117733"))
#'
#' pieplot(x1 = c(50 - 20, 20, 80, 180 - 80),
#'   cols = c("#bbbbbb", "#44aa99", "#117733", "#dddddd"), x2 = c(100,130))
#'
#' @return ggplot object
#' @import ggplot2
#'
#' @export


pieplot <- function(x1, cols, x2 = NULL, label = NULL, ...) {
  pcolours <- ymin <- ymax <- NULL
  # validate args
  if (!is.numeric(x1)) {
    stop("x1 must be a numeric vector")
  }
  if (!is.null(x2) && !is.numeric(x2)) {
    stop("x2 must be a numeric vector or NULL")
  }
  # check that x1 and optionally x2 are non-negative
  if (any(x1 < 0)) {
    stop("x1 must be a non-negative numeric vector")
  }
  if (!is.null(x2) && any(x2 < 0)) {
    stop("x2 must be a non-negative numeric vector or NULL")
  }
  if (length(x1) == 0) {
    stop("x1 must be a non-empty numeric vector")
  }
  if (!is.null(x2) && length(x2) == 0) {
    stop("x2 must be a non-empty numeric vector or NULL")
  }
  # colour check
  check_colour(cols)
  if (!is.character(cols) || length(cols) != length(x1)) {
    stop("cols must be a character vector of the same length as x1")
  }
  cols <- get_fp_colours(cols)

  df <- data.frame(
    pvalues = x1,
    pcolours = as.factor(seq_along(cols))
  )
  cs1 <- cumsum(x1)
  df$ymin <- c(0, cs1[seq_along(cs1) - 1])
  df$ymax <- cs1
  if (is.null(x2)) {
    # if x2 is NULL, we only have one layer
    # so df2 makes the outline over the pie chart
    df2 <- df
  } else {
    x2 <- x2 / sum(x2) * sum(x1)  # scale x2 to match x1
    cs2 <- cumsum(x2)
    df2 <- data.frame(
      pvalues = x2,
      ymin = c(0, cs2[seq_along(cs2) - 1]),
      ymax = cs2
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
