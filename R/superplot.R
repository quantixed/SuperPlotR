#' Make a SuperPlot
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas name of column with measurement (e.g. intensity or speed)
#' @param cond name of column with condition (e.g. Control, WT)
#' @param repl name of column with replicate (e.g. unique experiment ids)
#' @param pal name of colour palette to use (default is "tol_bright")
#' @param xlab string for x label (default is empty)
#' @param ylab string for y label (default is "Measurement")
#'
#' @return ggplot object
#' @import ggplot2
#' @import dplyr
#' @import ggforce
#' @import cowplot
#' @importFrom stats sd
#'
#' @export
#'
#' @examples
#' superplot(lord_jcb, Speed, Treatment, Replicate, ylab = "Speed (um/min)")
#'
superplot <- function(df,
                      meas, cond, repl,
                      pal = "tol_bright",
                      xlab = "", ylab = "Measurement") {

  ncond <- nrepl <- Mean <- SD <- NULL

  # if the repl column is not character, convert it
  if (!is.character(df[[deparse(substitute(repl))]])) {
    df[[deparse(substitute(repl))]] <-
      as.character(df[[deparse(substitute(repl))]])
  }

  # how many unique values in cond and repl?
  ncond <- length(unique(df[[deparse(substitute(cond))]]))
  nrepl <- length(unique(df[[deparse(substitute(repl))]]))

  # calculate summary statistics
  summary_df <- df %>%
    group_by({{cond}}, {{repl}}) %>%
    summarise(Mean = mean({{meas}}, na.rm = TRUE),
              SD = sd({{meas}}, na.rm = TRUE))
  # generate a warning is NROW of summary_df doesn't equal the product of
  # unique values in cond and repl
  if (nrow(summary_df) != ncond * nrepl) {
    warning("Summary statistics were not calculated for all combinations of
            condition and replicate.")
  }
  # get colour values for the repl column
  sp_colours <- get_sp_colours(nrepl, pal)

  # make superplot
  p <- ggplot() +
    geom_sina(data = df,
              aes(x = {{cond}}, y = {{meas}}, colour = {{repl}}),
              shape = 16, alpha = 0.5, position = "auto",
              size = 0.8, maxwidth = 0.3) +
    geom_point(data = summary_df,
               aes(x = {{cond}}, y = Mean, fill = {{repl}}),
               shape = 22, size = 1.5, stroke = 0.5, alpha = 0.7) +
    scale_color_manual(values = sp_colours) +
    scale_fill_manual(values = sp_colours) +
    labs(x = xlab, y = ylab) +
    lims(y = c(0, NA)) +
    theme_cowplot(9) +
    theme(legend.position = "none")

  return(p)
}
