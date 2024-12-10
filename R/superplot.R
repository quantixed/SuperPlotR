#' Make a SuperPlot
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#' @param repl character name of column with replicate (e.g. unique experiment
#' identifiers)
#' @param pal name of colour palette to use (default is "tol_bright")
#' @param xlab string for x label (default is empty)
#' @param ylab string for y label (default is "Measurement")
#' @param rep_summary string for summary statistic to use for replicates, select
#' ("rep_mean" or "rep_median")
#' @param gg ggplot object to add to (default is NULL)
#'
#' @return ggplot object
#' @import ggplot2
#' @import dplyr
#' @import ggforce
#' @import cowplot
#' @importFrom stats sd median
#'
#' @export
#'
#' @examples
#' superplot(lord_jcb,
#' "Speed", "Treatment", "Replicate", ylab = "Speed (um/min)")
#'
superplot <- function(df,
                      meas, cond, repl,
                      pal = "tol_bright",
                      xlab = "", ylab = "Measurement",
                      rep_summary = "rep_mean",
                      gg = NULL) {
  ncond <- nrepl <- NULL
  rep_mean <- rep_sd <- rep_sem <- rep_ci <- NULL

  # if the repl column is not character, convert it
  if (!is.character(df[[repl]])) {
    df[[repl]] <- as.character(df[[repl]])
  }

  # how many unique values in cond and repl?
  ncond <- df %>% pull(!!sym(cond)) %>% unique() %>% length()
  nrepl <- df %>% pull(!!sym(repl)) %>% unique() %>% length()

  # calculate summary statistics
  summary_df <- df %>%
    group_by(!!sym(cond), !!sym(repl)) %>%
    summarise(
      rep_mean = mean(!!sym(meas), na.rm = TRUE),
      rep_median = median(!!sym(meas), na.rm = TRUE),
      rep_sd = sd(!!sym(meas), na.rm = TRUE),
      rep_sem = sd(!!sym(meas), na.rm = TRUE) / sqrt(n()),
      rep_ci = 1.96 * sd(!!sym(meas), na.rm = TRUE) / sqrt(n())
    )
  # generate a warning if NROW of summary_df doesn't equal the product of
  # unique values in cond and repl
  if (nrow(summary_df) != ncond * nrepl) {
    warning("Summary statistics were not calculated for all combinations of
            condition and replicate.")
  }
  # get colour values for the repl column
  sp_colours <- get_sp_colours(nrepl, pal)

  # we may have an existing ggplot object to add to
  if (is.null(gg)) {
    p <- ggplot()
  } else {
    p <- gg
  }

  # make superplot
  p <- p +
    geom_sina(
      data = df,
      aes(x = !!sym(cond), y = !!sym(meas), colour = !!sym(repl)),
      shape = 16, alpha = 0.5, position = "auto",
      size = 0.8, maxwidth = 0.3
    )
  p <- p + geom_point(
      data = summary_df,
      aes(x = !!sym(cond), y = !!sym(rep_summary), fill = !!sym(repl)),
      shape = 22, size = 1.5, stroke = 0.5, alpha = 0.7
    )
  p <-  p + scale_color_manual(values = sp_colours) +
    scale_fill_manual(values = sp_colours) +
    labs(x = xlab, y = ylab) +
    lims(y = c(0, NA)) +
    theme_cowplot(9) +
    theme(legend.position = "none")

  return(p)
}
