#' Make a FlatPlot
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#' @param colour string for colour palette to use, select ("rl_green", "rl_red",
#' "rl_blue", "rl_purple", "rl_orange", "rl_magenta", or a hex colour, default
#' is black)
#' @param xlab string for x label (default is empty)
#' @param ylab string for y label (default is "Measurement")
#' @param datadist string for data distribution to use, select ("sina" default,
#' or "jitter")
#' @param size numeric size of data points (default is 2)
#' @param alpha numeric vector of alpha range data and summary points (default
#' is c(0.5, 0.7))
#' @param bars string for type of error bars to add, select ("none",
#' "mean_sd" (default), "mean_sem", or "mean_ci")
#' @param fsize numeric font size for text (default is 12)
#' @param gg ggplot object to add to (default is NULL)
#' @param stats logical for whether to add statistical tests (default is FALSE)
#' @param stats_test string for statistical test to use, select
#' ("para_unpaired", "para_paired", "nonpara_unpaired", or "nonpara_paired")
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
#' flatplot(lord_jcb, "Speed", "Treatment", ylab = "Speed (um/min)")
#'
flatplot <- function(df,
                      meas, cond,
                      colour = "#000000",
                      xlab = "", ylab = "Measurement",
                      datadist = "sina",
                      size = 2,
                      alpha = 0.5,
                      bars = "mean_sd",
                      fsize = 12,
                      gg = NULL,
                      stats = FALSE,
                      stats_test = "para_unpaired") {
  ncond <- nrepl <- NULL
  rep_mean <- rep_median <- NULL

  # validate args
  validate_args(colour = colour, xlab = xlab, ylab = ylab, datadist = datadist,
                bars = bars, fsize = fsize,
                gg = gg, stats = stats, stats_test = stats_test)
  # size and alpha should be a single numeric value
  if (!is.numeric(size) || length(size) != 1) {
    stop("size must be a single numeric value")
  }
  if (!is.numeric(alpha) || length(alpha) != 1) {
    stop("alpha must be a single numeric value")
  }

  # verify that the data frame to make sure that it is suitable for SuperPlot
  if (verify_fp_columns(df, meas, cond) == FALSE) {
    return(NULL)
  }

  # if the cond column is not character, convert it
  if (!is.character(df[[cond]])) {
    df[[cond]] <- as.character(df[[cond]])
  }

  fp_colour <- get_fp_colour(colour)

  # how many unique values in cond?
  ncond <- df %>%
    pull(!!sym(cond)) %>%
    unique() %>%
    length()

  # make superplot ----
  # we may have an existing ggplot object to add to
  if (is.null(gg)) {
    p <- ggplot()
  } else {
    p <- gg
  }

  # data points get plotted here
  if (datadist == "sina") {
    p <- p +
      geom_sina(
        data = df,
        aes(x = !!sym(cond), y = !!sym(meas)),
        colour = fp_colour,
        alpha = alpha, shape = 16, jitter_y = FALSE,
        size = size, maxwidth = 0.8
      )
  } else if (datadist == "jitter") {
    p <- p +
      geom_jitter(
        data = df,
        aes(x = !!sym(cond), y = !!sym(meas)),
        colour = fp_colour,
        alpha = alpha, shape = 16,
        size = size
      )
  } else {
    warning("datadist must be one of 'sina' or 'jitter'")
  }
  # add mean and error bars here if requested
  if (bars != "") {
    p <- add_sp_bars(p, bars, df, cond, meas)
  }
  # colours, shapes, and labels
  p <- p + labs(x = xlab, y = ylab)
  # limits
  if (min(df[[meas]], na.rm = TRUE) > 0) {
    p <- p + lims(y = c(0, NA))
  } else {
    # plot is scaled automatically
  }
  # theme
  p <- p + theme_cowplot(fsize) +
    theme(legend.position = "none")
  # add stats if requested
  if (stats == TRUE) {
    nrepl <- df %>%
      group_by(!!sym(cond)) %>%
      summarise(n = n())
    nrepl <- min(nrepl$n, na.rm = TRUE)
    get_sp_stats(as.data.frame(df), meas, cond, repl = NULL,
                 ncond, nrepl, stats_test)
  }

  return(p)
}
