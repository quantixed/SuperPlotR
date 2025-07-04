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
#' @param datadist string for data distribution to use, select ("sina" default,
#' "jitter", or "violin")
#' @param size numeric vector of size range data and summary points (default is
#' c(2, 3))
#' @param alpha numeric vector of alpha range data and summary points (default
#' is c(0.5, 0.7))
#' @param bars string for type of error bars to add, select ("none" default,
#' "mean_sd", "mean_sem", or "mean_ci")
#' @param linking logical for whether to link summary points between conditions
#' (default is FALSE)
#' @param fsize numeric font size for text (default is 12)
#' @param shapes logical for whether to use different shapes for replicates
#' @param rep_summary string for summary statistic to use for replicates, select
#' ("rep_mean" default, or "rep_median")
#' @param gg ggplot object to add to (default is NULL)
#' @param stats logical for whether to add statistical tests (default is FALSE)
#' @param stats_test string for statistical test to use, select
#' ("para_unpaired", "para_paired", "nonpara_unpaired", or "nonpara_paired")
#' @param info logical for whether to print information about the plot (default
#' is FALSE)
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
#'   "Speed", "Treatment", "Replicate",
#'   ylab = "Speed (um/min)")
#'
superplot <- function(df,
                      meas, cond, repl,
                      pal = "tol_bright",
                      xlab = "", ylab = "Measurement",
                      datadist = "sina",
                      size = c(2, 3),
                      alpha = c(0.5, 0.7),
                      bars = "",
                      linking = FALSE,
                      rep_summary = "rep_mean",
                      shapes = FALSE,
                      fsize = 12,
                      gg = NULL,
                      stats = FALSE,
                      stats_test = "para_unpaired",
                      info = FALSE) {
  ncond <- nrepl <- NULL
  rep_mean <- rep_median <- NULL

  # validate args
  validate_args(pal = pal, xlab = xlab, ylab = ylab, datadist = datadist,
                size = size, alpha = alpha, bars = bars, linking = linking,
                rep_summary = rep_summary, shapes = shapes, fsize = fsize,
                gg = gg, stats = stats, stats_test = stats_test, info = info)

  # verify that the data frame to make sure that it is suitable for SuperPlot
  if (verify_sp_columns(df, meas, cond, repl) == FALSE) {
    return(NULL)
  }

  # if the cond column is not character, convert it
  # but only if it is not already a factor
  if (!is.factor(df[[cond]]) && !is.character(df[[cond]])) {
    df[[cond]] <- as.character(df[[cond]])
  }

  # if the repl column is not character, convert it
  # but only if it is not already a factor
  if (!is.character(df[[repl]]) && !is.factor(df[[repl]])) {
    df[[repl]] <- as.character(df[[repl]])
  }

  # how many unique values in cond and repl?
  ncond <- df %>%
    pull(!!sym(cond)) %>%
    unique() %>%
    length()
  nrepl <- df %>%
    pull(!!sym(repl)) %>%
    unique() %>%
    length()

  # calculate summary statistics
  summary_df <- get_sp_summary(df = df,
                               meas = meas, cond = cond, repl = repl)

  # generate a warning if NROW of summary_df doesn't equal the product of
  # unique values in cond and repl
  if (nrow(summary_df) != ncond * nrepl && info == FALSE) {
    warning("Summary statistics were not calculated for all combinations of
            condition and replicate.\nCheck for missing data.\n
            Call again with `info = TRUE` to see more details.")
  }
  # get colour values for the repl column
  sp_colours <- get_sp_colours(nrepl, pal)
  sp_shapes <- get_sp_shapes(nrepl, shapes)

  # if info is TRUE, print information about the plot
  if (info == TRUE) {
    get_sp_info(df = df,
                meas = meas, cond = cond, repl = repl,
                pal = pal, xlab = xlab, ylab = ylab,
                datadist = datadist, size = size, alpha = alpha,
                bars = bars, linking = linking,
                rep_summary = rep_summary, shapes = shapes,
                fsize = fsize, gg = gg,
                stats = stats, stats_test = stats_test
    )
  }

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
        aes(x = !!sym(cond), y = !!sym(meas),
            colour = !!sym(repl), fill = !!sym(repl), shape = !!sym(repl)),
        alpha = alpha[1], stroke = 0, jitter_y = FALSE, position = "auto",
        size = size[1], maxwidth = 0.3
      )
  } else if (datadist == "jitter") {
    p <- p +
      geom_jitter(
        data = df,
        aes(x = !!sym(cond), y = !!sym(meas),
            colour = !!sym(repl), fill = !!sym(repl), shape = !!sym(repl)),
        alpha = alpha[1], stroke = 0,
        size = size[1]
      )
  } else if (datadist == "violin") {
    p <- p +
      geom_violin(
        data = df,
        aes(x = !!sym(cond), y = !!sym(meas), group = !!sym(cond)),
        fill = "grey", width = 0.5, alpha = alpha[1]
      )
  } else {
    warning("datadist must be one of 'sina', 'jitter', or 'violin'")
  }
  if (linking == TRUE) {
    p <- p + geom_path(
      data = summary_df,
      aes(x = !!sym(cond), y = !!sym(rep_summary), group = !!sym(repl)),
      linetype = "dashed", alpha = 0.5
    )
  }
  # add mean and error bars here if requested
  if (bars != "") {
    p <- add_sp_bars(p, bars, summary_df, cond, rep_summary)
  }
  # add summary points here
  p <- p + geom_point(
    data = summary_df,
    aes(x = !!sym(cond), y = !!sym(rep_summary),
        fill = !!sym(repl), shape = !!sym(repl)),
    size = size[2], stroke = 0.5, alpha = alpha[2]
  )
  # colours, shapes, and labels
  p <- p + scale_color_manual(values = sp_colours) +
    scale_shape_manual(values = sp_shapes) +
    scale_fill_manual(values = sp_colours) +
    labs(x = xlab, y = ylab)
  # limits
  if (min(df[[meas]], na.rm = TRUE) > 0) {
    p <- p + lims(y = c(0, NA))
  } else {
    # plot is scaled automatically
  }
  # theme
  p <- p + theme_cowplot(fsize)
  # info is FALSE hide legend
  if (info == TRUE) {
    p <- p + theme(legend.position = "right")
  }
  p <- p + theme(legend.position = "none")

  # add stats if requested
  if (stats == TRUE) {
    get_sp_stats(as.data.frame(summary_df), rep_summary, cond, repl, ncond, nrepl, stats_test)
  }

  return(p)
}
