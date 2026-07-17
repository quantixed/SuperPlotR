#' Make a SuperPlot
#'
#' @description
#' The function \code{superplot()} creates a SuperPlot using \code{ggplot2}.
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#' @param repl character name of column with replicate (e.g. unique experiment
#'   identifiers)
#' @param facet character name of column to facet by (default is NULL)
#' @param pal name of colour palette to use (default is "tol_bright")
#' @param xlab string for x label (default is empty)
#' @param ylab string for y label (default is "Measurement")
#' @param datadist string for data distribution to use, select ("sina" default,
#'   "jitter", or "violin")
#' @param size numeric vector of size range data and summary points (default is
#'   c(2, 3))
#' @param alpha numeric vector of alpha range data and summary points (default
#'   is c(0.5, 0.7))
#' @param bars string for type of error bars to add, select "mean_sd" (default),
#'   "mean_sem", or "mean_ci"; for no bars use an empty string (""); for no
#'   error bars but still show the mean with a crossbar, use "none".
#' @param linking logical for whether to link summary points between conditions
#'   (default is FALSE)
#' @param fsize numeric font size for text (default is 12)
#' @param shapes logical for whether to use different shapes for replicates
#' @param rep_summary string for summary statistic to use for replicates, select
#'   ("rep_mean" default, or "rep_median")
#' @param gg ggplot object to add to (default is NULL)
#' @param stats logical for whether to add statistical tests (default is FALSE)
#' @param stats_test string for statistical test to use, select
#'   ("para_unpaired", "para_paired", "nonpara_unpaired", or "nonpara_paired")
#' @param info logical for whether to print information about the plot (default
#'   is FALSE)
#' @param options optional named list for advanced layer customization. See
#'   `superplot_spec()` and `sp_modify()` for available options.
#'
#' @return ggplot object
#' @import ggplot2
#' @import dplyr
#' @import ggforce
#' @import cowplot
#' @importFrom stats sd median as.formula
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
                      facet = NULL,
                      pal = "tol_bright",
                      xlab = "", ylab = "Measurement",
                      datadist = "sina",
                      size = c(2, 3),
                      alpha = c(0.5, 0.7),
                      bars = "mean_sd",
                      linking = FALSE,
                      rep_summary = "rep_mean",
                      shapes = FALSE,
                      fsize = 12,
                      gg = NULL,
                      stats = FALSE,
                      stats_test = "para_unpaired",
                      info = FALSE,
                      options = NULL) {
  spec <- superplot_spec(
    df = df,
    meas = meas,
    cond = cond,
    repl = repl,
    facet = facet,
    pal = pal,
    xlab = xlab,
    ylab = ylab,
    datadist = datadist,
    size = size,
    alpha = alpha,
    bars = bars,
    linking = linking,
    rep_summary = rep_summary,
    shapes = shapes,
    fsize = fsize,
    gg = gg,
    stats = stats,
    stats_test = stats_test,
    info = info,
    options = options
  )

  autoplot(spec)
}
