#' Information about a SuperPlot
#'
#' This function prints information about a SuperPlot, it runs internally from
#' superplot()
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#' @param repl character name of column with replicate (e.g. unique experiment
#' identifiers)
#' @param pal argument passed to pal
#' @param xlab argument passed to xlab
#' @param ylab argument passed to ylab
#' @param datadist argument passed to datadist
#' @param size argument passed to size
#' @param alpha argument passed to alpha
#' @param bars argument passed to bars
#' @param linking argument passed to linking
#' @param rep_summary argument passed to rep_summary
#' @param shapes argument passed to shapes
#' @param fsize argument passed to fsize
#' @param gg argument passed to gg
#' @param stats argument passed to stats
#' @param stats_test argument passed to stats_test
#' @param ... additional arguments
#'
#' @import dplyr
#' @importFrom stats sd median
#' @importFrom rlang :=
#'
#' @returns none
#' @keywords internal

# this function will take all the arguments sent from superplot
get_sp_info <- function(df,
                      meas, cond, repl,
                      pal, xlab, ylab, datadist, size, alpha, bars,
                      linking, rep_summary, shapes, fsize, gg,
                      stats, stats_test,
                      ...) {
  ncond <- nrepl <- NULL
  rep_mean <- rep_median <- NULL

  # args are already validated

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

  # get colour values for the repl column
  sp_colours <- get_sp_colours(nrepl, pal)
  sp_shapes <- get_sp_shapes(nrepl, shapes)

  # add repl from summary_df to sp_colour and sp_shapes
  # repl is converted to factor so that the colours and shapes
  # are assigned in the order of the factor levels
  # but only if repl is not already a factor
  if (!is.factor(df[[repl]])) {
    summary_df <- summary_df %>%
      mutate(!!sym(repl) := factor(!!sym(repl), levels = unique(df[[repl]])))
  }
  summary_df <- summary_df %>%
    mutate(
      sp_colour = factor(!!sym(repl), levels = unique(df[[repl]]),
                         labels = sp_colours),
      sp_shape = factor(!!sym(repl), levels = unique(df[[repl]]),
                        labels = sp_shapes)
    )

  ## print information about the plot
  message("SuperPlot information")
  message("=====================")
  message("Number of conditions: ", ncond)
  message("Number of replicates: ", nrepl)
  message("Number of data points: ", nrow(df))
  message("Number of summary points: ", nrow(summary_df))
  message("=====================")
  message("Colour palette: ", pal)
  message("Data distribution: ", datadist)
  message("Summary statistic: ", rep_summary)
  if( bars != "") {
    message("Bars: ", bars)
  } else {
    message("No bars")
  }
  message("X-axis label: ", xlab)
  message("Y-axis label: ", ylab)
  if (linking == TRUE) {
    message("Linking: ", linking)
  }
  if (shapes == TRUE) {
    message("Shapes: ", shapes)
  }
  message("Point sizes: ", size[1], " (individual), ",
          size[2], " (summary)")
  message("Alpha for points: ", alpha[1], " (individual), ",
          alpha[2], " (summary)")
  message("Font size: ", fsize)
  if (stats== TRUE) {
    message("Statistics: ", stats_test)
  } else {
    message("No statistics")
  }
  message("=====================")
  message("Colours for replicates: ", paste(sp_colours, collapse = ", "))
  message("Shapes for replicates: ", paste(sp_shapes, collapse = ", "))

  if (nrow(summary_df) != ncond * nrepl) {
    s_summary_df <- summary_df %>%
      group_by(!!sym(cond)) %>%
      summarise(
        n_replicates = n()
      )
    message("Unequal number of replicates per condition:")

    print(s_summary_df)
  }

  message("=====================")
  message("Summary statistics:")
  print(summary_df)
}
