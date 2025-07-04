#' Report the representative datapoints in a SuperPlot
#'
#' This function finds the representative datapoints for each condition and
#' replicate in a SuperPlot dataset. It calculates the mean or median of the
#' replicates (conditions), then ranks the individual measurements by their difference from
#' the summary statistic. The function returns a data frame with the
#' representative datapoints for each treatment and replicate, along with their
#' rank of difference from the summary statistic.
#'
#' The top ranked datapoint for each treatment and replicate is printed to the
#' console. This is useful for identifying the most representative datapoint
#' for each condition, which can be used for showing in a figure.
#'
#' A label column can be specified to identify the datapoint (this could be a
#' file name or other identifier). If no label is specified, the row number is
#' used instead.
#'
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#' @param repl character name of column with replicate (e.g. unique experiment
#' identifiers)
#' @param label character name of column with labels for measurements (e.g. file
#' names), optional.
#' @param bars string to specify the summary stats of replicate sumaries, select
#' ("none" default, "mean_sd", "mean_sem", or "mean_ci"). Sepcifying this
#' overrides the `rep_summary` argument
#' @param rep_summary string for summary statistic to use for replicates, select
#' ("rep_mean" default, or "rep_median")
#' @param outlier logical, if TRUE then ranking is reversed to find the most
#' extreme datapoint in each treatment and replicate, default is FALSE.
#'
#' @return data frame with the representative datapoints for each treatment and
#' replicate, with the rank of the difference from the summary statistic.
#' @import dplyr
#' @importFrom stats sd median
#'
#' @export
#'
#' @examples
#' representative(lord_jcb,
#'   "Speed", "Treatment", "Replicate")
#'
representative <- function(df,
                           meas, cond, repl,
                           label = "",
                           bars = "",
                           rep_summary = "rep_mean",
                           outlier = FALSE) {
  ncond <- nrepl <- NULL
  rep_mean <- rep_median <- NULL
  rowno <- NULL

  # validate args
  validate_args(bars = bars, rep_summary = rep_summary)

  # verify that the data frame to make sure that it is suitable for SuperPlot
  if (verify_sp_columns(df, meas, cond, repl) == FALSE) {
    return(NULL)
  }

  # verify that label is a character vector if it is not empty
  if (label != "" && !is.character(df[[label]])) {
    stop("The label column must be a valid character column in the data frame.")
  }

  # if the cond column is not character, convert it
  # but only if it is not already a factor
  if (!is.factor(df[[cond]]) && !is.character(df[[cond]])) {
    df[[cond]] <- as.character(df[[cond]])
  }

  # if the repl column is not character, convert it
  if (!is.character(df[[repl]])) {
    df[[repl]] <- as.character(df[[repl]])
  }


  # calculate summary statistics
  summary_df <- df %>%
    group_by(!!sym(cond), !!sym(repl)) %>%
    summarise(
      rep_mean = mean(!!sym(meas), na.rm = TRUE),
      rep_median = median(!!sym(meas), na.rm = TRUE)
    ) %>%
    ungroup()

  # if bars is specified, use it to summarize the replicates
  if (bars != "") {
    summary_df <- summary_df %>%
      group_by(!!sym(cond)) %>%
      summarise(
        rep_mean = mean(rep_mean, na.rm = TRUE),
        rep_median = median(rep_median, na.rm = TRUE),
      ) %>%
      ungroup()
  }
  # if bars beigns with "mean_", then set rep_summary to "rep_mean"
  # if it begins with "median_", then set rep_summary to "rep_median"
  if (grepl("^mean_", bars)) {
    rep_summary <- "rep_mean"
  } else if (grepl("^median_", bars)) {
    rep_summary <- "rep_median"
  }

  # if summary_df has three columns then it is only cond
  # if it has four columns then it is cond and repl
  if (ncol(summary_df) == 3) {
    # if it has three columns then it is only cond
    s_df <- df %>%
      left_join(summary_df, by = cond) %>%
      mutate(
        diff = abs(!!sym(meas) - !!sym(rep_summary))
      )
  } else if (ncol(summary_df) == 4) {
    # if it has four columns then it is cond and repl
    s_df <- df %>%
      left_join(summary_df, by = c(cond, repl)) %>%
      mutate(
        diff = abs(!!sym(meas) - !!sym(rep_summary))
      )
  }

  # add a row number column to the summary data frame
  s_df <- s_df %>%
    mutate(rowno = row_number())

  # now sort the data frame by Treatment, Replicate and then by smallest to
  # largest difference add a column with the rank of the difference for each
  # treatment and replicate
  if(outlier) {
    # if outlier is TRUE, then reverse the ranking to find the most extreme
    # datapoint
    s_df <- s_df %>%
      mutate(diff = -diff)
  }

  s_df <- s_df %>%
    arrange(!!sym(cond), !!sym(repl), diff) %>%
    group_by(!!sym(cond), !!sym(repl)) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  # if label is specified, add it to the summary data frame
  if (label != "") {
    s_df <- s_df %>%
      select(!!sym(cond), !!sym(repl), !!sym(meas), !!sym(label), diff, rank)
  } else {
    s_df <- s_df %>%
      select(!!sym(cond), !!sym(repl), !!sym(meas), rowno, diff, rank)
  }

  # print the number 1 ranked choices for each treatment and replicate
  ranked_choices <- s_df %>%
    filter(rank == 1)

  print(ranked_choices)

  return(s_df)
}
