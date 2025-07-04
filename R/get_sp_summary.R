#' Get summary statistics for SuperPlot data
#'
#' This function calculates summary statistics (mean and median) for each
#' combination of condition and replicate in a SuperPlot dataset.
#'
#' It is useful to get teh summary data and computing further stats or making
#' plots. The function only needs 4 parameters, anything else is ignored. This
#' means you can simply exchange get_sp_summary() for your superplot() call and
#' get the correct data frame to work with.
#'
#' @param df A data frame containing the experimental data
#' @param meas Character string specifying the name of the column containing
#'   the measurements/values to summarize (e.g., "intensity", "speed")
#' @param cond Character string specifying the name of the column containing
#'   the experimental conditions (e.g., "Treatment", "Genotype")
#' @param repl Character string specifying the name of the column containing
#'   the replicate identifiers (e.g., "Replicate", "Experiment")
#' @param ... Additional arguments (ignored)
#'
#' @return A data frame with columns for condition, replicate, rep_mean
#'   (mean of measurements), and rep_median (median of measurements)
#'
#' @import dplyr
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' # Using the built-in dataset
#' get_sp_summary(lord_jcb, "Speed", "Treatment", "Replicate")
#'
get_sp_summary <- function(df, meas, cond, repl, ...) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!meas %in% names(df)) {
    stop("meas column '", meas, "' not found in data frame")
  }

  if (!cond %in% names(df)) {
    stop("cond column '", cond, "' not found in data frame")
  }

  if (!repl %in% names(df)) {
    stop("repl column '", repl, "' not found in data frame")
  }

  # Calculate summary statistics grouped by condition and replicate
  sdf <- df %>%
    group_by(!!sym(cond), !!sym(repl)) %>%
    summarise(
      rep_mean = mean(!!sym(meas), na.rm = TRUE),
      rep_median = median(!!sym(meas), na.rm = TRUE),
      .groups = "drop"
    )

  return(sdf)
}
