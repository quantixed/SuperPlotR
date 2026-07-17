#' Add Mean ± Error Bars to a SuperPlot
#'
#' @param p ggplot object
#' @param bars character string, one of "mean_sd", "mean_sem", or "mean_ci"
#' @param df data frame of summary data (supplied by superplot())
#' @param cond character string of the condition column
#' @param rep_summary character string of the replicate summary column
#' @param bar_params named list of additional parameters passed to the
#'   errorbar `stat_summary` layer
#' @param crossbar_params named list of additional parameters passed to the
#'   crossbar `stat_summary` layer
#'
#' @return ggplot object
#' @export
add_sp_bars <- function(p, bars, df, cond, rep_summary,
                        bar_params = list(),
                        crossbar_params = list()) {
  base_bar <- list(
    data = df,
    mapping = aes(x = !!sym(cond), y = !!sym(rep_summary)),
    geom = "errorbar",
    linewidth = 0.2,
    width = 0
  )

  if (bars == "mean_sd") {
    p <- p + do.call(stat_summary, utils::modifyList(
      c(base_bar, list(fun.data = trio_sd)),
      bar_params
    ))
  } else if (bars == "mean_sem") {
    p <- p + do.call(stat_summary, utils::modifyList(
      c(base_bar, list(fun.data = trio_sem)),
      bar_params
    ))
  } else if (bars == "mean_ci") {
    p <- p + do.call(stat_summary, utils::modifyList(
      c(base_bar, list(fun.data = trio_ci)),
      bar_params
    ))
  }

  # whatever the user enters, we also want to show the crossbars
  p <- p + do.call(stat_summary, utils::modifyList(
    list(
      data = df,
      mapping = aes(x = !!sym(cond), y = !!sym(rep_summary)),
      fun.data = trio_mean,
      geom = "crossbar",
      linewidth = 0.4,
      width = 0.8
    ),
    crossbar_params
  ))

  return(p)
}

#' Calculate error bars (mean ± standard deviation)
#'
#' @param x input from stat_summary
#'
#' @return data frame of y, ymin, and ymax
#' @keywords internal
trio_sd <- function(x) {
  avg <- mean(x)
  sd <- sd(x)
  trio <- data.frame(
    y = avg,
    ymin = avg - sd,
    ymax = avg + sd)
  return (trio)
}

#' Calculate error bars (mean ± standard error of the mean)
#'
#' @param x input from stat_summary
#'
#' @return data frame of y, ymin, and ymax
#' @keywords internal
trio_sem <- function(x) {
  avg <- mean(x)
  sd <- sd(x)
  n <- length(x)
  sem <-  sd / sqrt(n)
  trio <- data.frame(
    y = avg,
    ymin = avg - sem,
    ymax = avg + sem)
  return(trio)
}

#' Calculate error bars (mean ± 95% confidence interval)
#'
#' @param x input from stat_summary
#' @importFrom stats qt
#'
#' @return data frame of y, ymin, and ymax
#' @keywords internal
trio_ci <- function(x) {
  avg <- mean(x)
  sd <- sd(x)
  n <- length(x)
  sem <-  sd / sqrt(n)
  ci = qt(0.975, df = n - 1) * sem
  trio <- data.frame(
    y = avg,
    ymin = avg - ci,
    ymax = avg + ci)
  return(trio)
}

#' Calculate mean cross bar
#'
#' @param x input from stat_summary
#'
#' @return data frame of y, ymin, and ymax
#' @keywords internal
trio_mean <- function(x) {
  avg <- mean(x)
  trio <- data.frame(
    y = avg,
    ymin = avg,
    ymax = avg)
  return(trio)
}

