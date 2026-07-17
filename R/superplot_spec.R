#' Create a SuperPlot Specification
#'
#' @description
#' Build and validate a SuperPlot specification object. This separates data
#' preparation from rendering and enables advanced customization before
#' plotting.
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
#' @param options optional named list used for advanced customization.
#'   Supported names are:
#'   `raw_params`, `summary_params`, `link_params`, `bar_params`,
#'   `crossbar_params`, `theme`, `legend_position`, and `y_limits_from_zero`.
#'
#' @return object of class superplot_spec
#' @export
superplot_spec <- function(df,
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
  ncond <- nrepl <- NULL

  validate_args(pal = pal, xlab = xlab, ylab = ylab, datadist = datadist,
                size = size, alpha = alpha, bars = bars, linking = linking,
                rep_summary = rep_summary, shapes = shapes, fsize = fsize,
                gg = gg, stats = stats, stats_test = stats_test, info = info)

  if (verify_sp_columns(df, meas, cond, repl, facet) == FALSE) {
    return(NULL)
  }

  df <- .sp_prepare_df(df, cond, repl, facet)

  ncond <- df %>%
    pull(!!sym(cond)) %>%
    unique() %>%
    length()
  nrepl <- df %>%
    pull(!!sym(repl)) %>%
    unique() %>%
    length()

  nfacet <- NULL
  if (!is.null(facet)) {
    nfacet <- df %>%
      pull(!!sym(facet)) %>%
      unique() %>%
      length()
  }

  summary_df <- get_sp_summary(df = df,
                               meas = meas, cond = cond, repl = repl,
                               facet = facet)

  .sp_validate_summary_rows(summary_df = summary_df,
                            ncond = ncond,
                            nrepl = nrepl,
                            nfacet = nfacet,
                            info = info,
                            facet = facet)

  sp_colours <- get_sp_colours(nrepl, pal)
  sp_shapes <- get_sp_shapes(nrepl, shapes)

  if (info == TRUE) {
    get_sp_info(df = df,
                meas = meas, cond = cond, repl = repl, facet = facet,
                pal = pal, xlab = xlab, ylab = ylab,
                datadist = datadist, size = size, alpha = alpha,
                bars = bars, linking = linking,
                rep_summary = rep_summary, shapes = shapes,
                fsize = fsize, gg = gg,
                stats = stats, stats_test = stats_test)
  }

  spec <- list(
    data = df,
    summary = summary_df,
    mapping = list(meas = meas, cond = cond, repl = repl, facet = facet),
    params = list(
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
      options = .sp_resolve_options(options = options, info = info)
    ),
    derived = list(
      ncond = ncond,
      nrepl = nrepl,
      nfacet = nfacet,
      sp_colours = sp_colours,
      sp_shapes = sp_shapes
    )
  )

  class(spec) <- "superplot_spec"
  spec
}

#' Modify Advanced SuperPlot Options
#'
#' @param spec object returned by superplot_spec
#' @param options named list of advanced options
#' @param ... additional named options merged into the existing options
#'
#' @return updated superplot_spec object
#' @export
sp_modify <- function(spec, options = NULL, ...) {
  if (!inherits(spec, "superplot_spec")) {
    stop("spec must be a superplot_spec object", call. = FALSE)
  }

  merged_options <- spec$params$options
  if (!is.null(options)) {
    merged_options <- utils::modifyList(merged_options, options)
  }

  dots <- list(...)
  if (length(dots) > 0) {
    merged_options <- utils::modifyList(merged_options, dots)
  }

  spec$params$options <- .sp_resolve_options(
    options = merged_options,
    info = spec$params$info
  )
  spec
}

#' Plot a SuperPlot Specification
#'
#' @param object object returned by superplot_spec
#' @param ... additional arguments (currently unused)
#'
#' @return ggplot object
#' @export
autoplot.superplot_spec <- function(object, ...) {
  p <- object$params$gg
  if (is.null(p)) {
    p <- ggplot()
  }

  df <- object$data
  summary_df <- object$summary

  meas <- object$mapping$meas
  cond <- object$mapping$cond
  repl <- object$mapping$repl
  facet <- object$mapping$facet

  datadist <- object$params$datadist
  size <- object$params$size
  alpha <- object$params$alpha
  bars <- object$params$bars
  linking <- object$params$linking
  rep_summary <- object$params$rep_summary
  xlab <- object$params$xlab
  ylab <- object$params$ylab
  fsize <- object$params$fsize
  stats <- object$params$stats
  stats_test <- object$params$stats_test

  ncond <- object$derived$ncond
  nrepl <- object$derived$nrepl
  sp_colours <- object$derived$sp_colours
  sp_shapes <- object$derived$sp_shapes

  opts <- object$params$options

  if (datadist == "sina") {
    p <- p + do.call(geom_sina, utils::modifyList(
      list(
        data = df,
        mapping = aes(x = !!sym(cond), y = !!sym(meas),
                      colour = !!sym(repl), fill = !!sym(repl),
                      shape = !!sym(repl)),
        alpha = alpha[1],
        stroke = 0,
        jitter_y = FALSE,
        position = "auto",
        size = size[1],
        maxwidth = 0.3
      ),
      opts$raw_params
    ))
  } else if (datadist == "jitter") {
    p <- p + do.call(geom_jitter, utils::modifyList(
      list(
        data = df,
        mapping = aes(x = !!sym(cond), y = !!sym(meas),
                      colour = !!sym(repl), fill = !!sym(repl),
                      shape = !!sym(repl)),
        alpha = alpha[1],
        stroke = 0,
        size = size[1]
      ),
      opts$raw_params
    ))
  } else if (datadist == "violin") {
    p <- p + do.call(geom_violin, utils::modifyList(
      list(
        data = df,
        mapping = aes(x = !!sym(cond), y = !!sym(meas), group = !!sym(cond)),
        fill = "grey",
        width = 0.5,
        alpha = alpha[1]
      ),
      opts$raw_params
    ))
  } else {
    warning("datadist must be one of 'sina', 'jitter', or 'violin'")
  }

  if (linking == TRUE) {
    p <- p + do.call(geom_path, utils::modifyList(
      list(
        data = summary_df,
        mapping = aes(x = !!sym(cond), y = !!sym(rep_summary),
                      group = !!sym(repl)),
        linetype = "dashed",
        alpha = 0.5
      ),
      opts$link_params
    ))
  }

  if (bars != "") {
    p <- add_sp_bars(
      p = p,
      bars = bars,
      df = summary_df,
      cond = cond,
      rep_summary = rep_summary,
      bar_params = opts$bar_params,
      crossbar_params = opts$crossbar_params
    )
  }

  p <- p + do.call(geom_point, utils::modifyList(
    list(
      data = summary_df,
      mapping = aes(x = !!sym(cond), y = !!sym(rep_summary),
                    fill = !!sym(repl), shape = !!sym(repl)),
      colour = "black",
      size = size[2],
      stroke = 0.5,
      alpha = alpha[2]
    ),
    opts$summary_params
  ))

  p <- p + scale_color_manual(values = sp_colours) +
    scale_shape_manual(values = sp_shapes) +
    scale_fill_manual(values = sp_colours) +
    labs(x = xlab, y = ylab)

  if (isTRUE(opts$y_limits_from_zero) && min(df[[meas]], na.rm = TRUE) > 0) {
    p <- p + lims(y = c(0, NA))
  }

  if (!is.null(facet)) {
    p <- p + facet_wrap(as.formula(paste("~", facet)))
  }

  p <- p + theme_cowplot(fsize)

  if (!is.null(opts$theme)) {
    p <- p + opts$theme
  }

  p <- p + theme(legend.position = opts$legend_position)

  if (stats == TRUE) {
    if (!is.null(facet)) {
      warning("Statistical tests are not currently implemented for faceted SuperPlots.")
    } else {
      get_sp_stats(as.data.frame(summary_df), rep_summary, cond, repl,
                   ncond, nrepl, stats_test)
    }
  }

  p
}

.sp_prepare_df <- function(df, cond, repl, facet) {
  if (!is.factor(df[[cond]]) && !is.character(df[[cond]])) {
    df[[cond]] <- as.character(df[[cond]])
  }

  if (!is.character(df[[repl]]) && !is.factor(df[[repl]])) {
    df[[repl]] <- as.character(df[[repl]])
  }

  if (!is.null(facet)) {
    if (!is.character(df[[facet]]) && !is.factor(df[[facet]])) {
      df[[facet]] <- as.character(df[[facet]])
    }
  }

  df
}

.sp_validate_summary_rows <- function(summary_df, ncond, nrepl, nfacet, info,
                                      facet) {
  if (!is.null(facet)) {
    if (nrow(summary_df) != ncond * nrepl * nfacet && info == FALSE) {
      warning("Summary statistics were not calculated for all combinations of\ncondition, replicate, and facet.\nCheck for missing data.\nCall again with info = TRUE to see more details.")
    }
  } else {
    if (nrow(summary_df) != ncond * nrepl && info == FALSE) {
      warning("Summary statistics were not calculated for all combinations of\ncondition and replicate.\nCheck for missing data.\nCall again with info = TRUE to see more details.")
    }
  }
}

.sp_resolve_options <- function(options, info) {
  defaults <- list(
    raw_params = list(),
    summary_params = list(),
    link_params = list(),
    bar_params = list(),
    crossbar_params = list(),
    theme = NULL,
    legend_position = if (isTRUE(info)) "right" else "none",
    y_limits_from_zero = TRUE
  )

  if (is.null(options)) {
    return(defaults)
  }

  utils::modifyList(defaults, options)
}
