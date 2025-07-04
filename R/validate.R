#' Validate SuperPlot Arguments
#'
#' @param pal argument passed to pal
#' @param colour argument passed to colour
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
#'
#' @returns none
#' @keywords internal
validate_args <- function(pal = NULL, colour = NULL, xlab = NULL, ylab = NULL,
                          datadist = NULL, size = NULL, alpha = NULL,
                          bars = NULL, linking = NULL, rep_summary = NULL,
                          shapes = NULL, fsize = NULL, gg = NULL, stats = NULL,
                          stats_test = NULL, info = NULL) {
  if (!is.null(pal)) check_pal(pal)
  if (!is.null(colour)) check_colour(colour)
  if (!is.null(xlab)) check_xlab(xlab)
  if (!is.null(ylab)) check_ylab(ylab)
  if (!is.null(datadist)) check_datadist(datadist)
  if (!is.null(size)) check_size(size)
  if (!is.null(alpha)) check_alpha(alpha)
  if (!is.null(bars)) check_bars(bars)
  if (!is.null(linking)) check_linking(linking)
  if (!is.null(rep_summary)) check_rep_summary(rep_summary)
  if (!is.null(shapes)) check_shapes(shapes)
  if (!is.null(fsize)) check_fsize(fsize)
  if (!is.null(gg)) check_gg(gg)
  if (!is.null(stats)) check_stats(stats)
  if (!is.null(stats_test)) check_stats_test(stats_test)
  if (!is.null(info)) check_info(info)
}

#' Check pal argument
#'
#' @param arg argument passed as pal
#' @returns none
#' @keywords internal
check_pal <- function(arg) {
  # pal should be a list of colours, or one of the following:
  # tol_bright, tol_vibrant, tol_muted, tol_light, cud
  # first test if it is a character vector of length 1
  if (length(arg) == 1) {
    if (!arg %in% c("tol_bright", "tol_vibrant", "tol_muted",
                                      "tol_light", "cud")) {
      stop("'pal' must be a vector of colours or one of tol_bright, tol_vibrant,
             tol_muted, tol_light, cud", call. = FALSE)
    }
  if (is.vector(arg) && length(arg) > 1) {
      if (!all(sapply(arg, function(x) is.character(x)))) {
        stop("'pal' must be a vector of colours or one of tol_bright, tol_vibrant,
             tol_muted, tol_light, cud", call. = FALSE)
      }
    }
  }
}

#' Check colour argument
#'
#' @param arg argument passed as colour
#' @returns none
#' @keywords internal
check_colour <- function(arg) {
  # colour should be character, one of the following:
  # "rl_green", "rl_red", "rl_blue", "rl_purple", "rl_orange", "rl_magenta", or
  # a hex colour
  # first test if it is a character vector of length 1
  if (length(arg) > 1) {
    stop("'colour' must be a hex colour or one of rl_green, rl_red, rl_blue,
         rl_purple, rl_orange, or rl_magenta", call. = FALSE)
  } else if (!arg %in% c("rl_green", "rl_red", "rl_blue", "rl_purple",
                         "rl_orange", "rl_magenta")) {
    if (!grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", arg)) {
      stop("'colour' must be a hex colour or one of rl_green, rl_red, rl_blue,
           rl_purple, rl_orange, or rl_magenta", call. = FALSE)
    }
  }
}

#' Check xlab argument
#'
#' @param arg argument passed as xlab
#' @returns none
#' @keywords internal
check_xlab <- function(arg) {
  # xlab should be a character
  if (!is.character(arg)) {
    stop("'xlab' must be a character", call. = FALSE)
  }
}

#' Check ylab argument
#'
#' @param arg argument passed as ylab
#' @returns none
#' @keywords internal
check_ylab <- function(arg) {
  # ylab should be a character
  if (!is.character(arg)) {
    stop("'ylab' must be a character", call. = FALSE)
  }
}

#' Check datadist argument
#'
#' @param arg argument passed as datadist
#' @returns none
#' @keywords internal
check_datadist <- function(arg) {
  # datadist should be one of the following:
  # "sina", "jitter", "boxplot", "violin"
  if (!arg %in% c("sina", "jitter", "boxplot", "violin")) {
    stop("'datadist' must be one of sina, jitter, boxplot, violin",
         call. = FALSE)
  }
}

#' Check size argument
#'
#' @param arg argument passed as size
#' @returns none
#' @keywords internal
check_size <- function(arg) {
  # size should be a vector of length 2
  if (!is.vector(arg) | length(arg) != 2) {
    stop("'size' must be a vector of length 2", call. = FALSE)
  }
}

#' Check alpha argument
#'
#' @param arg argument passed as alpha
#' @returns none
#' @keywords internal
check_alpha <- function(arg) {
  # alpha should be a vector of length 2
  if (!is.vector(arg) | length(arg) != 2) {
    stop("'alpha' must be a vector of length 2", call. = FALSE)
  }
}

#' Check bars argument
#'
#' @param arg argument passed as bars
#' @returns none
#' @keywords internal
check_bars <- function(arg) {
  # bars should be a character
  if (!is.character(arg)) {
    stop("'bars' must be a character", call. = FALSE)
  }
}

#' Check linking argument
#'
#' @param arg argument passed as linking
#' @returns none
#' @keywords internal
check_linking <- function(arg) {
  # linking should be a logical
  if (!is.logical(arg)) {
    stop("'linking' must be a logical", call. = FALSE)
  }
}

#' Check rep_summary argument
#'
#' @param arg argument passed as rep_summary
#' @returns none
#' @keywords internal
check_rep_summary <- function(arg) {
  # rep_summary should be one of the following:
  # "rep_mean", "rep_median"
  if (!arg %in% c("rep_mean", "rep_median")) {
    stop("'rep_summary' must be one of rep_mean, rep_median", call. = FALSE)
  }
}

#' Check shapes argument
#'
#' @param arg argument passed as shapes
#' @returns none
#' @keywords internal
check_shapes <- function(arg) {
  # shapes should be a logical
  if (!is.logical(arg)) {
    stop("'shapes' must be a logical", call. = FALSE)
  }
}

#' Check fsize argument
#'
#' @param arg argument passed as fsize
#' @returns none
#' @keywords internal
check_fsize <- function(arg) {
  # fsize should be a numeric
  if (!is.numeric(arg)) {
    stop("'fsize' must be a numeric", call. = FALSE)
  }
}

#' Check gg argument
#'
#' @param arg ggplot object
#' @returns none
#' @keywords internal
check_gg <- function(arg) {
  if (!inherits(arg, "gg")) {
    stop("'gg' must be a ggplot object", call. = FALSE)
  }
}

#' Check stats argument
#'
#' @param arg argument passed as stats
#' @returns none
#' @keywords internal
check_stats <- function(arg) {
  # stats should be either TRUE or FALSE only
  if (!is.logical(arg)) {
    stop("'stats' must be a logical", call. = FALSE)
  }
}

#' Check stats_test argument
#'
#' @param arg argument passed as stats_test
#' @returns none
#' @keywords internal
check_stats_test <- function(arg) {
  # stats_test should be one of the following:
  # "para_unpaired", "para_paired", "nonpara_unpaired", or "nonpara_paired"
  if (!arg %in% c("para_unpaired", "para_paired", "nonpara_unpaired",
                  "nonpara_paired")) {
    stop("'stats_test' must be one of para_unpaired, para_paired,
         nonpara_unpaired, nonpara_paired", call. = FALSE)
  }
}

#' Check info argument
#'
#' @param arg argument passed as info
#' @returns none
#' @keywords internal
check_info <- function(arg) {
  # info should be a logical
  if (!is.logical(arg)) {
    stop("'info' must be a logical", call. = FALSE)
  }
}
