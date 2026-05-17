#' Verify the data frame used for SuperPlot
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#' @param repl character name of column with replicate (e.g. unique experiment
#'   identifiers)
#' @param facet character name of column to facet by (e.g. further grouping
#'   variable, default is NULL)
#'
#' @return logical to allow plot to go ahead
#' @keywords internal
verify_sp_columns <- function(df, meas, cond, repl, facet = NULL) {
  # check that meas, cond and repl are character
  if (!is.character(meas) | !is.character(cond) | !is.character(repl)) {
    message("meas, cond and repl must be character")
    return(FALSE)
  }
  # check that facet is character or NULL
  if (!is.null(facet) & !is.character(facet)) {
    message("facet must be character or NULL")
    return(FALSE)
  }
  # verify the data frame - check if the required columns are present
  if (!cond %in% colnames(df) | !repl %in% colnames(df) | !meas %in% colnames(df)) {
    message("The data frame does not contain the required columns")
    return(FALSE)
  }
  # if facet was not NULL, check that the facet column is present
  if (!is.null(facet) && !facet %in% colnames(df)) {
    message("The data frame does not contain the facet column")
    return(FALSE)
  }
  # check that column meas is numeric
  if (!is.numeric(df[[meas]])) {
    message("The column ", meas, " is not numeric")
    return(FALSE)
  }
  return(TRUE)
}

#' Verify the data frame used for FlatPlot
#'
#' @param df data frame with at least three columns: meas, cond, repl
#' @param meas character name of column with measurement (e.g. intensity)
#' @param cond character name of column with condition (e.g. Control, WT)
#'
#' @return logical to allow plot to go ahead
#' @keywords internal
verify_fp_columns <- function(df, meas, cond) {
  # check that meas, cond and repl are character
  if (!is.character(meas) | !is.character(cond)) {
    message("meas and cond must be character")
    return(FALSE)
  }
  # verify the data frame - check if the required columns are present
  if (!cond %in% colnames(df) | !meas %in% colnames(df)) {
    message("The data frame does not contain the required columns")
    return(FALSE)
  }
  # check that column meas is numeric
  if (!is.numeric(df[[meas]])) {
    message("The column ", meas, " is not numeric")
    return(FALSE)
  }
  return(TRUE)
}


#' Ignore unused imports
#'
#' Currently patchwork is used in the documentation but not in the package
#' itself. This function is a workaround to avoid warnings about unused imports
#' when building the package.
#'
#' @keywords keyword
ignore_unused_imports <- function() {
  patchwork::wrap_plots
}
