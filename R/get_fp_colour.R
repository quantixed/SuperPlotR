#' FlatPlot Colour Selection (Multiple Colors)
#'
#' @param keys character vector of color names/codes, can be "rl_green", "rl_red", "rl_blue",
#' "rl_yellow", "rl_purple", "rl_orange", "rl_magenta", hex colours, or valid R color names
#'
#' @return character vector of hex colours
#' @export
#'
#' @examples
#' get_fp_colours(c("rl_green", "#FF0000", "blue"))
#' get_fp_colours("rl_green")
get_fp_colours <- function(keys) {
  # Apply get_fp_colour to each element in the vector
  sapply(keys, get_fp_colour, USE.NAMES = FALSE)
}

#' FlatPlot Colour Selection (Single Color)
#'
#' @param key character name, can be "rl_green", "rl_red", "rl_blue",
#' "rl_yellow", "rl_purple", "rl_orange", "rl_magenta", or a hex colour
#'
#' @return character of hex colour
#' @importFrom grDevices colors
#' @export
#'
#' @examples
#' get_fp_colour("rl_green")
get_fp_colour <- function(key) {
  # key has already been validated to be either a single hex value or one of
  # the allowed strings
  # so, if it is a hex code, return it
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", key)) {
    return(key)
  }
  valid_r_colours <- colors()
  # if the key is a valid R colour, return it
  if (key %in% valid_r_colours) {
    return(key)
  }
  colour <- switch(key,
         "rl_green" = "#00a651",
         "rl_red" = "#ed1c24",
         "rl_blue" = "#2276b9",
         "rl_yellow" = "#ffc620",
         "rl_purple" = "#64318e",
         "rl_orange" = "#f59331",
         "rl_magenta" = "#da70d6")
  return(colour)
}
