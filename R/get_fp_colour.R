#' FlatPlot Colour Selection (Multiple Colours)
#'
#' This function takes a character vector of color names/codes and returns a
#' character vector of hex colours. It uses the `get_fp_colour` function to
#' convert each color name/code to its corresponding hex colour. The input can
#' include predefined color names like "rl_green", "rl_red", etc., as well as
#' hex color codes or valid R color names. Use of `get_fp_colors` is valid too.
#'
#' @param keys character vector of color names/codes, can be "rl_green",
#'   "rl_red", "rl_blue", "rl_yellow", "rl_purple", "rl_orange", "rl_magenta",
#'   "rl_inv", or hex colours, or valid R color names
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
#' @rdname get_fp_colours
#' @examples get_fp_colors("rl_green")
#' @export
get_fp_colors <- get_fp_colours

#' FlatPlot Colour Selection (Single Colour)
#'
#' This function takes a single character name/code and returns the
#' corresponding hex colour. It checks if the input is a valid hex colour code
#' or a valid R color name, and if not, it uses a predefined mapping for
#' specific color names like "rl_green", "rl_red", etc. If the input is not
#' valid, it will return NULL. Use of `get_fp_color` is valid too.
#'
#' @param key character name, can be "rl_green", "rl_red", "rl_blue",
#'   "rl_yellow", "rl_purple", "rl_orange", "rl_magenta", "rl_inv", or a hex
#'   colour, or a valid R color name
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
         "rl_magenta" = "#da70d6",
         "rl_inv" = "#ab66f0")
  return(colour)
}
#' @rdname get_fp_colour
#' @examples get_fp_color("rl_green")
#' @export
get_fp_color <- get_fp_colour
