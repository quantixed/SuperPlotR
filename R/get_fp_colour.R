#' FlatPlot Colour Selection
#'
#' @param key character name, can be "rl_green", "rl_red", "rl_blue",
#' "rl_purple", "rl_orange", "rl_magenta", or a hex colour
#'
#' @return character of hex colour
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
  colour <- switch(key,
         "rl_green" = "#00a651",
         "rl_red" = "#ed1c24",
         "rl_blue" = "#2276b9",
         "rl_purple" = "#64318e",
         "rl_orange" = "#f59331",
         "rl_magenta" = "#da70d6")
  return(colour)
}
