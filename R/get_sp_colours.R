#' SuperPlot Colour Palette Selection
#'
#' @param n integer number of colours to return
#' @param scheme character name, colour scheme to use (default is "tol_bright")
#'
#' @return character vector of colours
#' @export
#'
#' @examples
#' get_sp_colours(3, "tol_bright")
get_sp_colours <- function(n, scheme) {
  colours <- NULL
  tol_bright <- c("#EE6677", "#228833", "#4477AA", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB")
  tol_muted <- c("#88CCEE", "#44AA99", "#117733", "#332288", "#DDCC77", "#999933" ,"#CC6677", "#882255", "#AA4499", "#DDDDDD")
  tol_light <- c("#BBCC33", "#AAAA00", "#77AADD", "#EE8866", "#EEDD88", "#FFAABB", "#99DDFF", "#44BB99", "#DDDDDD")
  # get the colour scheme
  if(scheme == "tol_bright") {
    if (n == 1) {
      x <- 3
    } else if (n == 2) {
      x <- c(3, 1)
    } else if (n == 3) {
      x <- c(3, 4, 1)
    } else if (n == 4) {
      x <- c(3, 2, 4, 1)
    } else if (n == 5) {
      x <- c(3, 5, 2, 4, 1)
    } else if (n == 6) {
      x <- c(3, 5, 2, 4, 1, 6)
    } else {
      x <- rep(c(3, 5, 2, 4, 1, 6), length.out = n)
    }
    colours <- tol_bright[x]
  } else if(scheme == "tol_muted") {
    x <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), length.out = n)
    colours <- tol_muted[x]
  } else if(scheme == "tol_light") {
    x <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9), length.out = n)
    colours <- tol_light[x]
  } else {
    stop("Invalid colour scheme. Choose from 'tol_bright', 'tol_muted', or 'tol_light'.")
  }
  return(colours)
}
