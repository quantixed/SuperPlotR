#" SuperPlot Colour Palette Selection
#"
#" @param n integer number of colours to return
#" @param scheme character name, colour scheme to use
#"  Choose from one of four palettes from Paul Tol ("tol_bright", "tol_vibrant",
#"  "tol_muted", "tol_light"), or Color Universal Design ("cud"). Or a vector of
#"  colours, e.g. c("#FF0000", "green", "#0000FF").
#"
#" @return character vector of colours
#" @export
#"
#" @examples
#" get_sp_colours(3, "tol_bright")
get_sp_colours <- function(n, scheme) {
  colours <- NULL

  # check if scheme is a vector of colours and return it if so
  # will result in an error if user enters non-colours as elements
  if (is.vector(scheme) && length(scheme) > 1) {
    if (length(scheme) < n) {
      colours <- rep(scheme, length.out = n)
    } else {
      colours <- scheme[1:n]
    }
    return(colours)
  }

  tol_bright <- c("#4477AA", "#EE6677", "#228833",
                  "#CCBB44", "#66CCEE", "#AA3377",
                  "#BBBBBB")
  tol_vibrant <-  c("#EE7733", "#0077BB", "#33BBEE",
                    "#EE3377", "#CC3311", "#009988",
                    "#BBBBBB")
  tol_muted <- c("#CC6677", "#332288", "#DDCC77",
                 "#117733", "#88CCEE", "#882255",
                 "#44AA99", "#999933", "#AA4499")
  tol_light <- c("#77AADD", "#EE8866", "#EEDD88",
                 "#FFAABB", "#99DDFF", "#44BB99",
                 "#BBCC33", "#AAAA00", "#DDDDDD")
  cud <- c("#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00",
           "#CC79A7", "#000000")

  # get the colour scheme
  if (scheme == "tol_bright") {
    if (n < 7) {
      selection <-  list(c(1), c(1, 2), c(1, 4, 2), c(1, 3, 4, 2),
                         c(1, 5, 3, 4, 2), c(1, 5, 3, 4, 2, 6))
      x <- selection[[n]]
    } else {
      x <- rep(c(1, 5, 3, 4, 2, 6, 7), length.out = n)
    }
    colours <- tol_bright[x]
  } else if (scheme == "tol_vibrant") {
    x <- rep(c(1, 2, 3, 4, 5, 6, 7), length.out = n)
    colours <- tol_vibrant[x]
  } else if (scheme == "tol_muted") {
    x <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), length.out = n)
    colours <- tol_muted[x]
  } else if (scheme == "tol_light") {
    x <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9), length.out = n)
    colours <- tol_light[x]
  } else if (scheme == "cud") {
    x <- rep(c(1, 2, 3, 4, 5, 6, 7, 8), length.out = n)
    colours <- cud[x]
  } else {
    stop("Invalid colour scheme. Choose a palette or use your own vector of
         colours.")
  }
  return(colours)
}
