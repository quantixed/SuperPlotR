#' SuperPlot Colour Palette Selection
#'
#' This function takes an integer `n` and a character `scheme` and returns a
#' character vector of `n` colours from the specified colour scheme. The
#' function supports four colour schemes from Paul Tol ("tol_bright",
#' "tol_vibrant", "tol_muted", "tol_light", "tol_rainbow") and Color Universal Design ("cud"
#' scheme). If the `scheme` argument is a vector of colours, the function will
#' return the first `n` colours from that vector, repeating the vector if it has
#' fewer than `n` colours. If the `scheme` argument is not a valid colour scheme
#' or a vector of colours, the function will throw an error. Use of
#' `get_sp_colors()` is valid too.
#'
#' @param n integer number of colours to return
#' @param scheme character name, colour scheme to use
#'  Choose from one of four palettes from Paul Tol ("tol_bright", "tol_vibrant",
#'  "tol_muted", "tol_light", "tol_rainbow"), or Color Universal Design ("cud"). Or a vector of
#'  colours, e.g. c("#FF0000", "green", "#0000FF").
#'
#' @param order optional ordering of the colours: NULL (default, no change),
#'  "reverse", "random" (the order used is printed), or an integer vector that
#'  is a permutation of 1:n, e.g. c(2, 1, 3). Note that c(1, 5, 3, 4, 2, 6, 7)
#'  works well.
#'
#' @return character vector of colours
#' @export
#'
#' @examples
#' get_sp_colours(3, "tol_bright")
#' get_sp_colours(3, "tol_bright", order = "reverse")
get_sp_colours <- function(n, scheme, order = NULL) {
  colours <- .sp_base_colours(n, scheme)
  order <- resolve_pal_order(order, n)
  if (!is.null(order)) colours <- colours[order]
  colours
}

#' Resolve the palette order argument
#'
#' @param order NULL, "reverse", "random" or an integer vector
#' @param n number of colours
#' @returns NULL or an integer vector of length n (prints it if random)
#' @keywords internal
resolve_pal_order <- function(order, n) {
  if (is.null(order)) return(NULL)
  if (is.character(order) && length(order) == 1) {
    if (order == "reverse") return(rev(seq_len(n)))
    if (order == "random") {
      order <- sample(seq_len(n))
      message("Random colour order: c(", paste(order, collapse = ", "),
              ") - use this as pal_order to reuse it")
      return(order)
    }
  }
  if (!is.numeric(order) || anyNA(order) || any(order != round(order))) {
    stop("'pal_order' must be \"random\", \"reverse\" or a vector of integers",
         call. = FALSE)
  }
  if (length(order) != n) {
    stop("'pal_order' must have length ", n,
         " (the number of replicates), not ", length(order), call. = FALSE)
  }
  if (!identical(as.integer(sort(order)), seq_len(n))) {
    stop("'pal_order' must contain each integer from 1 to ", n, " once",
         call. = FALSE)
  }
  as.integer(order)
}

.sp_base_colours <- function(n, scheme) {
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
                 "#44AA99", "#999933", "#AA4499", "#DDDDDD")
  tol_light <- c("#77AADD", "#EE8866", "#EEDD88",
                 "#FFAABB", "#99DDFF", "#44BB99",
                 "#BBCC33", "#AAAA00", "#DDDDDD")
  tol_rainbow = c("#E8ECFB", "#D9CCE3", "#D1BBD7", "#CAACCB", "#BA8DB4",
                "#AE76A3", "#AA6F9E", "#994F88", "#882E72", "#1965B0",
                "#437DBF", "#5289C7", "#6195CF", "#7BAFDE", "#4EB265",
                "#90C987", "#CAE0AB", "#F7F056", "#F7CB45", "#F6C141",
                "#F4A736", "#F1932D", "#EE8026", "#E8601C", "#E65518",
                "#DC050C", "#A5170E", "#72190E", "#42150A")
  cud <- c("#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00",
           "#CC79A7", "#000000")

  # get the colour scheme
  if (scheme == "tol_bright") {
    x <- rep_len(1:7, n)
    colours <- tol_bright[x]
  } else if (scheme == "tol_vibrant") {
    x <- rep_len(1:7, n)
    colours <- tol_vibrant[x]
  } else if (scheme == "tol_muted") {
    x <- rep_len(1:10, n)
    colours <- tol_muted[x]
  } else if (scheme == "tol_light") {
    x <- rep_len(1:9, n)
    colours <- tol_light[x]
  } else if (scheme == "tol_rainbow") {
    selection <-  list(c(9), c(9, 25), c(9, 17, 25), c(9, 14, 17, 25), c(9, 13, 14, 17,
                25), c(9, 13, 14, 16, 17, 25), c(8, 9, 13, 14, 16, 17, 25), c(8,
                9, 13, 14, 16, 17, 22, 25), c(8, 9, 13, 14, 16, 17, 22, 25, 27),
                c(8, 9, 13, 14, 16, 17, 20, 23, 25, 27), c(8, 9, 11, 13, 14, 16,
                17, 20, 23, 25, 27), c(2, 5, 8, 9, 11, 13, 14, 16, 17, 20, 23,
                25), c(2, 5, 8, 9, 11, 13, 14, 15, 16, 17, 20, 23, 25), c(2, 5,
                8, 9, 11, 13, 14, 15, 16, 17, 19, 21, 23, 25), c(2, 5, 8, 9, 11,
                13, 14, 15, 16, 17, 19, 21, 23, 25, 27), c(2, 4, 6, 8, 9, 11,
                13, 14, 15, 16, 17, 19, 21, 23, 25, 27), c(2, 4, 6, 7, 8, 9, 11,
                13, 14, 15, 16, 17, 19, 21, 23, 25, 27), c(2, 4, 6, 7, 8, 9, 11,
                13, 14, 15, 16, 17, 19, 21, 23, 25, 26, 27), c(1, 3, 4, 6, 7, 8,
                9, 11, 13, 14, 15, 16, 17, 19, 21, 23, 25, 26, 27), c(1, 3, 4,
                6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 19, 21, 23, 25, 26,
                27), c(1, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 20,
                22, 24, 25, 26, 27), c(1, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15,
                16, 17, 18, 20, 22, 24, 25, 26, 27, 28), c(0, 1, 3, 4, 6, 7, 8,
                9, 10, 12, 13, 14, 15, 16, 17, 18, 20, 22, 24, 25, 26, 27, 28))
    if (n < 24) {
      x <- selection[[n]]
    } else {
      x <- rep_len(1:27, n)
    }
    colours <- tol_rainbow[x]
  } else if (scheme == "cud") {
    x <- rep_len(1:8, n)
    colours <- cud[x]
  } else {
    stop("Invalid colour scheme. Choose a palette or use your own vector of
         colours.")
  }
  return(colours)
}
#' @rdname get_sp_colours
#' @examples get_sp_colors(3, "tol_bright")
#' @export
get_sp_colors <- get_sp_colours
