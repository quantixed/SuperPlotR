#' @title Get shapes for data and summary plots
#'
#' @param n number of shapes to return
#' @param shape_logical logical for whether to use different shapes for
#' replicates. Default is FALSE, which results in a circle being used. TRUE
#' means that a selection of shapes are used, with data and summary points for
#' each replicate being the same. Four shapes are possible, so if n is greater
#' than 4, the shapes will repeat (but with different colors).
#'
#' @return contiguous vector of shape integers
#' @export
#'
#' @examples
#' get_sp_shapes(3, FALSE)
get_sp_shapes <- function(n, shape_logical) {
  # the original idea here was to have shapes specified for both data and
  # summary points, but this was too complicated due to scale_shape_manual
  # overriding the shapes. Instead, we will use the same shapes for both.
  # data_shapes are
  data_shapes <- c(21, 22, 24, 23, 25)

  if (shape_logical == TRUE) {
    sp_shapes <- rep(data_shapes, length.out = n)
  } else {
    # just circles
    sp_shapes <- rep(21, length.out = n)
  }

  return(sp_shapes)
}
