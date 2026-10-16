# SuperPlot Colour Palette Selection

This function takes an integer `n` and a character `scheme` and returns
a character vector of `n` colours from the specified colour scheme. The
function supports four colour schemes from Paul Tol ("tol_bright",
"tol_vibrant", "tol_muted", "tol_light", "tol_rainbow") and Color
Universal Design ("cud" scheme). If the `scheme` argument is a vector of
colours, the function will return the first `n` colours from that
vector, repeating the vector if it has fewer than `n` colours. If the
`scheme` argument is not a valid colour scheme or a vector of colours,
the function will throw an error. Use of `get_sp_colors()` is valid too.

## Usage

``` r
get_sp_colours(n, scheme, order = NULL)

get_sp_colors(n, scheme, order = NULL)
```

## Arguments

- n:

  integer number of colours to return

- scheme:

  character name, colour scheme to use Choose from one of four palettes
  from Paul Tol ("tol_bright", "tol_vibrant", "tol_muted", "tol_light",
  "tol_rainbow"), or Color Universal Design ("cud"). Or a vector of
  colours, e.g. c("#FF0000", "green", "#0000FF").

- order:

  optional ordering of the colours: NULL (default, no change),
  "reverse", "random" (the order used is printed), or an integer vector
  that is a permutation of 1:n, e.g. c(2, 1, 3). Note that c(1, 5, 3, 4,
  2, 6, 7) works well.

## Value

character vector of colours

## Examples

``` r
get_sp_colours(3, "tol_bright")
#> [1] "#4477AA" "#EE6677" "#228833"
get_sp_colours(3, "tol_bright", order = "reverse")
#> [1] "#228833" "#EE6677" "#4477AA"
get_sp_colors(3, "tol_bright")
#> [1] "#4477AA" "#EE6677" "#228833"
```
