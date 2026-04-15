# SuperPlot Colour Palette Selection

This function takes an integer `n` and a character `scheme` and returns
a character vector of `n` colours from the specified colour scheme. The
function supports four colour schemes from Paul Tol ("tol_bright",
"tol_vibrant", "tol_muted", "tol_light") and Color Universal Design
("cud" scheme). If the `scheme` argument is a vector of colours, the
function will return the first `n` colours from that vector, repeating
the vector if it has fewer than `n` colours. If the `scheme` argument is
not a valid colour scheme or a vector of colours, the function will
throw an error. Use of `get_sp_colors()` is valid too.

## Usage

``` r
get_sp_colours(n, scheme)

get_sp_colors(n, scheme)
```

## Arguments

- n:

  integer number of colours to return

- scheme:

  character name, colour scheme to use Choose from one of four palettes
  from Paul Tol ("tol_bright", "tol_vibrant", "tol_muted", "tol_light"),
  or Color Universal Design ("cud"). Or a vector of colours, e.g.
  c("#FF0000", "green", "#0000FF").

## Value

character vector of colours

## Examples

``` r
get_sp_colours(3, "tol_bright")
#> [1] "#4477AA" "#CCBB44" "#EE6677"
get_sp_colors(3, "tol_bright")
#> [1] "#4477AA" "#CCBB44" "#EE6677"
```
