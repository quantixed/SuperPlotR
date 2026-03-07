# SuperPlot Colour Palette Selection

SuperPlot Colour Palette Selection

## Usage

``` r
get_sp_colours(n, scheme)
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
```
