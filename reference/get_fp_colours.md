# FlatPlot Colour Selection (Multiple Colours)

This function takes a character vector of color names/codes and returns
a character vector of hex colours. It uses the `get_fp_colour` function
to convert each color name/code to its corresponding hex colour. The
input can include predefined color names like "rl_green", "rl_red",
etc., as well as hex color codes or valid R color names. Use of
`get_fp_colors` is valid too.

## Usage

``` r
get_fp_colours(keys)

get_fp_colors(keys)
```

## Arguments

- keys:

  character vector of color names/codes, can be "rl_green", "rl_red",
  "rl_blue", "rl_yellow", "rl_purple", "rl_orange", "rl_magenta",
  "rl_inv", or hex colours, or valid R color names

## Value

character vector of hex colours

## Examples

``` r
get_fp_colours(c("rl_green", "#FF0000", "blue"))
#> [1] "#00a651" "#FF0000" "blue"   
get_fp_colours("rl_green")
#> [1] "#00a651"
get_fp_colors("rl_green")
#> [1] "#00a651"
```
