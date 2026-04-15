# FlatPlot Colour Selection (Single Colour)

This function takes a single character name/code and returns the
corresponding hex colour. It checks if the input is a valid hex colour
code or a valid R color name, and if not, it uses a predefined mapping
for specific color names like "rl_green", "rl_red", etc. If the input is
not valid, it will return NULL. Use of `get_fp_color` is valid too.

## Usage

``` r
get_fp_colour(key)

get_fp_color(key)
```

## Arguments

- key:

  character name, can be "rl_green", "rl_red", "rl_blue", "rl_yellow",
  "rl_purple", "rl_orange", "rl_magenta", "rl_inv", or a hex colour, or
  a valid R color name

## Value

character of hex colour

## Examples

``` r
get_fp_colour("rl_green")
#> [1] "#00a651"
get_fp_color("rl_green")
#> [1] "#00a651"
```
