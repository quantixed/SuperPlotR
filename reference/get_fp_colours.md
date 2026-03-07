# FlatPlot Colour Selection (Multiple Colors)

FlatPlot Colour Selection (Multiple Colors)

## Usage

``` r
get_fp_colours(keys)
```

## Arguments

- keys:

  character vector of color names/codes, can be "rl_green", "rl_red",
  "rl_blue", "rl_yellow", "rl_purple", "rl_orange", "rl_magenta", hex
  colours, or valid R color names

## Value

character vector of hex colours

## Examples

``` r
get_fp_colours(c("rl_green", "#FF0000", "blue"))
#> [1] "#00a651" "#FF0000" "blue"   
get_fp_colours("rl_green")
#> [1] "#00a651"
```
