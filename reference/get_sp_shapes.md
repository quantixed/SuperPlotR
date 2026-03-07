# Get shapes for data and summary plots

Get shapes for data and summary plots

## Usage

``` r
get_sp_shapes(n, shape_logical)
```

## Arguments

- n:

  number of shapes to return

- shape_logical:

  logical for whether to use different shapes for replicates. Default is
  FALSE, which results in a circle being used. TRUE means that a
  selection of shapes are used, with data and summary points for each
  replicate being the same. Four shapes are possible, so if n is greater
  than 4, the shapes will repeat (but with different colors).

## Value

contiguous vector of shape integers

## Examples

``` r
get_sp_shapes(3, FALSE)
#> [1] 21 21 21
```
