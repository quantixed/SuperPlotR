# Make a pie chart using ggplot2

The function `pie_maker` creates a pie chart using `ggplot2`. It takes
two vectors of values and optionally a second vector of values to create
a pie chart with two layers.

## Usage

``` r
pieplot(x1, cols, x2 = NULL, label = NULL, ...)
```

## Arguments

- x1:

  A numeric vector containing the values for the first layer of the pie
  chart.

- cols:

  A character vector containing the colors for the slices of the pie
  chart.

- x2:

  An optional numeric vector containing the values for the second layer
  of the pie chart.

- label:

  An optional character string to be used as the title

- ...:

  Currently not used.

## Value

ggplot object

## Examples

``` r
pieplot(x1 = c(123, 456),
  cols = c("#44aa99", "#117733"))


pieplot(x1 = c(50 - 20, 20, 80, 180 - 80),
  cols = c("#bbbbbb", "#44aa99", "#117733", "#dddddd"), x2 = c(100,130))

```
