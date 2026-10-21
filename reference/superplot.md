# Make a SuperPlot

The function `superplot()` creates a SuperPlot using `ggplot2`. At a
minimum the user needs to supply a data frame with at least three
columns that have the measurement (meas), condition (cond) and replicate
(repl) information. There are direct arguments to change the colours,
axes labels, sizes, and more (see details below). There is also a
flexible `options` argument to supply a list to specify even more
customisation.

If you are building a very complicated SuperPlot, it is preferable to
specify a superplot_spec using
[`superplot_spec()`](https://quantixed.github.io/SuperPlotR/reference/superplot_spec.md)
and
[`sp_modify()`](https://quantixed.github.io/SuperPlotR/reference/sp_modify.md)
before plotting with `autoplot()`

## Usage

``` r
superplot(
  df,
  meas,
  cond,
  repl,
  facet = NULL,
  pal = "tol_bright",
  pal_order = NULL,
  xlab = "",
  ylab = "Measurement",
  datadist = "sina",
  size = c(2, 3),
  alpha = c(0.5, 0.7),
  bars = "mean_sd",
  linking = FALSE,
  rep_summary = "rep_mean",
  shapes = FALSE,
  fsize = 12,
  gg = NULL,
  stats = FALSE,
  stats_test = "para_unpaired",
  info = FALSE,
  options = NULL
)
```

## Arguments

- df:

  data frame with at least three columns: meas, cond, repl

- meas:

  character name of column with measurement (e.g. intensity)

- cond:

  character name of column with condition (e.g. Control, WT)

- repl:

  character name of column with replicate (e.g. unique experiment
  identifiers)

- facet:

  character name of column to facet by (default is NULL)

- pal:

  name of colour palette to use (default is "tol_bright")

- pal_order:

  optional order of the palette colours: NULL (default), "reverse",
  "random" (the order used is printed so it can be reused), or an
  integer vector containing a permutation of the sequence of numbers
  from 1 to the total

- xlab:

  string for x label (default is empty)

- ylab:

  string for y label (default is "Measurement")

- datadist:

  string for data distribution to use, select ("sina" default, "jitter",
  or "violin")

- size:

  numeric vector of size range data and summary points (default is c(2,
  3))

- alpha:

  numeric vector of alpha range data and summary points (default is
  c(0.5, 0.7))

- bars:

  string for type of error bars to add, select "mean_sd" (default),
  "mean_sem", or "mean_ci"; for no bars use an empty string (""); for no
  error bars but still show the mean with a crossbar, use "none".

- linking:

  logical for whether to link summary points between conditions (default
  is FALSE)

- rep_summary:

  string for summary statistic to use for replicates, select ("rep_mean"
  default, or "rep_median")

- shapes:

  logical for whether to use different shapes for replicates

- fsize:

  numeric font size for text (default is 12)

- gg:

  ggplot object to add to (default is NULL)

- stats:

  logical for whether to add statistical tests (default is FALSE)

- stats_test:

  string for statistical test to use, select ("para_unpaired",
  "para_paired", "nonpara_unpaired", or "nonpara_paired")

- info:

  logical for whether to print information about the plot (default is
  FALSE)

- options:

  optional named list for advanced layer customization. See
  [`superplot_spec()`](https://quantixed.github.io/SuperPlotR/reference/superplot_spec.md)
  and
  [`sp_modify()`](https://quantixed.github.io/SuperPlotR/reference/sp_modify.md)
  for available options.

## Value

ggplot object

## Examples

``` r
superplot(lord_jcb,
  "Speed", "Treatment", "Replicate",
  ylab = "Speed (um/min)")

```
