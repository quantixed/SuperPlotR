# Make a SuperPlot

The function `superplot` creates a SuperPlot using `ggplot2`.

## Usage

``` r
superplot(
  df,
  meas,
  cond,
  repl,
  pal = "tol_bright",
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
  info = FALSE
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

- pal:

  name of colour palette to use (default is "tol_bright")

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

## Value

ggplot object

## Examples

``` r
superplot(lord_jcb,
  "Speed", "Treatment", "Replicate",
  ylab = "Speed (um/min)")

```
