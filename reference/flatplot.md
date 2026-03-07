# Make a FlatPlot

Make a FlatPlot

## Usage

``` r
flatplot(
  df,
  meas,
  cond,
  colour = "#000000",
  xlab = "",
  ylab = "Measurement",
  datadist = "sina",
  size = 2,
  alpha = 0.5,
  bars = "mean_sd",
  fsize = 12,
  gg = NULL,
  stats = FALSE,
  stats_test = "para_unpaired"
)
```

## Arguments

- df:

  data frame with at least three columns: meas, cond, repl

- meas:

  character name of column with measurement (e.g. intensity)

- cond:

  character name of column with condition (e.g. Control, WT)

- colour:

  string for colour palette to use, select ("rl_green", "rl_red",
  "rl_blue", "rl_purple", "rl_orange", "rl_magenta", or a hex colour,
  default is black)

- xlab:

  string for x label (default is empty)

- ylab:

  string for y label (default is "Measurement")

- datadist:

  string for data distribution to use, select ("sina" default, or
  "jitter")

- size:

  numeric size of data points (default is 2)

- alpha:

  numeric vector of alpha range data and summary points (default is
  c(0.5, 0.7))

- bars:

  string for type of error bars to add, select ("none", "mean_sd"
  (default), "mean_sem", or "mean_ci")

- fsize:

  numeric font size for text (default is 12)

- gg:

  ggplot object to add to (default is NULL)

- stats:

  logical for whether to add statistical tests (default is FALSE)

- stats_test:

  string for statistical test to use, select ("para_unpaired",
  "para_paired", "nonpara_unpaired", or "nonpara_paired")

## Value

ggplot object

## Examples

``` r
flatplot(lord_jcb, "Speed", "Treatment", ylab = "Speed (um/min)")

```
