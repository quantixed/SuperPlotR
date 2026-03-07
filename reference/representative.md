# Report the representative datapoints in a SuperPlot

This function finds the representative datapoints for each condition and
replicate in a SuperPlot dataset. It calculates the mean or median of
the replicates (conditions), then ranks the individual measurements by
their difference from the summary statistic. The function returns a data
frame with the representative datapoints for each treatment and
replicate, along with their rank of difference from the summary
statistic.

## Usage

``` r
representative(
  df,
  meas,
  cond,
  repl,
  label = "",
  bars = "",
  rep_summary = "rep_mean",
  outlier = FALSE
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

- label:

  character name of column with labels for measurements (e.g. file
  names), optional.

- bars:

  string to specify the summary stats of replicate sumaries, select
  ("none" default, "mean_sd", "mean_sem", or "mean_ci"). Sepcifying this
  overrides the `rep_summary` argument

- rep_summary:

  string for summary statistic to use for replicates, select ("rep_mean"
  default, or "rep_median")

- outlier:

  logical, if TRUE then ranking is reversed to find the most extreme
  datapoint in each treatment and replicate, default is FALSE.

## Value

data frame with the representative datapoints for each treatment and
replicate, with the rank of the difference from the summary statistic.

## Details

The top ranked datapoint for each treatment and replicate is printed to
the console. This is useful for identifying the most representative
datapoint for each condition, which can be used for showing in a figure.

A label column can be specified to identify the datapoint (this could be
a file name or other identifier). If no label is specified, the row
number is used instead.

## Examples

``` r
representative(lord_jcb,
  "Speed", "Treatment", "Replicate")
#> # A tibble: 6 × 6
#>   Treatment Replicate Speed rowno     diff  rank
#>   <chr>     <chr>     <dbl> <int>    <dbl> <int>
#> 1 Control   1          41.5     5 0.0524       1
#> 2 Control   2          32.9    98 0.220        1
#> 3 Control   3          20.7   124 0.0541       1
#> 4 Drug      1          29.4   178 0.217        1
#> 5 Drug      2          22.3   207 0.000237     1
#> 6 Drug      3          12.9   285 0.0113       1
#> # A tibble: 300 × 6
#>    Treatment Replicate Speed rowno   diff  rank
#>    <chr>     <chr>     <dbl> <int>  <dbl> <int>
#>  1 Control   1          41.5     5 0.0524     1
#>  2 Control   1          41.4    13 0.0875     2
#>  3 Control   1          41.9     2 0.366      3
#>  4 Control   1          42.2    31 0.688      4
#>  5 Control   1          40.2    26 1.26       5
#>  6 Control   1          39.6    36 1.86       6
#>  7 Control   1          39.5    37 1.97       7
#>  8 Control   1          43.7     1 2.20       8
#>  9 Control   1          39.2    27 2.24       9
#> 10 Control   1          43.8    17 2.27      10
#> # ℹ 290 more rows
```
