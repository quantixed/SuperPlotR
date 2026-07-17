# Add Mean ± Error Bars to a SuperPlot

Add Mean ± Error Bars to a SuperPlot

## Usage

``` r
add_sp_bars(
  p,
  bars,
  df,
  cond,
  rep_summary,
  bar_params = list(),
  crossbar_params = list()
)
```

## Arguments

- p:

  ggplot object

- bars:

  character string, one of "mean_sd", "mean_sem", or "mean_ci"

- df:

  data frame of summary data (supplied by superplot())

- cond:

  character string of the condition column

- rep_summary:

  character string of the replicate summary column

- bar_params:

  named list of additional parameters passed to the errorbar
  `stat_summary` layer

- crossbar_params:

  named list of additional parameters passed to the crossbar
  `stat_summary` layer

## Value

ggplot object
