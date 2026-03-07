# Add Mean ± Error Bars to a SuperPlot

Add Mean ± Error Bars to a SuperPlot

## Usage

``` r
add_sp_bars(p, bars, df, cond, rep_summary)
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

## Value

ggplot object
