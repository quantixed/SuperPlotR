# Information about a SuperPlot

This function prints information about a SuperPlot, it runs internally
from superplot()

## Usage

``` r
get_sp_info(
  df,
  meas,
  cond,
  repl,
  pal,
  xlab,
  ylab,
  datadist,
  size,
  alpha,
  bars,
  linking,
  rep_summary,
  shapes,
  fsize,
  gg,
  stats,
  stats_test,
  ...
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

  argument passed to pal

- xlab:

  argument passed to xlab

- ylab:

  argument passed to ylab

- datadist:

  argument passed to datadist

- size:

  argument passed to size

- alpha:

  argument passed to alpha

- bars:

  argument passed to bars

- linking:

  argument passed to linking

- rep_summary:

  argument passed to rep_summary

- shapes:

  argument passed to shapes

- fsize:

  argument passed to fsize

- gg:

  argument passed to gg

- stats:

  argument passed to stats

- stats_test:

  argument passed to stats_test

- ...:

  additional arguments

## Value

none
