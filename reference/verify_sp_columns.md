# Verify the data frame used for SuperPlot

Verify the data frame used for SuperPlot

## Usage

``` r
verify_sp_columns(df, meas, cond, repl, facet = NULL)
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

  character name of column to facet by (e.g. further grouping variable,
  default is NULL)

## Value

logical to allow plot to go ahead
