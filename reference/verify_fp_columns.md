# Verify the data frame used for FlatPlot

Verify the data frame used for FlatPlot

## Usage

``` r
verify_fp_columns(df, meas, cond)
```

## Arguments

- df:

  data frame with at least three columns: meas, cond, repl

- meas:

  character name of column with measurement (e.g. intensity)

- cond:

  character name of column with condition (e.g. Control, WT)

## Value

logical to allow plot to go ahead
