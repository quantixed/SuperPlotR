# Get statistical test results for the plotted data

Get statistical test results for the plotted data

## Usage

``` r
get_sp_stats(df, rep_summary, cond, repl, ncond, nrepl, stats_test)
```

## Arguments

- df:

  data frame containing the data

- rep_summary:

  summary statistic to use for testing

- cond:

  column name for the condition

- repl:

  column name for the replicate

- ncond:

  number of unique values in cond

- nrepl:

  number of unique values in repl

- stats_test:

  string for statistical test to use, select ("para_unpaired",
  "para_paired", "nonpara_unpaired", or "nonpara_paired")

## Value

nothing, prints results to console
