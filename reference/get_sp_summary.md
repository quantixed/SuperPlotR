# Get summary statistics for SuperPlot data

This function calculates summary statistics (mean and median) for each
combination of condition and replicate in a SuperPlot dataset.

## Usage

``` r
get_sp_summary(df, meas, cond, repl, ...)
```

## Arguments

- df:

  A data frame containing the experimental data

- meas:

  Character string specifying the name of the column containing the
  measurements/values to summarize (e.g., "intensity", "speed")

- cond:

  Character string specifying the name of the column containing the
  experimental conditions (e.g., "Treatment", "Genotype")

- repl:

  Character string specifying the name of the column containing the
  replicate identifiers (e.g., "Replicate", "Experiment")

- ...:

  Additional arguments (ignored)

## Value

A data frame with columns for condition, replicate, rep_mean (mean of
measurements), and rep_median (median of measurements)

## Details

It is useful to get teh summary data and computing further stats or
making plots. The function only needs 4 parameters, anything else is
ignored. This means you can simply exchange get_sp_summary() for your
superplot() call and get the correct data frame to work with.

## Examples

``` r
# Using the built-in dataset
get_sp_summary(lord_jcb, "Speed", "Treatment", "Replicate")
#> # A tibble: 6 × 4
#>   Treatment Replicate rep_mean rep_median
#>   <chr>         <int>    <dbl>      <dbl>
#> 1 Control           1     41.5       41.7
#> 2 Control           2     32.6       34.4
#> 3 Control           3     20.6       20.3
#> 4 Drug              1     29.6       30.1
#> 5 Drug              2     22.3       21.9
#> 6 Drug              3     12.9       12.6
```
