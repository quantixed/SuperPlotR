# Limitations to SuperPlotR

## Current limitations

There are some limitations to SuperPlotR. This vignette covers them and
offers solutions where possible.

### Conditions and Replicates are categories

If you have a dataset where the conditions and replicates are numeric,
SuperPlotR converts them to character for plotting. This is the intended
behaviour for a SuperPlot.

Currently, if your data has Conditions that are numeric, for example
time points, and these are unevenly spaced, e.g. 0, 1, 3, 9 hours; then
these will appear evenly spaced on the x-axis. This is a limitation of
the current implementation. Raise an issue if you would like this to be
changed.

### Adding annotations directly to the SuperPlot

Adding labels or statistical testing results is not currently supported.

Such annotations need to be added manually either in R or post-hoc in
Inkscape or similar vector-based drawing program.
