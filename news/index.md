# Changelog

## SuperPlotR 0.2.0

- SuperPlotting is now done using
  [`superplot_spec()`](https://quantixed.github.io/SuperPlotR/reference/superplot_spec.md)
  behind the scenes, which allows for greater flexibility in the future.
- Better handling of colour and shape aesthetics, including the ability
  to specify them in the
  [`superplot()`](https://quantixed.github.io/SuperPlotR/reference/superplot.md)
  function.

## SuperPlotR 0.1.2

- Ability to facet superplots
- Added text description of colours used in information output
- Documentation improvements

## SuperPlotR 0.1.1

- Use of mean ± sd bars is now the default
- Minor changes to colour handling and documentation

## SuperPlotR 0.1.0

- Test structure added. Bumping to 0.1.0 to reflect this change.

## SuperPlotR 0.0.8

- Added
  [`pieplot()`](https://quantixed.github.io/SuperPlotR/reference/pieplot.md)
  to conveniently make pie charts with ggplot2

## SuperPlotR 0.0.7

- Added ability to find representative datapoints with
  [`representative()`](https://quantixed.github.io/SuperPlotR/reference/representative.md)
- Added ability to retrieve information about superplot using the option
  `info = TRUE`. User is prompted to do this if there are unequal
  numbers of replicates in each condition
- Added
  [`get_sp_summary()`](https://quantixed.github.io/SuperPlotR/reference/get_sp_summary.md)
  to retrieve the data frame that is overlaid on the superplot

## SuperPlotR 0.0.6

- Fixed factoring bug.

## SuperPlotR 0.0.5

- Added `flatplot` function for simple plots.

## SuperPlotR 0.0.4

- Added simple statistical testing for the `superplot` function.

## SuperPlotR 0.0.3

- Improved vignettes.
- Better error handling.

## SuperPlotR 0.0.2

- Vignettes added, bugs squashed.
- Logo added. Ready for pkgdown.

## SuperPlotR 0.0.1

- Initial working version.
- Documentation and examples to be added or improved.
