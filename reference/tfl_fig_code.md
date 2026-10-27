# Generate ggplot2 code from a plot spec

Generate ggplot2 code from a plot spec

## Usage

``` r
tfl_fig_code(spec, plot_id = NULL, adam = NULL)
```

## Arguments

- spec:

  A spec from
  [`tfl_read_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_fig_spec.md)
  or
  [`tfl_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_fig_spec.md).

- plot_id:

  Plot id(s); `NULL` = all plots.

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)).
  When given, codelist values (colours, factor levels, legend items) are
  resolved from the data and written literally into the code; variable
  labels become axis labels.

## Value

A named character vector of R scripts (one per plot).
