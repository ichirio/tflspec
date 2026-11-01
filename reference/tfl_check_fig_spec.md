# Check a plot spec against ADaM data

Reports unknown plot types / layers / presets, missing required roles,
variables not in the datasets and codelist values not in the data.

## Usage

``` r
tfl_check_fig_spec(spec, adam = NULL)
```

## Arguments

- spec:

  A spec from
  [`tfl_read_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_fig_spec.md)
  or
  [`tfl_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_fig_spec.md).

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)).
  When given, codelist values (colours, factor levels, legend items) are
  resolved from the data and written literally into the code; variable
  labels become axis labels.

## Value

A data frame of issues (`plot_id`, `sheet`, `message`), invisibly.
Issues are also printed.
