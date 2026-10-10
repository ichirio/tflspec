# Write generated code to `.R` files

Write generated code to `.R` files

## Usage

``` r
tfl_write_fig_code(spec, dir, plot_id = NULL, adam = NULL)
```

## Arguments

- spec:

  A spec from
  [`tfl_read_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_fig_spec.md)
  or
  [`tfl_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_fig_spec.md).

- dir:

  Output folder; files are named `<plot_id>.R`.

- plot_id:

  Plot id(s); `NULL` = all plots.

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)).
  When given, codelist values (colours, factor levels, legend items) are
  resolved from the data and written literally into the code; variable
  labels become axis labels.

## Value

File paths, invisibly.
