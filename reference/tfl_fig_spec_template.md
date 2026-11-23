# Write an Excel spec template with drop-down lists built from ADaM data

Datasets, variables and codelist values offered in the drop-downs come
from `adam`. Variables that depend on a dataset pick the list of that
dataset (the row's `dataset`, or the plot's `dataset` when blank).

## Usage

``` r
tfl_fig_spec_template(adam, path, spec = NULL, max_levels = 50, n_rows = 300)
```

## Arguments

- adam:

  Result of
  [`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)
  (or a named list of data frames).

- path:

  Output `.xlsx` path.

- spec:

  Optional spec (list of data frames, e.g. from
  [`tfl_read_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_fig_spec.md))
  whose rows are pre-filled.

- max_levels:

  Variables with more distinct values than this are not offered as
  codelists.

- n_rows:

  Rows prepared with drop-downs in each sheet.

## Value

`path`, invisibly.
