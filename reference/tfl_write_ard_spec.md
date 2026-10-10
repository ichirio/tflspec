# Write an ARD definition to a workbook

Writes the five sheets (`study`, `datasets`, `populations`,
`analysis_data`, `analyses`) and no more; what each column means is a
comment on its header cell
([`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md)).
[`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
takes one back. With other specs in one workbook:
[`tfl_write_specs()`](https://ichirio.github.io/tflspec/reference/tfl_write_specs.md).

## Usage

``` r
tfl_write_ard_spec(
  spec,
  path,
  statistics = NULL,
  methods = NULL,
  catalogs = FALSE
)
```

## Arguments

- spec:

  An ARD definition: an
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md),
  or a list of the sheets.

- path:

  The workbook to write.

- statistics, methods:

  The catalogs
  ([`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md),
  [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md))
  to use instead of the current ones.

- catalogs:

  `TRUE` also writes the catalogs the spec is checked against, as the
  sheets `_methods` and `_statistics` (for reference: the reader passes
  over sheets whose name starts with `_`).

## Value

`path`, invisibly.
