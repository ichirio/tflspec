# Write a table or report definition to a workbook

Each writer writes the sheets its kind of spec needs and no more:
`tfl_write_table_spec()` the table sheets (`tables`, `variables`,
`cells`, `layout`, `columns`, `style`, `col_header`),
`tfl_write_report_spec()` the report sheets (`report`, `page`, `header`,
`footer`, `titles`, `footnotes`, `tokens`), each with the `study` sheet
(showing the keys that kind reads: `rounding`; `output_path`,
`program_dir`) and an `about` sheet stating `spec_version`. Its own
sheets are written even when empty, so their columns are there to fill
in; a sheet of the other half is written only when the spec has rows in
it, so nothing is dropped. What each column means is a comment on its
header cell
([`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md)).
Both halves in one workbook:
[`tfl_write_specs()`](https://ichirio.github.io/tflspec/reference/tfl_write_specs.md).

## Usage

``` r
tfl_write_report_spec(spec, path)

tfl_write_table_spec(spec, path)
```

## Arguments

- spec:

  An
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
  (or what it accepts).

- path:

  Destination `.xlsx`.

## Value

`path`, invisibly.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)
