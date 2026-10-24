# Read an ARD table definition from a workbook

Read an ARD table definition from a workbook

## Usage

``` r
tfl_read_table_spec(path, output_id = NULL)
```

## Arguments

- path:

  An `.xlsx` workbook (needs readxl) with the sheets of
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
  (`study`, `tables`, `variables`, `cells`, `layout`, `columns`,
  `style`, `col_header`). Any of them may be absent. A definition is one
  file with several sheets, so it is an Excel workbook and nothing else.

- output_id:

  The report to narrow the workbook to. Rows with a blank `output_id`
  are the study's defaults and stay; a row naming this report replaces
  the default with the same key. `NULL` (default) reads the whole
  workbook; what needs one report —
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html),
  [`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
  — then takes a workbook of one report as it is and asks which of
  several.

## Value

An
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md).

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
[`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md)
