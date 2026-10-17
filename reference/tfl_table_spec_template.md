# Scaffold a definition workbook from an ARD

Walks the ARD and writes the three
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
sheets: one `tables` row, one `variables` row per analysis variable (its
levels filled in for a categorical one), and one `cells` row per row
template — a continuous variable gets the templates its statistics can
fill, a categorical one `{n} ({p})`. Edit the labels, templates and
digits, and hand it back through `spec =`.

## Usage

``` r
tfl_table_spec_template(ard, path = NULL, cols = NULL, output_id = NULL)
```

## Arguments

- ard:

  A cards/cardx ARD.

- path:

  Optional destination; when given the spec is also written there with
  [`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md).

- cols:

  The column keys for the `tables` row, if known.

- output_id:

  The report the rows belong to; `NULL` writes them as defaults.

## Value

An
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
invisibly when `path` is given.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
`plan_template(form = "spread")`
