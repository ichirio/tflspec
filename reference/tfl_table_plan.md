# A table's plan from its definition

Builds the
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
a table definition stands for: the roles of its `tables` sheet go into
`table_plan()`, and the other sheets become the plan's first layers
through the same verbs
[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md)
writes — so a verb written afterwards still wins, which is how one
report departs from the study's workbook in a line of code. Like
`table_plan()`, the data comes first, so it pipes.

## Usage

``` r
tfl_table_plan(data, spec, output_id = NULL, ...)
```

## Arguments

- data:

  The normalized ARD, as
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  takes it.

- spec:

  A table definition
  ([`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md),
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)),
  or the path(s) of its workbook(s).

- output_id:

  The table, when the definition has several.

- ...:

  Roles for
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  (`cols`, `rows`, `label`, `stat`); one given here wins over the
  workbook's. Everything else is a layer: add its verb after this call.

## Value

An
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## See also

[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md),
the same steps as code;
[`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md),
the other way round.

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- tfl_read_table_spec("study.xlsx", output_id = "DM")
plan <- ard |> normalize_ard() |> tfl_table_plan(spec)
plan |> plan_paginate_rows(max_rows = 40)   # this report's own change
} # }
```
