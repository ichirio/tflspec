# Write a plan as a table definition workbook

`tfl_as_table_spec()` turns an
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
— typically one a report already has as code — into a
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
the definition
[`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md)
writes as an Excel workbook. It is how an existing report becomes the
**template for a new study**: write the workbook, edit its labels,
levels and output ids, and read it back with
`tfl_table_plan(data, tfl_read_table_spec(path, output_id = ))`.

Everything is read from what the plan **resolves to**, against its own
data: the roles, levels, labels and cell templates (digits written in),
the pages, groups, blank rows and stub, the column widths **by name**
(`.values` when every value column shares one), and the column header —
with a literal that is a column's own key value turned back into `{col}`
/ `{col1}`, a repeated per-column cell into `span = each`, and one
spanner per arm into `span = <key>`, so the header keeps up with a study
that has a different number of arms or time points.

What a workbook cannot say is **listed, not dropped**: a `plan_after()`
step (except `set_decimal_split()`, which becomes
`columns$decimal_split` on the value columns), a guarded label, a
column-scoped `labels` entry, `plan_cell_style()`, a literal `n`. The
result is then run back through
[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
on the plan's data, and whether it gives **the same pages** is reported.

## Usage

``` r
tfl_as_table_spec(x, output_id = NULL, compare = TRUE)
```

## Arguments

- x:

  An
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html),
  a **named list** of them (the names are the output ids; one workbook
  for the study), or anything
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
  takes.

- output_id:

  The report the rows belong to. `NULL` writes them as defaults (blank
  `output_id`).

- compare:

  `TRUE` (default) rebuilds the pages from the workbook and compares
  their RTF, byte by byte, with the plan's (the titles and footnotes
  aside: they are the report's). The RTF is what a reader gets: two page
  objects that differ in how they hold the same output compare as the
  same.

## Value

A
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
with attributes `"not_converted"` (what the workbook could not carry)
and `"same_pages"` (`TRUE` / `FALSE`: the same RTF, or `NA` when not
compared).

## Lifecycle

**Spike.** See
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## See also

[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
[`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md),
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)

## Examples
