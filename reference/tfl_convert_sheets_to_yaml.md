# Convert a study's definition sheets to the YAML spec

Reads the definition files of a study folder as tflplanner wrote them –
`study.yml`, `spec/table_spec.xlsx`, `spec/report_spec.xlsx`,
`spec/ard_definition.json` (or `spec/ard_spec.xlsx`),
`spec/listing_figure_spec.xlsx`, `spec/figures/<id>.yml` – and writes
the YAML spec to `to`: `study.yml`, `spec/data.yml`,
`spec/defaults.yml`, `spec/codelists.yml`, `spec/patterns/<name>.yml`,
`spec/toc.yml` and one `spec/outputs/<id>.yml` a report. Loose files (a
table and a report workbook alone) convert too: `from` may be a folder
holding them.

## Usage

``` r
tfl_convert_sheets_to_yaml(
  from,
  to,
  content_fn = NULL,
  overwrite = FALSE,
  figures = c("design", "items")
)

tfl_yaml_from_sheets(
  sheets,
  to,
  content_fn = NULL,
  overwrite = FALSE,
  figures = c("design", "items")
)
```

## Arguments

- from:

  The study folder (or a folder of the definition files).

- to:

  The folder to write to (made if missing; `spec/` under it).

- content_fn:

  A function `(spec, output_id)` returning a list of
  [`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  for a table (`output_id = NA` for the defaults). `NULL`: the package's
  own bridge from the sheets to the layers
  (`.plan_content_from_sheets()`) when it is there, else none.

- overwrite:

  Write over files already in `to`?

- figures:

  How a figure is written: `"design"` (default) its designer's pieces as
  the content's items, as the figure designer reads them; `"items"` its
  content of function calls, plot by plot
  ([`tfl_fig_content_from_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_content_from_design.md):
  `ggplot`, `geom_line`, ..., `tfl_image`), the form the designer is
  moving to.

- sheets:

  The definition as objects, a list: `table` (a `tfl_table_spec`, or its
  sheets; the report sheets with them), `planner` (tflplanner's
  `_tflplanner` sheet: the report list and the setup code), `ard` (the
  ARD sheets), `listing` (`listings`, `listing_cols`), `figures` (a
  figure's design list, by id), `study` (study.yml's fields). What the
  app holds, or what the workbooks read as.

## Value

Invisibly, a list: `to`, `files` (paths written), `not_converted` (a
data frame: `output`, `sheet`, `row`, `column`, `reason`, `value`).

## Details

A table's `content:` (its layers) is made by
`content_fn(spec, output_id)` – the bridge from the sheets to the plan's
layers; without one, the content is left empty and said so in
`_not_converted.yml`. Cells that hold R (`where`, `derive`, `args`,
`post`, `code`, `when`) are kept as `!r` text and listed; `(none)`
becomes `!unset`.

Use, in R:“\`r r \<- tfl_convert_sheets_to_yaml("C:/studies/ABC-101",
"C:/studies/ABC-101-yaml") r\$files \# what was written r\$not_converted
\# what was not, or changed form (also \_not_converted.yml)
tfl_read_outputs(r\$to) \# the reports, read back
