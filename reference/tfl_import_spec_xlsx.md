# Take an edited flat-view workbook back into the YAML spec

The workbook
[`tfl_write_spec_xlsx()`](https://ichirio.github.io/tflspec/reference/tfl_write_spec_xlsx.md)
wrote, edited in Excel, read back: each row is matched by its key
(`toc`: `id`; `datasets`: `dataset`; `populations`: `id`;
`analysis_data` and `analyses`: `output_id` + `id`; `codelists`:
`output_id` + `variable` + `value`; `tokens`: `output_id` + `name`;
`header` / `footer` / `titles` / `footnotes`: `output_id` + `line`;
`page` / `report`: `output_id`; `listing_cols`: `output_id` + `vars`)
and the item it names is updated with the row's cells; a row with a new
key is added; an item the workbook no longer has is reported
(`missing`), not removed. Cells read as the converter reads sheets:
`a | b` a list, `(none)` an `!unset`, the columns that hold R as `!r`.
Nothing else of the YAML files changes.

## Usage

``` r
tfl_import_spec_xlsx(dir, path, write = TRUE)
```

## Arguments

- dir:

  The study folder (or its `spec/`).

- path:

  The workbook.

- write:

  Write the changed YAML files? `FALSE` only reports.

## Value

Invisibly, a list: `changes` (a data frame: `sheet`, `key`, `column`,
`from`, `to`), `added` (keys new to the spec), `missing` (keys of the
spec the workbook lacks), `files` (written).
