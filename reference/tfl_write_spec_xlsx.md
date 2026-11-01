# Write the flat view of a study's YAML spec as a workbook

Reads the study's YAML spec
([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md),
[`tfl_read_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md);
the files as written, not layered) and writes the parts that are rows to
`path`: `toc`, `datasets`, `populations`, `analysis_data`, `analyses`,
`codelists`, `tokens`, `header`, `footer`, `titles`, `footnotes`,
`page`, `report`, `listing_cols`, and `_not_in_xlsx` (what the workbook
does not hold: `content:`, `!r` values, nested calls, a figure's plot).
Lists are written `a | b`; R text as it is (the columns that hold R are
known). The workbook is a view for Excel; an import back changes only
these rows, by their keys.

## Usage

``` r
tfl_write_spec_xlsx(dir, path)
```

## Arguments

- dir:

  The study folder (or its `spec/`).

- path:

  The workbook to write.

## Value

Invisibly, a list: `path`, `sheets` (the names written), `not_in_xlsx`
(a data frame: `output`, `part`, `reason`).
