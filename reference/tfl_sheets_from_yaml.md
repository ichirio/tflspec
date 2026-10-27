# The YAML spec as sheets

`tfl_sheets_from_yaml()` reads a study's YAML spec and gives the sheets
the workbooks held: for the app's grids and for the generators that
still read sheets. The study's defaults are the rows with a blank
`output_id`, a pattern's the `@<name>` rows, a report's its own rows
(the files as written, not layered – the sheets' readers layer them as
they always did). `tfl_sheets_from_spec()` does the same from a spec
already read (or changed in memory, not written yet):
`tfl_sheets_from_yaml(dir)` is
`tfl_sheets_from_spec(tfl_read_study_spec(dir), tfl_read_outputs(dir))`.

## Usage

``` r
tfl_sheets_from_yaml(dir)

tfl_sheets_from_spec(study, outputs)
```

## Arguments

- dir:

  The study folder (or its `spec/`).

- study:

  The study's spec, as
  [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  reads it.

- outputs:

  The reports, as
  [`tfl_read_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  reads them: a list of `tfl_output` (as written, not resolved), named
  by id.

## Value

A list: `table` (the table and report sheets, with `study`), `planner`
(the `_tflplanner` sheet: the report list and the setup), `ard` (the ARD
sheets), `listing` (`listings`, `listing_cols`), `figures` (a figure's
design list, by id), `study` (study.yml's fields), `not_converted` (what
the sheets cannot hold).

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
sh <- tfl_sheets_from_yaml(d)
sh$planner$output_id
#> [1] NA         "T-14-1-1"
# the same from the spec in memory, a title changed and not written
s <- tfl_read_study_spec(d)
outs <- tfl_read_outputs(d)
outs[["T-14-1-1"]]$label <- "Demographics (draft)"
tfl_sheets_from_spec(s, outs)$planner$description
#> [1] "(every report)"       "Demographics (draft)"
```
