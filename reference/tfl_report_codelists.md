# A report's code lists, with the ones its layers give

`tfl_report_codelists()` gives the code lists the ARD program of each
report is written with. A report's code lists can come from the layers
it extends – the study's `defaults.yml`, a pattern – as well as from its
own file
([`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)
puts them together for the RTF); the sheets
([`tfl_sheets_from_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_sheets_from_yaml.md))
hold a report's own only, so that they stay the round trip back to the
YAML. This is the sheets' `codelists` rows with the layers' lists added,
report by report.

## Usage

``` r
tfl_report_codelists(study, outputs = NULL, rows = NULL)
```

## Arguments

- study:

  The study's spec, as
  [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  reads it.

- outputs:

  The reports, as
  [`tfl_sheets_from_spec()`](https://ichirio.github.io/tflspec/reference/tfl_sheets_from_yaml.md)
  takes them: a list of `tfl_output` named by id. `NULL` (default): the
  study's own files, read as
  [`tfl_sheets_from_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_sheets_from_yaml.md)
  reads them.

- rows:

  The reports' own code list rows (the sheets' `codelists`, a template's
  lists among them), when the caller has them: the sheets are then not
  made again. The result keeps the columns of `rows` (a layer's rows
  have `NA` there), and a row of a report not in `outputs` stays. `NULL`
  (default): made from `outputs`.

## Value

A data frame of the sheets' `codelists` rows (`output_id`, `variable`,
`value`, `label`, `order`, `under`), the reports in the TOC's order;
every row has its report (no `NA` `output_id`), and zero rows when no
report has a list.
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)
and
[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
write a study's ARD with these.

## Details

For each report and variable there is one list: the report's own when it
has one (a template's own lists among them, as the sheets hold them),
else the one of the layers it extends (a pattern's over the defaults').
A report that extends nothing (`extends: none`) has only its own.

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
tfl_report_codelists(tfl_read_study_spec(d))
#>   output_id variable   value          label order under
#> 1  T-14-1-1   TRT01A  Drug X  Drug X 200 mg     1  <NA>
#> 2  T-14-1-1   TRT01A Control Control 100 mg     2  <NA>
```
