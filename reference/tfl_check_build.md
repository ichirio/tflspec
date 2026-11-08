# Build each report, to know that it can be made

The step after
[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)'s
reading: each report's data read, its ARD made, its table, listing or
figure object made – all a program does but writing the RTF – in the
study folder. A report that cannot be made gives an `error` row, `part`
`"build"`, with the message the build stopped on; one that can gives no
row. The person's bar for OK (2026-10-30): the report's object can be
made. A few seconds a report; the AI's drafts apart
(`options(tflspec.outputs_dir = )`) are read as
[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)
reads them.

## Usage

``` r
tfl_check_build(study, output_id = NULL)
```

## Arguments

- study:

  A study read with
  [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md),
  or its folder.

- output_id:

  The reports to check; `NULL` for every report.

## Value

A data frame as
[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)'s
(`output_id`, `part`, `item`, `level`, `message`): the reports that
could not be built. Attributes `ids` (the reports built or tried) and
`kinds`.

## See also

[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)
(`build = TRUE` does both),
[`tfl_run_output()`](https://ichirio.github.io/tflspec/reference/tfl_run_output.md)
