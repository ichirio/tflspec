# The check's three levels, a report at a time

[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)'s
rows summed up by report: its level – `ERROR` (it cannot be copied into
the study), `WARNING` (its program cannot be written), `OK` (its code is
written) – with the counts behind it, and apart from the level, what the
AI marked to confirm
([`tfl_ai_confirm()`](https://ichirio.github.io/tflspec/reference/tfl_ai_confirm.md)).
A problem of the study's ARD definition as a whole (no report) counts
for every report.

## Usage

``` r
tfl_check_reports(problems)
```

## Arguments

- problems:

  [`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md)'s
  result.

## Value

A data frame, one row a report: `output_id`, `level` (`"ERROR"`,
`"WARNING"`, `"OK"`), `errors`, `warnings`, `notes` (the counts),
`confirms` (how many marked to confirm) and `confirm` (their texts, `; `
between them; `NA` for none).

## See also

[`tfl_check_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_spec.md),
[`tfl_check_fix_text()`](https://ichirio.github.io/tflspec/reference/tfl_check_fix_text.md)
