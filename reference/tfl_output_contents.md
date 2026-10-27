# The objects of a report with several: each built

A report whose `contents:` names several objects (one RTF, its tables
and figures on its pages): each object run as a report of its own – a
table's plan from the report's ARD
([`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md),
[`tfl_content_run()`](https://ichirio.github.io/tflspec/reference/tfl_content_run.md)),
a figure's plot from its data steps. What
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
takes as its `content` for such a report.

## Usage

``` r
tfl_output_contents(output, study, dir = ".")
```

## Arguments

- output:

  The report
  ([`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)),
  with `contents:`.

- study:

  The study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)).

- dir:

  The study folder (the ARD and the data are read from it).

## Value

A named list, one element an object (its name in `contents:`): a table's
plan, a figure's plot.

## See also

[`tfl_run_output()`](https://ichirio.github.io/tflspec/reference/tfl_run_output.md)
