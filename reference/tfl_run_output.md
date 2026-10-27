# Build a report's RTF from its YAML file

One line to make a report's RTF from the study's files: the report read
from `spec/outputs/`, its ARD built, its content run, its RTF written
where
[`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)
says. A report with several objects (`contents:`) has them on its pages
as its `page.layout:` says.

## Usage

``` r
tfl_run_output(output, dir = ".", overwrite = TRUE)
```

## Arguments

- output:

  A report's id, its YAML file, or a
  [`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md).

- dir:

  The study folder.

- overwrite:

  Write over an RTF already there.

## Value

The RTF's path, invisibly.

## See also

[`tfl_output_contents()`](https://ichirio.github.io/tflspec/reference/tfl_output_contents.md),
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
