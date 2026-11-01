# Whether a report's RTF file may be written over

What
[`rtfreporter::generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.html)'s
`overwrite` is for a report of the YAML spec: its page's
`generate_rtfreport: {overwrite: }` (the report's own, its pattern's or
the study's defaults), `TRUE` when none says it – a report's program
writes its file again each run.

## Usage

``` r
tfl_report_overwrite(output, study = NULL)
```

## Arguments

- output:

  A report of the YAML spec
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)).

- study:

  The study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)),
  for its layers.

## Value

`TRUE` or `FALSE`.

## See also

[`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
tfl_report_overwrite(tfl_read_outputs(d)[["T-14-1-1"]], tfl_read_study_spec(d))
#> [1] TRUE
```
