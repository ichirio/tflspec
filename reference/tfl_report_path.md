# Where a report's RTF file goes

`file.path(study$output_path, report$file)`, with `{output_id}` filled:
the path
[`rtfreporter::generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.html)
writes to. For a report of the YAML spec
([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)):
its own `file` (its `settings: file`, else its TOC entry's), else
`study.yml`'s `settings: file` (`"{output_id}.rtf"` when none says it),
in `settings: output_path` as it is written (a relative one from the
working directory).

## Usage

``` r
tfl_report_path(spec, output_id = NULL, study = NULL)
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  narrowed to one report, or the path(s) to read it from; or a report of
  the YAML spec
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)).

- output_id:

  The report, when `spec` is a path or still defines several.

- study:

  For a YAML report: the study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)),
  for its settings and TOC.

## Value

A single path.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
