# Where a report's RTF file goes

`file.path(study$output_path, report$file)`, with `{output_id}` filled:
the path
[`rtfreporter::generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.html)
writes to.

## Usage

``` r
tfl_report_path(spec, output_id = NULL)
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  narrowed to one report, or the path(s) to read it from.

- output_id:

  The report, when `spec` is a path or still defines several.

## Value

A single path.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
