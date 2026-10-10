# Build a report's RTF document from its definition

`tfl_report()` is the document half of a report definition: the page,
the running header and footer, the titles and footnotes, and the table's
pages — an
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
or anything
[`rtfreporter::rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.html)
takes — in one
[`rtfreporter::rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.html)
ready for
[`rtfreporter::generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.html).
The document carries its program, so `{PROGRAM}` needs nothing more.

## Usage

``` r
tfl_report(spec, output_id = NULL, content)
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  narrowed to one report, or the path(s) to read it from.

- output_id:

  The report, when `spec` is a path or still defines several.

- content:

  The report's content: an
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  or `rtftable` pages for a table or listing, figures for a
  `type = figure` report.

## Value

An
[`rtfreporter::rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.html).

## Details

    spec <- tfl_read_report_spec(c("report.xlsx", "tables.xlsx"), output_id = id)
    plan <- ard |> normalize_ard() |> tfl_table_plan(spec)
    generate_rtfreport(tfl_report(spec, content = plan), tfl_report_path(spec),
                       overwrite = TRUE)

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md),
[`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)
