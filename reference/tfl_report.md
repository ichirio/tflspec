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
The document carries its program, so `{PROGRAM}` needs nothing more, and
its file
([`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md):
`rtf_document(file = )`), so `generate_rtfreport(doc)` writes there; a
path given to it wins. A YAML report's file needs its `study`.

## Usage

``` r
tfl_report(spec, output_id = NULL, content, study = NULL)
```

## Arguments

- spec:

  A report definition
  ([`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
  narrowed to one report, or the path(s) to read it from; or a report of
  the YAML spec
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md),
  resolved:
  [`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)),
  its `page:` the calls – `rtf_page`, `rtf_default_format`,
  `rtf_document`, `rtf_tables` / `rtf_figures`, `rtf_titles`,
  `rtf_footnotes`, each its arguments, and the lines of `header`,
  `footer`, `titles`, `footnotes` and the `tokens`.

- output_id:

  The report, when `spec` is a path or still defines several.

- content:

  The report's content: an
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  or `rtftable` pages for a table or listing, figures for a
  `type = figure` report.

- study:

  For a YAML report: the study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)),
  for its program (the TOC's `program`, else `settings: program`, in
  `settings: program_dir`), and where the report's titles and footnotes
  print: `settings: titles_at` `header` (the running header, after the
  study's lines) or `titles` (above the table), `footnotes_at` `footer`
  (the running footer, before the study's lines) or `footnotes` (below
  the table); not set, above and below the table.

## Value

An
[`rtfreporter::rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.html).

## Details

    spec <- tfl_read_report_spec(c("report.xlsx", "tables.xlsx"), output_id = id)
    plan <- ard |> normalize_ard() |> tfl_table_plan(spec)
    generate_rtfreport(tfl_report(spec, content = plan))   # to tfl_report_path(spec)

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md),
[`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)
