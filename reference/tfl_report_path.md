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

## Details

The file is a pattern. Beside `{output_id}`, the parts of the report's
number fill it: `{type}` (`t`, `f`, `l`), `{num}` (`15.1.5.1`),
`{num:N}` (its first N levels joined: `{num:2}` is `151`), `{part}`
(`a`), `{part_no}` (`1`); and `{shell_code}`, the id written at the top
of the report's shell (its TOC entry's `shell_code`, which
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
reads: a cell of the shell's first rows in the form `t_dem_01` or
`151_t_dem_01`, or `study.yml`'s `settings: shell_code`, a regular
expression). A part's entry is its number's. `[ ... ]` is left out when
a field in it is empty: `[m{part_no}_]{shell_code}.rtf` is
`m1_151_t_dem_01.rtf` for part a, `151_t_dem_01.rtf` for a report with
no parts.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
