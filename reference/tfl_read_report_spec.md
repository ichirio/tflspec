# Read a report definition: the table and everything around it

A **report definition** is the
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
sheets plus the ones that say how a report's pages are dressed:

|  |  |  |
|----|----|----|
| sheet | one row per | holds |
| `report` | report | `type`, `file`, `program`, `auto_section`, font sizes |
| `page` | report | paper, orientation, margins, the document's text defaults |
| `header` | line of the page header | `line`, `left`, `center`, `right` |
| `footer` | line of the page footer | the same |
| `titles` | line above the table | the same |
| `footnotes` | line below the table | the same |
| `tokens` | token of one's own | `name`, `value`: `{STUDY}` in any cell |

plus `study` keys `output_path` (where the RTF files go) and
`program_dir` (where the programs are, for `{PROGRAM}`).

The one `output_id` rule applies, and on the line sheets **the line is
the key**: the study's page header is written once, as default lines 1
and 2, a report's titles follow on its own lines 3, 4, ..., and a
run-information line 99 in the default footer
(`{PROGRAM} Generated on: {DATETIME}`) closes every report's footnotes.
A line with no text is a blank line; a report's line that says `(none)`
(in any of its cells) takes the default line of that number out instead
– the run line a report does without. The page tokens (`{PAGE}`,
`{TOTAL_PAGES}`, ...) and the run tokens
([`rtfreporter::generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.html))
work in every cell.

The `tokens` sheet names tokens of one's own: `name` `STUDY`, `value`
`ABC-123`, and a header, footer, title or footnote says `{STUDY}`
([`rtfreporter::rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.html)'s
`tokens`). A blank `output_id` is the study's default; a report's row of
the same name replaces it, and one that says `(none)` takes it out. A
name is upper case – a letter, then letters, digits or `_` – and not one
of rtfreporter's own tokens.

The sheets may be in **one workbook or several** — a `report.xlsx` a
lead keeps (the list of outputs, titles, footnotes) and a `tables.xlsx`
the programmers keep: give `path` as a vector and the sheets are read
together. A sheet found in two of them is an error.

## Usage

``` r
tfl_read_report_spec(path, output_id = NULL)
```

## Arguments

- path:

  One or more `.xlsx` workbooks.

- output_id:

  The report to narrow the definition to.

## Value

A
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
carrying the report sheets as well; it serves
[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
and
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
alike.

## `report`

`type` (`table`, `listing`, `figure`; default `table`), `file` (default
`{output_id}.rtf`), `program` (joined to `study$program_dir` for
`{PROGRAM}`; blank: the file name of the program that runs, and
`{output_id}` only when none is found – rtfreporter's
`program_fallback`), `auto_section`, `section_align`, `auto_title`,
`title_align` (as
[`rtfreporter::rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.html)
takes them), and `table_font_size_half_points`,
`title_font_size_half_points`, `footnote_font_size_half_points`, and
`page_header` / `page_footer` (`FALSE` drops that running band for the
report, the study's default lines included), `watermark` (a word drawn
behind every page, `rtf_document(watermark = )`: `DRAFT`), and
`figure_width_in` / `figure_height_in` (a figure report's figure size in
inches,
[`rtfreporter::rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.html)).

## `page`

`paper_size`, `orientation`, `width_in`, `height_in`, `margin_top_in`,
`margin_bottom_in`, `margin_left_in`, `margin_right_in`,
`header_dist_in`, `footer_dist_in` (the page, as
[`rtfreporter::rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.html)
takes it), `font` (the document's font: `rtf_document(font_table = )`),
and `font_size_half_points`, `title_format`, `footnote_format`,
`title_width`, `footnote_width`, `markup`
([`rtfreporter::rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.html)).
The study's row (blank `output_id`) of `font` and
`font_size_half_points` is the company's:
[`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md)
writes it once as
`options(rtfreporter.font = , rtfreporter.font_size_half_points = )`.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md),
[`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md),
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)
