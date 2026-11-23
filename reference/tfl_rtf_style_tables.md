# The tables of a folder of past RTF reports, one row each

Sums up
[`tfl_rtf_style_scan()`](https://ichirio.github.io/tflspec/reference/tfl_rtf_style_scan.md)'s
rows a table, for a person (or an AI agent) who writes a company
standard from what past reports do. SAS's ODS RTF is read as it is
written: the titles and footnotes in each page's `\header` / `\footer`
(or as paragraphs before and after the table), a page a section, the
column header rows (`\trhdr`) repeated on each page and counted once.
rtfreporter's RTF is read too: its page header's line "Page 1 of 1" (the
page number written out, not as a field) is the page-number line, not a
title, so the titles are the paragraphs before the table; a footnote
written as a one-cell row after the table is a footnote, not a table.

## Usage

``` r
tfl_rtf_style_tables(rtf_dir)
```

## Arguments

- rtf_dir:

  The folder of RTF files (a subfolder a study).

## Value

A data frame, one row a table: `file`, `table`, `pages`, `kind` (a guess
from the title: `demog`, `ae_soc_pt`, `ae`, `param_change`, `shift`,
`listing`, `figure`, `other`), `title` (its title lines, `/` between
them), `header_rows`, `body_rows`, `n_cols`, `rel_widths` (the last
header row's cells, as parts of the narrowest), `stub_ratio` (the first
column's width to the median of the others'), `rules` (where lines are
drawn: `above_header`, `under_spanner`, `under_header`, `body`, `end`,
`|` between), `font`, `font_size_pt` (the body's), `stub_align`,
`value_align`, `orientation`, `margins_in` (left, right, top, bottom),
`title_place` and `footnote_place` (`header` / `footer`, the page's;
`before` / `after`, paragraphs in the body; `none`), `title_align`,
`footnote_align`, `footnotes` (their number).

## See also

[`tfl_rtf_style_scan()`](https://ichirio.github.io/tflspec/reference/tfl_rtf_style_scan.md)

## Examples

``` r
d <- tempfile()
dir.create(d)
writeLines(c("{\\rtf1\\ansi{\\fonttbl{\\f0 Courier New;}}",
  "{\\header\\pard\\qc Table 14.1.1\\par\\pard\\qc Demographic Characteristics\\par}",
  paste0("\\trowd\\trhdr\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\cellx3000",
         "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\cellx4500"),
  "\\pard\\intbl\\ql \\cell\\pard\\intbl\\qc Drug X\\cell\\row",
  "\\trowd\\clbrdrb\\brdrs\\cellx3000\\clbrdrb\\brdrs\\cellx4500",
  "\\pard\\intbl\\ql Age\\cell\\pard\\intbl\\qc 54\\cell\\row}"),
  file.path(d, "t.rtf"))
tfl_rtf_style_tables(d)[, c("kind", "header_rows", "stub_ratio", "rules")]
#>    kind header_rows stub_ratio                             rules
#> 1 demog           1          2 above_header | under_header | end
```
