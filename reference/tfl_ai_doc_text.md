# A document as Markdown text for the agent

An agent in the editor reads text; a SAP in Word or PDF, or a workbook,
reaches it as nothing. This turns one into Markdown: a `.docx`'s
paragraphs with its heading styles as `#` headings, its list paragraphs
as `-` items and its tables as Markdown tables (needs the officer
package); a `.pdf`'s text page by page, each page marked
`<!-- page n -->` (needs pdftools); a `.xlsx` / `.xls` sheet by sheet,
each sheet a heading and a Markdown table of its cells (readxl). A `.md`
or `.txt` is its own text.
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
calls it for `docs = ` and for the TOC / SHELL workbook the agent may
open, and names the text files in `AGENTS.md`.

## Usage

``` r
tfl_ai_doc_text(path, file = NULL)
```

## Arguments

- path:

  The document.

- file:

  A file to write the text to (UTF-8), or `NULL`.

## Value

The text, one string (Markdown), invisibly when written to `file`.

## Examples

``` r
if (requireNamespace("openxlsx", quietly = TRUE)) {
  xl <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Sets = data.frame(Set = c("SAF", "FAS"),
                                              Definition = c("treated", "randomized"))), xl)
  cat(tfl_ai_doc_text(xl))
}
#> ## Sets
#> 
#> | Set | Definition |
#> | --- | --- |
#> | SAF | treated |
#> | FAS | randomized |
```
