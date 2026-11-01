# Read an AI agent's survey of a company's past TFLs

The survey
[`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)
asks the agent to write: the forms of the reports' look (lines, column
header, widths, fonts, alignment) and how many tables have each.

## Usage

``` r
tfl_ai_style_read(file = NULL)
```

## Arguments

- file:

  The survey (YAML); `NULL`:
  [`tfl_ai_style_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_style_prompt.md)'s
  default.

## Value

A data frame, one row a form: `section` (`borders`, `column_header`,
`widths`, `fonts`, `titles`, `footnotes`), `kind` (a table kind, for
widths), `form`, `count`; in the order written. Its attribute `text` is
a short summary, a line a section (the commonest form and its count),
which [`print()`](https://rdrr.io/r/base/print.html) shows; `files` /
`tables` the survey's counts and `notes` its notes.
