# Read a drafting answer

Reads the text a chat model returned for a drafting prompt
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md)):
the first fenced block (```` ```yaml ```` or ```` ```json ````) that can
be read and starts with the `tflspec_ai` header; the whole text when no
block does. YAML and JSON are both read, whichever the prompt asked for.

## Usage

``` r
tfl_ai_parse(text, task)
```

## Arguments

- text:

  The answer: a string, or lines of text.

- task:

  One of
  [`tfl_ai_tasks()`](https://ichirio.github.io/tflspec/reference/tfl_ai_tasks.md):
  the task the prompt asked for.

## Value

A `tfl_ai_answer`: a list with `task`, `header` (the `tflspec_ai`
mapping), `output_id`, `sheets` (`toc`: a list holding the `toc` data
frame) or `design` (`figure`), `note`, `assumptions` (character),
`problems` (a data frame `where`, `problem`, `severity` – `error` or
`warning`), `format` (`yaml` or `json`) and `raw` (the block's text).
`sheets` / `design` is `NULL` when the answer could not be read.

## Details

Nothing is repaired: a block YAML cannot read (a `{mean}` not quoted, a
tab) is a problem naming the parser's line, as is a header of another
task or version, a column or key the task does not have, a mapping where
a value is wanted, or a block that is not closed (an answer cut off).
The problems go to
[`tfl_ai_repair_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_repair_prompt.md).

- `toc`: the rows become a data frame with
  [`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)'s
  fields as columns, every value text (numbers as text, a list joined
  with `" | "`, missing `NA`).

- `table`: each report of `outputs` is read as
  [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  reads a report file, into `outputs` (a list of `tfl_output` by id); a
  key a report file does not have is a problem. The assumptions (text,
  or mappings with `output_id`, `where`, `item`, `assumption`,
  `question`) are kept as rows too, in `assumption_rows`.

- `figure`: the design becomes a
  [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  (the 0.0.12 shape `type` / `style` / `args` is read as one `figure`
  layer, and a `stats` list of before 0.0.24.9074 after the `data`
  steps, each with a warning); a `note` key is kept as the answer's
  `note`.

## See also

[`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md)
for the checks that need the study's context.

## Examples

``` r
answer <- c(
  "Here is the TOC:",
  "```yaml",
  "tflspec_ai: {task: toc, version: 1}",
  "toc:",
  "- {output_id: T-14-1-1, type: table, title: Demographics,",
  "   population: Safety Population}",
  "- {output_id: F-14-2-1, type: figure, title: Time to Death}",
  "assumptions:",
  "- No footnotes are given in the SAP.",
  "```")
a <- tfl_ai_parse(answer, "toc")
a$sheets$toc[c("output_id", "type", "title")]
#>   output_id   type         title
#> 1  T-14-1-1  table  Demographics
#> 2  F-14-2-1 figure Time to Death
a$assumptions
#> [1] "No footnotes are given in the SAP."
```
