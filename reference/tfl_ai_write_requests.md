# Write a study's table requests, one file a request

Splits the TOC's tables into requests
([`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md))
and writes the prompt of each
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md))
to its own file, to paste into a chat as **one message**: no attachment
is needed. A request longer than `max_chars` is split again, until it
fits or holds one report. The requests' contexts are kept beside them
(`_contexts.rds`), so
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
checks each answer against its own request.

## Usage

``` r
tfl_ai_write_requests(
  study_dir,
  shell,
  adam,
  size = NULL,
  dir = file.path("ai", "requests"),
  max_chars = NULL,
  families = NULL,
  types = c("table", "figure"),
  language = NULL,
  examples = NULL,
  instructions = c("once", "each"),
  workbook = c("text", "attach", "both", "file"),
  overwrite = FALSE,
  add = FALSE,
  templates = TRUE,
  mode = c("chat", "agent"),
  progress = NULL,
  unit = c("sheet", "toc")
)
```

## Arguments

- study_dir:

  The study folder
  ([`tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.md)).

- shell:

  The TOC and shells
  ([`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)).

- adam:

  The ADaM summary
  ([`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md)).

- size:

  The reports a request holds at most (1 or more); `NULL` (the default):
  1 with `mode = "agent"` (each answer checked and built before the
  next), 8 in a chat.

- dir:

  The folder the requests go to, under `study_dir`.

- max_chars:

  A request's length at most, in characters; `NULL` for no limit. A
  request over it is halved; with `templates`, it first lists the
  templates' arguments by name only (no type, default or description).

- families, types:

  As
  [`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md):
  the tables in batches. A figure (`"figure"` in `types`, the default)
  is a request of its own, `figure-1`, `figure-2` ... (family `figure`):
  its shell, the ADaM's summary, the figure templates; its answer a
  figure design.

- language:

  As
  [`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md):
  the language of the AI's assumptions.

- examples:

  Reviewed report files for the AI to follow
  ([`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)'s
  `examples`). Named by family (`c(ae = "spec/outputs/T-14-3-1.yml")`),
  each goes only to its family's requests; unnamed, to every request.

- instructions:

  In a chat: `"once"` (default), the instructions in
  `00-instructions.md` (the chat's first message), the requests without
  them; `"each"`, every request holds them, one paste a request. With
  `mode = "agent"` either is the same: the instructions are written
  once, `ai/instructions-table.md` / `ai/instructions-figure.md`, and no
  request repeats them.

- workbook:

  How the TOC / SHELL workbook reaches the chat: `"text"` (default) the
  shells as text in each request
  ([`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md));
  `"attach"` the workbook attached to each chat, the request saying
  where each report's shell is in it (its sheet, cells and TOC row)
  instead of the text; `"both"` the places and the text, the attachment
  right where the two differ – for a workbook whose text is not read
  right. A chat that takes no Excel file needs `"text"`. `"file"`: for
  an agent that reads files
  ([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)`(mode = "agent")`)
  – the text, and the workbook's path with each report's place in it, to
  read that place alone when the text looks cut, never another sheet.

- overwrite:

  `TRUE` to write over the requests already there; with `add`, to ask
  again for a report asked for already (its request is written anew).

- add:

  `TRUE` to keep the requests already there and add these: the new ones
  named after them (`ae-2` when `ae-1` is there, `figure-3` ...), the
  index listing all of them, each request's `book` the workbook its
  reports' shells are in.

- templates:

  `TRUE` (default): each request asks the AI to answer a report with a
  template (`template: {name, args}`) where one fits – the templates
  registered when the requests are written listed in it, its first line
  `<!-- templates: TRUE -->` – else as a code SPEC. `FALSE`: the code
  SPEC only, the requests as they were before templates.

- mode:

  `"chat"` (default): each request whole, to paste. `"agent"`
  ([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)`(mode = "agent")`):
  what every request would repeat in a file of the study's `ai/` folder,
  written once (`instructions-table.md`, `instructions-figure.md`,
  `templates.md`), the ADaM summary and `spec/` files named, not copied;
  each request names them, its answer's header and, with `templates`,
  the templates likely for its reports (from their titles). The folder
  the answers go to (`ai/answers`, beside `dir`) is made too.

- progress:

  `NULL`, or a function(step, i, n) called as the requests are made:
  `step` `"request"`, `i` of `n` written (0 first) – for a progress bar
  (shiny's `setProgress()`). Each request is written as it is made;
  `index.md` and the contexts at the end.

- unit:

  A request's unit: `"sheet"` (default; `shell`'s `shells`) a shell
  sheet, its TOC items as candidates, and one request of the TOC items
  no sheet chose; `"toc"` the TOC's reports in batches
  ([`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md)),
  as before. A sheet's request lets the AI name its reports: their ids
  are not held to a list (one that is no TOC item's is a warning), and a
  sheet that is no shell is answered with no report (a warning).

## Value

A data frame, one row a request: `request` (the file's name, `ae-1`),
`family`, `outputs` (the report ids, comma-separated), `n`, `chars`,
`file`. The same is written to `index.md` in `dir`.

## Details

Start small (`size = 2` or `3`), check the answers, then raise `size`
while the chat keeps up.

A request holds the instructions and schema (about 23,000 characters,
the same in every request) and its own reports (the TOC rows, shells and
ADaM summary). For a chat that cannot take that in one message,
`instructions = "once"` writes the instructions to `00-instructions.md`
and each request without them: paste `00-instructions.md` first in a new
chat, then the request.

## See also

[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
for the answers.

## Examples

``` r
f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
d <- file.path(tempdir(), "req-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
if (requireNamespace("haven", quietly = TRUE)) {
  haven::write_xpt(as.data.frame(lapply(adsl, function(x)
    if (is.factor(x)) as.character(x) else x)), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, overwrite = TRUE)
  r <- tfl_ai_write_requests(d, tfl_ai_read_shell(f),
                             tfl_ai_adam_summary(file.path(d, "adam")),
                             size = 2, overwrite = TRUE)
  r[c("request", "outputs", "chars")]
}
#> 4 requests (5 reports) in ai/requests; the longest 49116 characters.
#> Next: paste ai/requests/demographics-1.md into a new chat after 00-instructions.md; save the whole answer as ai/answers/demographics-1.md, then run tfl_ai_collect_answers().
#>          request            outputs chars
#> 1 demographics-1           T-14-1-1  7166
#> 2     efficacy-1           T-14-2-1  7048
#> 3           ae-1 T-14-3-1, T-14-3-5  7929
#> 4       figure-1           F-14-2-2 49116
```
