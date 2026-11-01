# Write a study's requests, one file a request

Splits the TOC's tables into requests
([`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md))
and writes each one's text
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md))
to its own file, for a coding agent
([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)):
what every request would repeat is written once in the study's `ai/`
folder (`instructions-table.md`, `instructions-figure.md`,
`templates.md`), and each request names those files, the ADaM summary
and the `spec/` files instead of copying them. A request longer than
`max_chars` is split again, until it fits or holds one report. The
requests' contexts are kept beside them (`_contexts.rds`), so
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
checks each request's drafts against its own request.

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
  types = c("table", "listing", "figure"),
  language = NULL,
  examples = NULL,
  workbook = c("text", "file"),
  overwrite = FALSE,
  add = FALSE,
  templates = TRUE,
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
  1, each request's drafts checked before the next.

- dir:

  The folder the requests go to, under `study_dir`.

- max_chars:

  A request's length at most, in characters; `NULL` for no limit. A
  request over it is halved.

- families, types:

  As
  [`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md):
  the tables and listings in batches (a listing's family `listing`; its
  draft a report of `kind: listing`). A figure (`"figure"` in `types`,
  the default) is a request of its own, `figure-1`, `figure-2` ...
  (family `figure`): its shell, the ADaM's summary, the figure
  templates; its draft a figure report.

- language:

  As
  [`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md):
  the language of the AI's assumptions.

- examples:

  Reviewed report files for the AI to follow
  ([`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)'s
  `examples`). Named by family (`c(ae = "spec/outputs/T-14-3-1.yml")`),
  each goes only to its family's requests; unnamed, to every request.

- workbook:

  How the TOC / SHELL workbook reaches the agent: `"text"` (default) the
  shells as text in each request
  ([`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md));
  `"file"` the text, and the workbook's path with each report's place in
  it, for an agent allowed to open it
  ([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)'s
  `agent_reads`) to read that place alone when the text looks cut, never
  another sheet.

- overwrite:

  `TRUE` to write over the requests already there; with `add`, to ask
  again for a report asked for already (its request is written anew).

- add:

  `TRUE` to keep the requests already there and add these: the new ones
  named after them (`ae-2` when `ae-1` is there, `figure-3` ...), the
  index listing all of them, each request's `book` the workbook its
  reports' shells are in.

- templates:

  `TRUE` (default): each request asks the AI to write a report with a
  template (`template: {name, args}`) where one fits – the templates
  registered when the requests are written listed in `ai/templates.md`,
  each request naming the ones likely for its reports, its first line
  `<!-- templates: TRUE -->` – else as a code SPEC. `FALSE`: the code
  SPEC only.

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

## See also

[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md),
which calls it;
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
for the drafts.

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
#> 4 requests (5 reports) in ai/requests; the longest 4881 characters.
#> Next: give the agent ai/requests/demographics-1.md (ai/agent_prompt.md); when its drafts are written, run tfl_ai_collect_answers().
#>          request            outputs chars
#> 1 demographics-1           T-14-1-1  4174
#> 2     efficacy-1           T-14-2-1  4047
#> 3           ae-1 T-14-3-1, T-14-3-5  4881
#> 4       figure-1           F-14-2-2  3429
```
