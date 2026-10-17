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
  size = 8L,
  dir = file.path("ai", "requests"),
  max_chars = NULL,
  families = NULL,
  types = "table",
  language = NULL,
  examples = NULL,
  instructions = c("each", "once"),
  overwrite = FALSE
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

  The reports a request holds at most (1 or more).

- dir:

  The folder the requests go to, under `study_dir`.

- max_chars:

  A request's length at most, in characters; `NULL` for no limit.

- families, types:

  As
  [`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md).

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

  `"each"` (default): every request holds the instructions, one paste a
  request. `"once"`: the instructions in `00-instructions.md`, the
  requests without them.

- overwrite:

  `TRUE` to write over the requests already there.

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
#> 4 requests (4 reports) in ai/requests; the longest 28692 characters.
#>          request  outputs chars
#> 1 demographics-1 T-14-1-1 28692
#> 2           ae-1 T-14-3-1 28573
#> 3         labs-1 T-14-3-5 28394
#> 4     efficacy-1 T-14-2-1 28565
```
