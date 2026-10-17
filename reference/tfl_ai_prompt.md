# A drafting prompt for an AI chat

The prompt that asks a chat model to draft part of a specification from
the study's documents: the study's table of contents (`toc`) or one
figure's design (`figure`), or the report files of a batch of tables
(`table`: 5 to 10 tables of one family,
[`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md)).
tflspec sends nothing: the prompt is text, and so is the answer, which
[`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)
reads.

## Usage

``` r
tfl_ai_prompt(
  task,
  context = NULL,
  mode = c("chat", "api"),
  format = c("yaml", "json"),
  example = TRUE,
  documents = NULL
)
```

## Arguments

- task:

  One of
  [`tfl_ai_tasks()`](https://ichirio.github.io/tflspec/reference/tfl_ai_tasks.md).

- context:

  A
  [`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
  of the same task; `NULL` for an empty one (a figure needs one: its
  `output_id`).

- mode:

  `"chat"` (the user attaches the documents) or `"api"` (their text is
  in the prompt).

- format:

  The answer's format: `"yaml"` (the default) or `"json"`.
  [`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)
  reads both.

- example:

  Include an example answer.

- documents:

  Mode `"api"`: the documents' text, a list of lists each with `name`
  and `text` and optionally `kind` and `section`.

## Value

A `tfl_ai_prompt`: a list with `system`, `user` (strings), `attach`
(what the user attaches, mode `"chat"`), `messages` (the system and user
messages as a list of `role` / `content`), `task`, `mode`, `format` and
`nchar`. [`format()`](https://rdrr.io/r/base/format.html) and
[`print()`](https://rdrr.io/r/base/print.html) give it as one text to
copy.

## Details

The `system` part holds the ground rules (only the names given, nothing
invented, a draft that a person reviews), the task's instructions, the
answer's shape – one fenced block that starts with the header
`tflspec_ai: {task: ..., version: 1, ...}` and ends with the
`assumptions` – the schema
([`tfl_ai_schema()`](https://ichirio.github.io/tflspec/reference/tfl_ai_schema.md))
and an example answer from the sample study. The `user` part holds the
context
([`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md))
and what to look for in the documents. The prompt is English; the model
is asked to write its assumptions in the user's language
(`context$language`, or the language of the user's message).

- Mode `"chat"`: the user pastes `format(prompt)` into a chat and
  attaches the documents themselves; `attach` lists them.

- Mode `"api"`: the documents' text, extracted by the caller, is in the
  `user` part between `<document>` fences; `messages` is what a client
  sends. tflspec has no HTTP client.

The fixed wording is in the package's `ai/prompts` folder
(`system.file("ai", "prompts", package = "tflspec")`).

## See also

[`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md),
[`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md),
[`tfl_ai_repair_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_repair_prompt.md),
[`tfl_ai_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_diff.md).

## Examples

``` r
ctx <- tfl_ai_context("toc", study = list(study_id = "CDISCPILOT01"),
                      reports = "T-14-1-1")
p <- tfl_ai_prompt("toc", ctx)
p$attach
#> [1] "the statistical analysis plan (SAP)"                              
#> [2] "the study's list of planned outputs, if it is a separate document"
cat(substr(format(p), 1, 800))
#> You are helping a statistical programmer draft the specification of a clinical study's tables, listings and figures. The specification is read by tflspec, an R package that turns it into report programs. tflspec is new and is probably not in your training data: rely on this message, not on memory.
#> 
#> ## Your task
#> 
#> Draft the study's table of contents (TOC): the list of the tables, figures and listings the study will report, one row a report, with their numbers, titles, analysis sets and footnotes, as the documents plan them. tflspec reads your rows as it reads a company's TOC workbook.
#> 
#> - Write one row a report, in the order the documents give them. A section heading (such as "14.1 Demographics") is not a row: put it in the `section` column of the reports under it.
#> - Use the report numbers th

# a figure, the documents' text in the prompt
ctx <- tfl_ai_context("figure", output_id = "F-14-2-1",
                      titles = "Kaplan-Meier Plot of Time to Death")
p <- tfl_ai_prompt("figure", ctx, mode = "api", documents = list(
  list(name = "SAP", section = "9.4", text = "Overall survival ...")))
p$nchar
#> system   user 
#>  26878    554 
```
