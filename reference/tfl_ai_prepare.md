# Prepare a study's AI table requests in one call

The steps before the first request, one after the other:
[`tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.md)
(only when the study has no `study.yml` yet – one that is there is kept,
so the call can be run again),
[`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md)
(to `ai/adam_summary.md`),
[`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)
(to `ai/toc_shell.md`; what it could not find is the result's `hint`),
`spec/toc.yml` (the TOC's reports in order, with their section and
analysis set, as tflplanner reads it; when the study has none) and
[`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md).
Nothing is sent anywhere.

## Usage

``` r
tfl_ai_prepare(
  dir,
  toc,
  adam = "adam",
  study_id = "STUDY-01",
  title = NULL,
  toc_sheet = NULL,
  sheets = NULL,
  size = NULL,
  language = NULL,
  workbook = c("text", "attach", "both", "file"),
  instructions = c("once", "each"),
  max_chars = NULL,
  overwrite = FALSE,
  dry_run = FALSE,
  adam_summary = NULL,
  datasets = NULL,
  max_levels = 15L,
  quiet = FALSE,
  mode = c("chat", "agent"),
  shells = NULL,
  add = FALSE,
  templates = TRUE,
  progress = NULL,
  toc_cols = NULL,
  sheet_roles = NULL,
  unit = c("sheet", "toc")
)
```

## Arguments

- dir:

  The study folder.

- toc:

  The TOC / SHELL workbook (`.xlsx` or `.xlsm`).

- adam:

  The ADaM folder, from `dir` (relative) or absolute.

- study_id, title:

  The study's id and title, for a new study folder.

- toc_sheet, sheets:

  As
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md):
  the TOC sheet when the guess is wrong, the sheets to keep.

- size, language, workbook, instructions, max_chars:

  As
  [`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md):
  the reports a request holds (`NULL`, the default: 1 with
  `mode = "agent"`, 3 in a chat), the language of the assumptions, how
  the workbook reaches the chat (`"text"`, `"attach"`, `"both"`,
  `"file"`; with `mode = "agent"`, `"text"` is `"file"`: the text and
  each shell's place in the workbook file), the instructions in each
  request or once, a request's length at most.

- overwrite:

  `TRUE` to start the study folder again
  ([`tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.md)
  over its `study.yml` and `spec/`). The requests are written again on
  every call (the answers are kept).

- dry_run:

  `TRUE` to read the workbook and the ADaM and write nothing: the same
  result, to look at before the requests are written (its `requests` are
  the TOC's batches, without `chars` and `file`; `files` is empty).

- adam_summary:

  A
  [`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md)
  result to use instead of reading the ADaM again (a previous call's
  `$adam`); read again when `datasets` names others than it has.

- datasets, max_levels:

  As
  [`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md):
  the ADaM datasets the requests describe (`NULL`: all), the values
  listed of a variable. Fewer datasets, shorter requests.

- quiet:

  `TRUE` to say nothing (the steps' messages): a screen shows the
  result's `hint` and `next_steps` itself.

- mode:

  `"chat"` (default): requests to paste into a chat, one at a time.
  `"agent"`: for a coding agent – see *For a coding agent*.

- shells:

  Workbooks holding the shells when `toc` holds the TOC alone (as
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)):
  each report's shell is looked for in them; the requests and the `toc`
  say each report's `book`.

- add:

  `TRUE` to add this workbook's reports to the study's requests and
  answers already there (a TOC of listings, a book of PK figures, one at
  a time): the new requests are named after those there (`ae-2`), a
  report asked for already stops the call (`overwrite = TRUE` asks for
  it again), `spec/toc.yml` gets the new reports at its end, and the
  workbook's text is `ai/toc_shell-<workbook>.md`. With `add`,
  `overwrite` is about the requests alone: the study's `study.yml`,
  `spec/data.yml`, `spec/defaults.yml` and the agent's files are kept as
  they are.

- templates:

  `TRUE` (default): the AI answers a report with one of the study's
  templates where it fits (`template: {name, args}`, and the report's
  own content items for what it does not cover), else as a code SPEC;
  the requests (and `AGENTS.md`) list the templates registered now –
  tflspec's, a company's, the study's variants. `FALSE`: the code SPEC
  only, the requests as they were before templates. The setting is each
  request's first line (`templates: TRUE`) and `ai/status.md`'s.

- progress:

  `NULL`, or a function(step, i, n) called as the work goes: `step`
  `"adam"` (the ADaM's summary), `"workbook"` (the TOC / SHELL read),
  then `"request"`, `i` of `n` requests written – for a progress bar
  (shiny's `setProgress()`); see
  [`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md).

- toc_cols:

  The TOC's columns, when its headers do not say them (as
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)):
  `c(output_id = "Output No", shell = "Shell", title = c("Title 1", "Title 2"))`,
  a header's text or a column's letter.

- sheet_roles:

  As
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md):
  sheets' roles over the guess.

- unit:

  A request's unit: `"sheet"` (default) a shell sheet – what the sheet
  writes first, then its name, then its TOC candidates (the TOC and the
  sheet names can hold mistakes), the reports and their ids the AI's to
  read from it – and one request of the TOC items no sheet chose;
  `"toc"` the TOC's reports, as before (a report's shell found by its
  number).

## Value

A `tfl_ai_prepared`: a list with `requests` (one row a request:
`request`, `family`, `outputs` – the TOC items it names (a sheet's: its
candidates) –, `n`, `chars`, `file`, `sheets` – as
[`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md)
gives them), `toc` (one row a TOC item: `toc_id`, `title`, `type`,
`toc_sheets`, `sheets` – the shell sheets it is a candidate of –,
`shell`, `shell_found`, `request` – the requests naming it, `NA` for
none –, `book`, `shell_id`, `population`, `program`, `file`, `info` as
[`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)'s
`toc` says them), `shells` (a row a shell sheet, as
[`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)'s,
with its `request`), `counts` (`shells`, `toc_items`, `no_shell`),
`sheets` (the workbook's sheets and their roles), `adam` (the
[`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md):
`datasets`, `variables`, `text`), `files` (the files written, relative
to `dir`), `hint` (what the workbook's reading could not find – no TOC,
a report with no shell – and what to pass), `next_steps` (what to do
next) and `agent_prompt` (with `mode = "agent"`: the first message to
the agent, its paths from the study folder (a copy of the study says the
same), also written to `ai/agent_prompt.md`; else `NA`). Its print is
the workbook's summary and the next step. The files are UTF-8.

## For a coding agent

`mode = "agent"`: The requests for an agent (GitHub Copilot's agent in
VS Code, and the like) that works through all of them without stopping.
Besides the requests (each starting with the agent's steps), and what
every request would repeat written once for the agent to read –
`ai/instructions-table.md` and `ai/instructions-figure.md` (the
instructions and the schema), `ai/templates.md` (with
`templates = TRUE`: every template, each request naming the ones likely
for its reports), `ai/adam_summary.md` and `spec/`, each request naming
them – the study folder gets `AGENTS.md` and
`.github/copilot-instructions.md` (the same instructions: answer each
request into `ai/answers/`, run `run.R`, read `ai/status.md`, repair; a
question it cannot settle taken as an assumption with its `question`,
never a stop; the ADaM read with R only to settle a question the summary
does not), `run.R`
([`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md):
collect, ARD, RTF, `ai/status.md`, the questions gathered into
`ai/questions.md`), `ai/questions.md` (for the person's answers) and
`ai/decisions.md` (what the reports follow; the agent moves an answered
question there, then redoes the reports it touches – the second round).
`questions.md` and `decisions.md` are never written over. The agent
works in the study folder with the company's approved AI: it may read
the ADaM; a chat (`mode = "chat"`) is sent the ADaM's summary only.

## See also

The AI table guide:
`system.file("ai", "tflspec-ai-table-guide.md", package = "tflspec")`.

## Examples

``` r
# \donttest{
# (over 5 seconds: the workbook read, five ADaM written, every request)
ex <- system.file("ai", "example", package = "tflspec")
d <- file.path(tempdir(), "prepare-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
if (requireNamespace("haven", quietly = TRUE)) {
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  r <- tfl_ai_prepare(d, file.path(ex, "onco_toc_shell.xlsx"),
                      study_id = "XYZ-ONC-01", overwrite = TRUE)
  r$requests[c("request", "outputs", "chars")]
}
#> 4 requests (5 reports) in ai/requests; the longest 55818 characters.
#> Next: paste ai/requests/demographics-1.md into a new chat after 00-instructions.md; save the whole answer as ai/answers/demographics-1.md, then run tfl_ai_collect_answers().
#>          request            outputs chars
#> 1 demographics-1           T-14-1-1 12555
#> 2     efficacy-1           T-14-2-1 12437
#> 3           ae-1 T-14-3-1, T-14-3-5 13318
#> 4       figure-1           F-14-2-2 55818
# }
```
