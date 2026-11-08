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
  workbook = c("text", "file"),
  max_chars = NULL,
  overwrite = FALSE,
  dry_run = FALSE,
  adam_summary = NULL,
  datasets = NULL,
  max_levels = 15L,
  quiet = FALSE,
  shells = NULL,
  add = FALSE,
  templates = TRUE,
  progress = NULL,
  toc_cols = NULL,
  sheet_roles = NULL,
  unit = c("sheet", "toc"),
  area = TRUE,
  numbers = NULL,
  agent_reads = character(),
  docs = NULL,
  agent_max_reads = 3L,
  define = NULL,
  adam_spec = NULL
)
```

## Arguments

- dir:

  The study folder.

- toc:

  The TOC / SHELL workbook (`.xlsx` or `.xlsm`). Missing: the last
  call's settings, `ai/prepare.yml` (see *Repeating a call*).

- adam:

  The ADaM folder, from `dir` (relative) or absolute.

- study_id, title:

  The study's id and title, for a new study folder.

- toc_sheet, sheets:

  As
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md):
  the TOC sheet when the guess is wrong, the sheets to keep.

- size, language, workbook, max_chars:

  As
  [`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md):
  the reports a request holds (`NULL`, the default: 1), the language of
  the assumptions – and of the agent's questions in the chat: it is
  written to `study.yml` as `ai: language` –, how the workbook reaches
  the agent (`"text"`, `"file"`; with `"workbook"` in `agent_reads`,
  `"text"` is `"file"`: the text and each shell's place in the workbook
  file), a request's length at most.

- overwrite:

  `TRUE` to start the study folder again
  ([`tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.md)
  over its `study.yml` and `spec/`). The requests are written again on
  every call (the drafts are kept).

- dry_run:

  `TRUE` to read the workbook and the ADaM and write nothing: the same
  result, to look at before the requests are written (its `requests` are
  the TOC's batches, without `chars` and `file`; `files` is empty). The
  one thing it keeps is `numbers`, the person's corrected report
  numbers, in `ai/sheet_numbers.csv` when the study's `ai/` folder is
  there already (`files` then names that file); a dry run never makes
  `ai/` or the study folder.

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

- shells:

  Workbooks holding the shells when `toc` holds the TOC alone (as
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)):
  each report's shell is looked for in them; the requests and the `toc`
  say each report's `book`.

- add:

  `TRUE` to add this workbook's reports to the study's requests and
  drafts already there (a TOC of listings, a book of PK figures, one at
  a time): the new requests are named after those there (`ae-2`), a
  report asked for already stops the call (`overwrite = TRUE` asks for
  it again), `spec/toc.yml` gets the new reports at its end, and the
  workbook's text is `ai/toc_shell-<workbook>.md`. With `add`,
  `overwrite` is about the requests alone: the study's `study.yml`,
  `spec/data.yml`, `spec/defaults.yml` and the agent's files are kept as
  they are.

- templates:

  `TRUE` (default): the agent writes a report with one of the study's
  templates where it fits (`template: {name, args}`, and the report's
  own content items for what it does not cover), else as a code SPEC;
  the requests (and `AGENTS.md`) list the templates registered now –
  tflspec's, a company's, the study's variants. `FALSE`: the code SPEC
  only, the requests as they were before templates. The setting is a
  line near the top of each request (`templates: TRUE`) and
  `ai/status.md`'s.

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

- area:

  Where the agent's drafts go: `TRUE` (the default), their own folder
  `ai/spec/outputs/` (made here), built into `ai/output/`, which the
  person adopts into the study's `spec/outputs/`
  ([`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md));
  `FALSE`, the folder is not made here (a study of the form before is
  moved with
  [`tfl_ai_migrate()`](https://ichirio.github.io/tflspec/reference/tfl_ai_migrate.md)).

- numbers:

  The person's check of the report numbers each shell sheet writes (see
  the result's `numbers`), over the rule (a number written in the sheet
  or its name, and a TOC item's, is a report of the sheet's request): a
  data frame with `sheet`, `number` and `make` – `FALSE`, the number is
  a reference only (the request still shows it, not as one of its
  reports); `TRUE`, a report of that sheet's request (a TOC item the
  rule left a candidate, or a number no TOC item has, which is warned of
  and keeps its number as the sheet writes it as its id); `NA`, back to
  the rule – or a list named by sheet, each the numbers that sheet makes
  (its other numbers references only). The numbers and sheets not given
  keep the rule. The result's `toc` follows: a TOC item made in a sheet
  has that sheet as its shell (made in several, the first in the
  workbook's order; the others listed in `sheets`), one the rule made
  and no sheet makes now has none (the no-shell request's, counted in
  `counts`). Saved as `ai/sheet_numbers.csv` (`sheet`, `number`, `make`,
  UTF-8) when the requests are written, and by a dry run when the
  study's `ai/` folder is there already (a dry run on a new study keeps
  nothing), and read by a later call; given again, it goes over the
  saved rows (a row of the same sheet and number replaced, `NA` taking
  it out) and the file keeps the result. `NULL` (default): the saved
  file, if any. With `unit = "sheet"` only.

- agent_reads:

  What the agent may open besides the requests and the files they name:
  `"workbook"` (the TOC / SHELL workbook: each request then says where
  its shells are in it) and `"adam"` (the ADaM), read only to settle
  what a request leaves unclear, with whatever tool the PC has
  (PowerShell, Python, R).
  [`character()`](https://rdrr.io/r/base/character.html) (default):
  neither, and `AGENTS.md` says not to open them.

- docs:

  Documents the agent may read for what a request leaves unclear (a SAP,
  a data guide): their paths. A study's documents live in the study
  folder (its `docs/`): one inside the folder is read where it is, never
  copied – a `.md` / `.txt` at its own path; a `.docx`, `.pdf` or
  workbook, which an agent in the editor cannot read, as its text
  written to `ai/docs/<name>.md`
  ([`tfl_ai_doc_text()`](https://ichirio.github.io/tflspec/reference/tfl_ai_doc_text.md):
  the headings and tables kept). One outside the study folder is copied
  into `ai/docs/` (the agent works in the study folder), the text beside
  it for those kinds. `AGENTS.md` names what the agent reads and which
  document a text stands for; the agent reads no other document. `NULL`
  (default): none.

- agent_max_reads:

  How many times for a request the agent may open a file or run a
  command beyond the request and the files it names; then it asks, or
  marks what is unclear to confirm, and goes on. Default 3.

- define, adam_spec:

  The study's `define.xml`, or its ADaM spec workbook (Pinnacle 21
  layout), for the ADaM summary
  ([`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md)):
  its code lists in their order with their decodes, and the digits and
  display formats, value level too (a parameter's AVAL), go into
  `ai/adam_summary.md` as the study's. `NULL` (default): the data alone.

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
with its `request`), `numbers` (a row a report number a shell sheet
writes or its name has: `sheet`, `number` as the sheet writes it,
`title` – the TOC item's, else the sheet's own when it writes that
number alone, else `NA` –, `toc_id` – the TOC item it is, `NA` none –,
`make` – a report of that sheet's request –, `source` – `"rule"`,
`"guess"` (a TOC item no sheet writes or names, whose number – a part
letter taken off, `15.2.4.2.b` – sheets write, each number (a part with
its letter) on its own: made the best sheet's, its id the number the
sheet writes, the other sheets' a candidate each, not made; the best: an
exact number before one a part letter differs, in the sheet's title rows
or name before its body only, then the sheet whose own number is
nearest, then the first in the workbook), `"saved"`
(`ai/sheet_numbers.csv`) or `"given"` (`numbers`) –, `reason` – a
guess's: how it matches, "not an exact match" when not, its choice among
the sheets; `"set by the person"` for `"saved"` and `"given"`; `NA` the
rule's –; a dry run's and the written requests' alike), `counts`
(`shells`, `toc_items`, `no_shell`), `sheets` (the workbook's sheets and
their roles), `adam` (the
[`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md):
`datasets`, `variables`, `text`), `data_paths` (a study that has a
`spec/data.yml` already: per dataset of it, `in_data_yml` its path,
`given` the ADaM file given (`NA`: none), `exists` whether data.yml's
file is there and `differs` whether the file given is another file or
type; a warning names the rows that are missing or differ; `data.yml` is
not changed; `NULL` for a new study), `files` (the files written,
relative to `dir`), `hint` (what the workbook's reading could not find –
no TOC, a report with no shell – and what to pass), `next_steps` (what
to do next), `agent_prompt` (the first message to the agent, its paths
from the study folder (a copy of the study says the same), also written
to `ai/agent_prompt.md`) and `agent_files` (what the agent may open
besides the requests and the files they name, from the study folder: the
workbook and the ADaM as `agent_reads` allows, and the documents;
[`character()`](https://rdrr.io/r/base/character.html) else). Its print
is the workbook's summary and the next step. The files are UTF-8.

## The agent's work

The requests are for a coding agent (GitHub Copilot's agent in VS Code,
and the like) that **asks the person in the chat** what defines each
report and is not settled – the analysis set, a parameter, the treatment
variable, a denominator – waits for the answers, keeps them in
`ai/decisions.md`
([`tfl_ai_decisions_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_decisions_read.md))
and applies them to the reports after, then writes each report's
**skeleton** as a draft, `ai/spec/outputs/<id>.yml`: its data, its
analyses, the items that make its rows (or a template), its titles and
footnotes – nothing of the look. What it could not settle it marks under
the draft's `ai: confirm`
([`tfl_ai_confirm()`](https://ichirio.github.io/tflspec/reference/tfl_ai_confirm.md)).
The person checks the drafts (tflplanner's "Check",
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)),
finishes them in the builder and adopts them
([`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md)).

Besides the requests (each starting with the agent's steps and the
decisions that apply to its reports), and what every request would
repeat written once for the agent to read – `ai/instructions-table.md`
and `ai/instructions-figure.md` (the instructions, the schema, what a
draft holds), `ai/templates.md` (with `templates = TRUE`: every
template, each request naming the ones likely for its reports),
`ai/adam_summary.md` and `spec/`, each request naming them – the study
folder gets `ai/AGENTS.md` (the instructions: what the agent reads, how
it asks, what a draft holds), `AGENTS.md` and
`.github/copilot-instructions.md` (short: read `ai/AGENTS.md`), `run.R`
([`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md):
the check from the command line, with the ARD and RTF) and
`ai/decisions.md`, which is never written over. One message
(`ai/agent_prompt.md`) starts the agent: with one request, then a menu,
or with a batch of all the requests (`ai: run`); `ai/progress.md`, the
work queue (every request `todo`; never written over), says where it
goes on.

`study.yml`'s `ai:` keys set the agent's rules (the study standard's
`spec/defaults.yml` may say them under its own `ai:` key first):
`language` (the chat's: `"en"`, `"ja"` ...), `run` (`"one"`, the
default: the agent processes one request – its questions in the chat,
its drafts, its build and check – and stops at a menu: the next request,
the next N together, or all the rest; `"batch"`: all the requests at
once, the agent taking its default for every question, noting it in
`ai/questions-end.md`, and asking all of them in one message at the end,
with `ai/defaults.md`, the defaults it took, the most important first –
only the reports an answer changes are then written again),
`requests_per_chat` (0, no limit: after that many requests the agent
stops and asks for a new chat), `carry` (which decision scopes apply to
a later report without asking: `all`, `dataset`, `section`),
`max_rounds` (how many times one item is asked again, 3), `footnotes_at`
(`"footer"`, the default, or `"table"`: where the shell's footnotes go),
`review` (how deep the agent checks each report it builds itself, by
kind: `0` the check, `1` and the program, `2` and the facts – what R
made, in words – `3` and the output (a figure's PNG, a table's pages);
`none`: the person's check; `{figure: 3, table: 2, listing: 1}` by
default).

## Repeating a call

A call that writes the requests (not `dry_run`) keeps the arguments it
was given in `ai/prepare.yml` of the study folder – by name, a path
inside the study from its folder, the date-time and the tflspec version
at its head
([`tfl_ai_prepare_settings()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare_settings.md)
reads it and lists what the file holds and what is left out: `dir`,
`overwrite`, `dry_run`, `quiet`, `progress`, `add`, `adam_summary`,
`numbers`). `tfl_ai_prepare(dir)` with no `toc` repeats them: the
arguments given now go over the file's, so
`tfl_ai_prepare(dir, language = "ja")` is the last call in Japanese. A
path of the file that is not there any more stops the call, naming the
file and the setting; with no file, `toc` is asked for as always. With
`add = TRUE` the file keeps the first workbook's settings (see
[`tfl_ai_prepare_settings()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare_settings.md)).
The agent does not read the file.

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
#> 4 requests (5 reports) in ai/requests; the longest 5059 characters.
#> Next: give the agent ai/requests/T14.1.1.md (ai/agent_prompt.md); when its drafts are written, run tfl_ai_collect_answers().
#>   request            outputs chars
#> 1 T14.1.1           T-14-1-1  5153
#> 2 T14.2.1           T-14-2-1  5026
#> 3  Safety T-14-3-1, T-14-3-5  5888
#> 4 F14.2.2           F-14-2-2  4404
# }
```
