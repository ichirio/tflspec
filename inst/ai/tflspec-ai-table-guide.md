# Drafting tables' code spec with a coding agent

## 1. What it does

From a study's **TOC/SHELL workbook** and its **ADaM (XPT)**, a coding agent
(GitHub Copilot's agent in VS Code, and the like) drafts each table's
**code spec** (`ai/spec/outputs/<id>.yml`): it **asks you in its chat**
what defines a report and is not settled, keeps your answers as the
study's decisions, and writes each report's skeleton -- nothing of the
look. tflspec checks the drafts and makes the **RTF**; you finish them in
tflplanner's builder and adopt them. Everything runs in R on your PC;
tflspec sends nothing.

Japanese: `tflspec-ai-table-guide-ja.md` in the same folder
(`system.file("ai", package = "tflspec")`).

## 2. What goes where

```text
mystudy/
  TOC_SHELL.xlsx        your TOC and shells (one workbook)
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
  AGENTS.md             written by tflspec: the agent's instructions (step 3)
  ai/requests/          written by tflspec: one file a request (step 3)
  ai/decisions.md       your answers from the chat, one line each (step 5)
  ai/spec/outputs/      the agent's drafts, one file a report (step 5)
  ai/repairs/           written by tflspec: what to paste back (step 6)
```

Install once (a new R has no `remotes`; `broom` is what cardx's confidence
intervals need):

```r
install.packages(c("remotes", "cards", "cardx", "haven", "broom"))
remotes::install_github(c("ichirio/rtfreporter", "ichirio/tflspec"))
```

## 3. What to run

```r
library(tflspec)
r <- tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", adam = "adam",
                    study_id = "ABC-123", language = "Japanese")
r                                   # request, outputs, characters; the next steps
```

One call does the steps: the study folder when there is none (`study.yml`,
`spec/data.yml`, `spec/defaults.yml`; an existing `study.yml` is kept, so
the call can be run again), the ADaM summary (`ai/adam_summary.md`: the
variables, their labels and short value lists -- no records), the workbook
read (`ai/toc_shell.md`), the requests (`ai/requests/<request>.md`, one a
report by default: `T-14-1-1.md` ...; `size = 3` groups a family's reports,
`demographics-1.md`, `ae-1.md` ...; `ai/requests/index.md` lists them), what
every request would repeat written once (`ai/instructions-table.md`,
`ai/instructions-figure.md`, `ai/templates.md`), and the agent's files:
`AGENTS.md` and `.github/copilot-instructions.md` (the same text), `run.R`,
`ai/questions.md`, `ai/decisions.md` and `ai/agent_prompt.md` (its first
message). A figure of the TOC is a request of its own (`figure-1.md` ...):
its shell, and a figure report to draft. The steps one at a time:
`tfl_ai_study_init()`, `tfl_ai_adam_summary()`, `tfl_ai_read_shell()`,
`tfl_ai_write_requests()`.

The TOC and the shells in workbooks apart: `tfl_ai_prepare("mystudy",
"TOC.xlsx", shells = "SHELLS.xlsx", ...)`; another workbook later (the
listings, a book of PK figures): the same call with `add = TRUE` -- the
requests and drafts there are kept, the new requests added beside them.

Check before the first request:

| Look at | Why |
|---|---|
| `spec/data.yml` | the analysis sets (one per ADSL flag `SAFFL`, `FASFL` ...): the agent names them by id |
| `r$toc` | the reports found; a TOC sheet not found: `tfl_ai_prepare(..., toc_sheet = "Index")` |
| `r$toc$shell` | `NA` = no shell sheet found: `tfl_ai_prepare(..., sheets = c("DM shells", ...))` |
| `ai/adam_summary.md` | no records, but short value lists (up to 15): remove what your company does not allow |
| `AGENTS.md` | what the agent may read and must ask; `study.yml`'s `ai:` keys change its rules (below) |

**The sheets' report numbers.** Before the requests are written, check
which numbers each shell sheet makes: `tfl_ai_prepare(..., dry_run =
TRUE)$numbers` has a row a number a sheet writes (or its name has) --
`sheet`, `number`, `title`, `toc_id` (the TOC item, `NA` none), `make`
(made a report of that sheet's request), `source`. A number written only
as a reference ("see Table 14.2.1") is made by the rule too: correct it
with `numbers = data.frame(sheet = "t14.1.1", number = "Table 14.2.1",
make = FALSE)` (reference only: the request still shows it), or
`numbers = list(t14.1.1 = "Table 14.1.1")` (the numbers the sheet makes;
its others reference only). A number no TOC item has, made `TRUE`, is a
report all the same (with a warning). A TOC item no sheet writes or names
whose number (with a part letter: `Table 15.2.4.2.b`) one sheet writes is
made that sheet's report (`source` `"guess"`); written in two sheets, it is
a candidate of each. The correction is saved as
`ai/sheet_numbers.csv` when the requests are written, and used by the
next calls until `numbers` is given again.

**The agent's rules** are `study.yml`'s `ai:` keys (the study standard's
`spec/defaults.yml` may say them first; `study.yml` wins):

```yaml
ai:
  language: ja          # the chat's language ("en" by default)
  ask_when: request     # the request's reports' questions together; "report": one by one
  carry: [all, dataset, section]   # which decisions apply to a later report without asking
  max_rounds: 3         # one item asked again at most this many times, then marked to confirm
  footnotes_at: footer  # where the shell's footnotes go: "footer", or "table"
```

## 4. What the agent reads

| Reads | Never |
|---|---|
| the request it is given and the files it names: `ai/instructions-*.md`, `ai/templates.md`, `ai/adam_summary.md`, `spec/data.yml`, `spec/defaults.yml`, `ai/decisions.md` | the **XPT files**, or any data with records: subject ids, dates, results; other folders, other files |

Use the AI your company approves for the study's data: the agent is sent
the ADaM's summary, not the data. `AGENTS.md` says what it may open; with
`tfl_ai_prepare(..., agent_reads = c("workbook", "adam"))` it may also
read the workbook (the place a request names, when a shell's text looks
cut) and the ADaM (a variable's values, never a data file in the editor),
with whatever tool the PC has; `docs = ` gives it documents (a SAP, a data
guide), copied into `ai/docs/`. It installs nothing and runs nothing.

Each request is a few thousand characters: the agent's steps, the
decisions that apply to its reports, its reports' TOC rows and shells, and
the names of the files to read. A request longer than `max_chars` is split.

## 5. The agent's questions, your decisions, its drafts

1. Open `mystudy` in VS Code and give the agent's chat the text of
   `ai/agent_prompt.md`: the first one or two requests.
2. The agent **asks you**, numbered, what defines each report and is not
   settled -- the analysis set, a parameter, the treatment variable, the
   denominator of a percent, what is counted, missing values, a subgroup's
   condition -- each question saying the report's id and title, the
   choices and its default. **Answer in the chat**; "default" takes its
   default. It asks again when an answer does not settle the item, three
   times at most; then it takes the likeliest choice and marks it *to
   confirm*.
3. It records your answers in `ai/decisions.md`, one line a decision
   (`- D1 [all] treatment_var.SAF: TRT01A (chat, T-14-3-1)`), and applies
   them to the reports after -- without asking when it is sure the
   decision holds (its scope is `all`, or the report's dataset or TOC
   section, as `carry` says), asking "the same here?" when it is not. You
   can write lines of your own there before it starts (a line without a
   scope is `all`).
4. It writes each report's **skeleton** as `ai/spec/outputs/<id>.yml`:
   `id`, `kind`, `label`, `population`, `data`, `ard` (the analysis data,
   the analyses), the content (a template where one fits, else
   `table_plan` and `plan_cells`), the digits when the shell shows them,
   `page: titles` and the shell's footnotes (`footnotes_at`) -- and under
   `ai:`, its `assumptions` (each `from: D<n>`) and what is to `confirm`.
   No column widths, header rows, pages, borders or fonts: the study's
   defaults and your builder do those.
5. It stops and tells you the files written, the decisions added and what
   is to confirm. Check them (step 6), then ask for the next requests:
   `tfl_ai_agent_prompt("mystudy")` gives the message (the next ten
   without a draft).

A question about the work rather than a report (a file missing, a request
unclear) goes to `ai/questions.md` too: `tfl_ai_questions()` reads it,
`tfl_ai_answer_questions()` writes your answers.

## 6. Check the drafts, write the YAML, make the RTF

```r
res <- tfl_ai_collect_answers(".")   # read the drafts, check, write them anew
res                                  # each request: its status, reports and next step
```

tflplanner's "Check" does the same. Each request's drafts are read as its
answer and checked against the request -- the schema, the analyses, every
dataset and variable against the ADaM summary, the study's files -- and
written anew: the column widths filled, the assumptions numbered
(`spec/_assumptions.yml`), the `ai:` block kept. The print gives, for each
request, its status (`ok`, `warning`, `error`, `no answer`), its reports
and **the next step**, then the first problem of each report. A draft with
errors is left as it is; its repair prompt is `ai/repairs/<request>.md`.
Paste it into **the agent's chat**: it fixes the draft; then run
`tfl_ai_collect_answers(".")` again (2-3 rounds are usual). A draft whose
YAML cannot be read is refused with why: tell the agent, it fixes the form.

`tfl_ai_confirm(".")` lists what the agent marked to confirm (the report,
the item, why): decide those in the builder. They do not stop a draft from
being adopted; `tfl_check_spec()` shows them as `check` rows until they are
decided.

The print counts the reports, and `ai/manifest.csv` lists every one with
its status: check the count against the TOC (one TOC row can be several
reports).

Then the RTF of every report written:

```r
tfl_ai_run(".")   # check again, then each report's ARD and RTF: ai/output/tfl/<id>.rtf
```

It writes `ai/status.md` (each report: its status, its RTF or why none, the
next step). One report's failure does not stop the others. `Rscript run.R`
does the same from the command line.

| Function | Does |
|---|---|
| `tfl_ai_prepare()` | the study folder, the ADaM summary, the workbook read, the requests, the agent's files |
| `tfl_ai_write_requests()` | one file a request (`size`, `max_chars`) |
| `tfl_ai_agent_prompt()` | the message that asks the agent for the next requests |
| `tfl_ai_collect_answers()` | reads `ai/spec/outputs/`, checks each request's drafts, writes them anew |
| `tfl_ai_confirm()` | what the agent marked to confirm |
| `tfl_ai_run()` | checks, then each report's ARD and RTF; `ai/status.md` |
| `tfl_ai_adopt()` | a checked draft into the study's `spec/outputs/` |
| `tfl_ai_compare()` | one row a report: drafted with a template or a code SPEC, its values, check, repairs (`ai/compare.csv`) |
| `tfl_build_ard()` | the ARD; one report at a time to find an error: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | the table and its page → RTF |

**Read `spec/_assumptions.yml`**: what the agent took from a decision
(`from: D<n>`) or, after three rounds, on its own. `tfl_content_code(o)`
and `tfl_ard_code(st, output_id = id)` show the code.

A draft you have checked is adopted with `tfl_ai_adopt()`: copied to the
study's `spec/outputs/`, where the report list shows it (the agent never
writes over what was adopted). The folder opens in tflplanner as a study
(its `spec/toc.yml` lists the reports). A study of the form before (its
drafts in `spec/outputs/`) moves with `tfl_ai_migrate()`.

## A whole study (up to ~200 tables)

1. **Start small**: one request of each family (`T-14-1-1`, `T-14-3-1`
   ...), and check its RTF against the shell.
2. **Fix one table of each family by hand**, then write the requests again
   with them as examples, named by family -- each request gets its own
   family's, and the agent follows it:
   `tfl_ai_prepare(..., examples = c(demographics = "spec/outputs/T-14-1-1.yml", ae = "spec/outputs/T-14-3-1.yml"), overwrite = TRUE)`.
3. **Ask for more at a time** (`tfl_ai_agent_prompt(n = 10)`) while the
   drafts stay right; `size = 3` to `5` groups a family's reports in one
   request, their questions asked together.
4. **Your decisions carry**: a decision of scope `all` or a dataset holds
   for every report after; the agent asks once a family, not once a report.
5. `tfl_ai_collect_answers(".")` any time: it reads every draft there and
   lists what is left (`no answer`, `error`, to confirm).

## The second round: corrections

When the drafts are checked, correct the list of reports when it needs it
(`tfl_ai_outputs()`, `tfl_ai_expected_write()`: `ai/expected_outputs.csv`),
say which of the agent's assumptions are right or wrong and what is right
instead (`tfl_ai_assumptions()`, `tfl_ai_assumption_answers_write()`:
`ai/assumption_answers.csv`; for one report, or every report of its
kind), gather them with `tfl_ai_corrections()` (`ai/corrections.md`), and
tell the agent *"the corrections are written"*: it adds each answer to
`ai/decisions.md`, does each correction, and redoes the drafts they touch.

## Templates or the code SPEC (`templates`)

By default (`templates = TRUE`) each request asks the agent to draft a
report with one of the study's templates where one fits (`template: {name,
args}`, only the arguments that differ from the default, and the report's
own content items for what the template does not do), else as a code SPEC.
`ai/templates.md` lists the templates registered when the requests were
written -- tflspec's, the company's, the study's variants -- with their
arguments, and each request names the ones likely for its reports.
`templates = FALSE`: the code SPEC only. The setting is each request's
first line (`<!-- templates: TRUE -->`) and `ai/status.md`'s.
`tfl_ai_compare()` says, a row a report, which was used.

## Try it on the sample (10 minutes)

The files that come with tflspec: a TOC/SHELL workbook of four tables and a
Kaplan-Meier figure, the sample ADaM as XPT, and a stored draft for each of
its five reports (what an agent wrote). The installation is ready when the
last line prints `ok` for all five reports.

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
ex <- system.file("ai", "example", package = "tflspec")
r  <- tfl_ai_prepare(d, file.path(ex, "onco_toc_shell.xlsx"), adam = "adam",
                     study_id = "XYZ-ONC-01", size = 3, overwrite = TRUE)
# the stored drafts stand in for the agent's: one a report
invisible(file.copy(list.files(file.path(ex, "drafts"), full.names = TRUE),
                    file.path(d, "ai", "spec", "outputs"), overwrite = TRUE))
tfl_ai_collect_answers(d)            # 5 reports in 5 requests (the figure its own): ok 5 ...
```

Keep the `ai/` folder (requests, decisions, drafts) -- share it only within
your company.

## When it does not work

| What you see | Do |
|---|---|
| `r$toc` is empty: the TOC sheet was not found | `tfl_ai_prepare(..., toc_sheet = "<the TOC sheet's name>")` |
| `r$toc$shell` is `NA`: no shell found by the report's number | add that sheet: `tfl_ai_prepare(..., sheets = c("<shell sheet>", ...))` |
| the agent writes a draft without asking, or asks what a decision settles | its `AGENTS.md` was changed or not read: `tfl_ai_prepare(..., overwrite = TRUE)` writes it again; open the study folder itself in VS Code |
| `error`, "the YAML cannot be read", in `tfl_ai_collect_answers()` | tell the agent: it fixes the file's form (`ai/spec/outputs/<id>.yml`) |
| "Not read (no request asks for them)" | a draft named after no report of the TOC: the ids are in `ai/requests/index.md` |
| The shells as text look wrong (cells missed or out of order) | let the agent read the workbook: `tfl_ai_prepare(..., agent_reads = "workbook")` -- each request then says where its shells are in the file |
| `print(r)` ends with "Next: no TOC found" or "no shell found" | pass what it names: `toc_sheet = "<sheet>"` or `sheets = c(...)` |
| the same error after 2-3 repairs | correct `ai/spec/outputs/<id>.yml` by hand (or in the builder), then run step 6 |
| `tfl_build_ard()` stops: `The package "broom" (>= 1.0.8) is required.` | `install.packages("broom")` (another package named so: install it too) |
| `tfl_build_ard()` stops | find the report: `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)` |
