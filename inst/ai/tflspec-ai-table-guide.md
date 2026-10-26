# Drafting tables' code spec with an AI chat

## 1. What it does

From a study's **TOC/SHELL workbook** and its **ADaM (XPT)**, a chat AI
writes each table's **code spec** (`spec/outputs/<id>.yml`). tflspec checks
it and makes the **RTF**. Everything runs in R on your PC; tflspec sends
nothing — you paste a request into the chat and save the answer.

Japanese: `tflspec-ai-table-guide-ja.md` in the same folder
(`system.file("ai", package = "tflspec")`).

## 2. What goes where

```text
mystudy/
  TOC_SHELL.xlsx        your TOC and shells (one workbook)
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
  ai/requests/          written by tflspec: one file a request (step 3)
  ai/answers/           you save the AI's answers here (step 5)
  ai/repairs/           written by tflspec: what to send back (step 6)
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
setwd("mystudy")
# writes study.yml, spec/data.yml, spec/defaults.yml
tfl_ai_study_init(".", adam = "adam", study_id = "ABC-123")
adam  <- tfl_ai_adam_summary("adam", file = "ai/adam_summary.md")
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx", file = "ai/toc_shell.md")

r <- tfl_ai_write_requests(".", shell, adam, size = 3, language = "Japanese")
r                                   # request, outputs, characters
```

This writes `ai/requests/<request>.md` (`demographics-1.md`, `ae-1.md` ...,
one family each) and `ai/requests/index.md`. A figure of the TOC is a request of
its own (`figure-1.md` ...): its shell, and a figure design to answer.
The TOC and the shells in workbooks apart: `tfl_ai_prepare("mystudy",
"TOC.xlsx", shells = "SHELLS.xlsx", ...)`; another workbook later (the
listings, a book of PK figures): the same call with `add = TRUE` -- the
requests and answers there are kept, the new requests added beside them.

The same in one call (an existing `study.yml` is kept, so it can be run
again; it prints what the workbook read found):

```r
r <- tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", study_id = "ABC-123",
                    size = 3, language = "Japanese")
```

When the workbook's text is not read right, **and only when the chat reads
an attached Excel file** (a chat that takes pasted text only needs the
default `"text"`): `workbook = "both"` (or `"attach"`) -- each request says where its
reports' shells are in the workbook (sheet, cells, TOC row), and you attach
`TOC_SHELL.xlsx` to each chat with the request; with `"both"` the text is
there too, the attachment right where they differ.

Check before the first request:

| Look at | Why |
|---|---|
| `spec/data.yml` | the analysis sets (one per ADSL flag `SAFFL`, `FASFL` ...): the AI names them by id |
| `shell$toc` | the reports found; a TOC sheet not found: `tfl_ai_read_shell(..., toc_sheet = "Index")` |
| `shell$toc$shell` | `NA` = no shell sheet found: `tfl_ai_read_shell(..., sheets = c("DM shells", ...))` |
| `ai/adam_summary.md` | no records, but short value lists (up to 15): remove what your company does not allow |

## 4. What to give the AI

| Give | Never |
|---|---|
| one `ai/requests/<request>.md` — the whole file, pasted as one message in a **new chat** | the **XPT files**, or any data with records: subject ids, dates, results |

Each request already holds the instructions, its reports' TOC rows and
shells, the ADaM summary, `spec/data.yml` and `spec/defaults.yml`.

**First, one request as it is.** A request is about 23,000 characters of
instructions plus its own reports (about 33,000 with one report).

1. Paste one request, as it is, into a new chat.
2. **If the chat will not take it** (the message is too long, or is cut),
   write the requests again with the instructions apart:

   ```r
   r <- tfl_ai_write_requests(".", shell, adam, size = 3, instructions = "once",
                              overwrite = TRUE)
   ```

   Then for each request: a **new chat**, paste `ai/requests/00-instructions.md`
   first, then the request (about 10,000 characters).
3. Still too long: a smaller `size`, or `max_chars = 20000` (a request over
   it is split).

## 5. Where to put the AI's answer

Copy the **whole answer** (one `yaml` block) into
`ai/answers/<the same name>.md`: the answer to `ae-1.md` is
`ai/answers/ae-1.md` (the folder is made by `tfl_ai_write_requests()`).

Save it as **UTF-8** (with or without BOM; Windows line ends are fine). In
Notepad: *File > Save As*, *Encoding: UTF-8*, *Save as type: All files*
(otherwise Notepad adds `.txt`; `ae-1.md.txt` is read too). A file saved
as ANSI / Shift-JIS is reported as an `error` saying so.

## 6. Check it, write the YAML, make the RTF

```r
res <- tfl_ai_collect_answers(".")   # parse, check, write spec/outputs/<id>.yml
res                                  # each request: its status, reports and next step
```

The print gives, for each request, its status (`ok`, `warning`, `error`,
`no answer`), its reports and **the next step**, then the first problem of
each report. An answer with errors writes no file; its repair prompt is
`ai/repairs/<request>.md`. Paste it into **the same chat**, save the new
answer over `ai/answers/<request>.md`, and run `tfl_ai_collect_answers(".")`
again (2-3 rounds are usual).

Each answer starts with the list of the reports it makes (`reports:`: id,
title, how its TOC row is split, the TOC row). The print counts them, and
`ai/manifest.csv` lists every one with its status: check the count against
the TOC (one TOC row can be several reports), and look at a report "listed
but not answered" or "answered but not listed".

Then the RTF of every report written:

```r
tfl_ai_run(".")   # collect again, then each report's ARD and RTF: ai/output/tfl/<id>.rtf
```

It writes `ai/status.md` (each report: its status, its RTF or why none, the
next step) and gathers the answers' questions into `ai/questions.md`. One
report's failure does not stop the others.

| Function | Does |
|---|---|
| `tfl_ai_write_requests()` | one file a request (`size`, `max_chars`, `instructions`) |
| `tfl_ai_collect_answers()` | reads `ai/answers/`, checks each against its request, writes the report files |
| `tfl_ai_run()` | collects, then each report's ARD and RTF; `ai/status.md`, `ai/questions.md` |
| `tfl_ai_compare()` | one row a report: answered with a template or a code SPEC, its values, check, repairs (`ai/compare.csv`) |
| `tfl_build_ard()` | the ARD; one report at a time to find an error: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | the table and its page → RTF |

**Read `spec/_assumptions.yml`**: the AI's guesses (arm variable, flag,
visit, denominator). `tfl_content_code(o)` and `tfl_ard_code(st, output_id = id)`
show the code.

## A whole study (up to ~200 tables)

1. **Start small**: `size = 2` or `3`. Do one request of each family
   (`demographics-1`, `ae-1`, `efficacy-1` ...) and check its RTF against the
   shell.
2. **Fix one table of each family by hand**, then write the requests again
   with them as examples, named by family — each request gets its own
   family's, and the AI follows it:
   `tfl_ai_write_requests(..., examples = c(demographics = "spec/outputs/T-14-1-1.yml", ae = "spec/outputs/T-14-3-1.yml"), overwrite = TRUE)`.
3. **Raise `size`** (5 to 8) while the answers stay right.
4. **How many requests**: the reports ÷ `size`, rounded up within each
   family — 200 tables at `size = 5` is about 40-45 requests, at `size = 8`
   about 25-30. `nrow(r$requests)` says it.
5. `tfl_ai_collect_answers(".")` any time: it reads every answer there and
   lists what is left (`no answer`, `error`).

## With a coding agent (GitHub Copilot in VS Code)

An agent works through every request without stopping, and gathers what it
cannot settle into one file for you.

1. Use the AI your company approves for the study's data. A chat
   (`mode = "chat"`) is sent the ADaM's summary only; the agent works in the
   study folder and may read the ADaM with R -- only to settle a question
   the summary does not (a value as the data write it, a variable, an
   arm's levels), never opening a data file in the editor.
2. `tfl_ai_prepare("mystudy", "mystudy/TOC_SHELL.xlsx", adam = "C:/data/ABC-123/adam", study_id = "ABC-123", mode = "agent")`.
   Besides the requests, the folder gets `AGENTS.md` and
   `.github/copilot-instructions.md` (the agent's instructions), `run.R`
   (`tfl_ai_run()`), `ai/questions.md` and `ai/decisions.md`.
3. Open `mystudy` in VS Code, and in the agent's chat: *"Follow AGENTS.md:
   do every request of ai/requests/index.md."* (A folder above it open:
   *"Follow mystudy/AGENTS.md ..."*.) For each request it saves an answer,
   runs `run.R`, reads `ai/status.md` and repairs, then goes on.
4. When it is done, answer `ai/questions.md` (one question asked by several
   reports is there once, with the reports). Correct the list of reports
   when it needs it (`tfl_ai_outputs()`, `tfl_ai_expected_write()`:
   `ai/expected_outputs.csv`), say which of the AI's assumptions are
   right or wrong and what is right instead (`tfl_ai_assumptions()`,
   `tfl_ai_assumption_answers_write()`: `ai/assumption_answers.csv`; for
   one report, or every report of its kind), gather the answers and corrections with
   `tfl_ai_corrections()` (`ai/corrections.md`), and tell the agent *"the
   answers and corrections are written"*: it moves each answer to
   `ai/decisions.md`, does each correction, and redoes the reports they
   touch.

In a new study the AI's report files are drafts in `ai/spec/outputs/`,
their RTF files in `ai/output/tfl/`. A draft you have checked is adopted
with `tfl_ai_adopt()`: copied to the study's `spec/outputs/`, where the
report list shows it (the AI never writes over what was adopted). The
folder opens in tflplanner as a study (its `spec/toc.yml` lists the
reports). A study of the form before (its drafts in `spec/outputs/`) moves
with `tfl_ai_migrate()`.

What every request would repeat is written once in `ai/` for the agent:
`ai/instructions-table.md` and `ai/instructions-figure.md` (the
instructions and the schema), `ai/templates.md` (the templates), next to
`ai/adam_summary.md`. Each request names them, so a request is a few
thousand characters.

## Templates or the code SPEC (`templates`)

`tfl_ai_prepare(templates = TRUE)` (the default) asks the AI to answer a
report with one of the study's templates where one fits
(`template: {name, args}`, only the arguments that differ), else as a code
SPEC. The templates are listed from the registry -- tflspec's, the
company's, the study's variants -- when the requests are written: in each
chat request, or in `ai/templates.md` for an agent, each request naming
the templates likely for its reports. A chat request over `max_chars`
lists the templates' arguments by name only. `templates = FALSE` asks for
the code SPEC only (the chat requests as before templates).

To set the two side by side, prepare the same TOC in two folders, one with
each setting, have each answered, then run `tfl_ai_compare(".")` in each:
one row a report -- what it was answered with (`template:<name>` or
`code`), how many values its report file holds, its check, its repairs --
written to `ai/compare.csv`.

## Try it on the sample (10 minutes)

The files that come with tflspec: a TOC/SHELL workbook of four tables and a
Kaplan-Meier figure, the sample ADaM as XPT, and a stored answer for each of
its five requests (`figure-1` is the figure's). The installation is ready
when the last line prints `ok` for all five reports.

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
ex    <- system.file("ai", "example", package = "tflspec")
shell <- tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
r     <- tfl_ai_write_requests(d, shell, tfl_ai_adam_summary(file.path(d, "adam")),
                               overwrite = TRUE)
# the stored answers stand in for the chat's: one a request
invisible(file.copy(list.files(file.path(ex, "answers"), full.names = TRUE),
                    file.path(d, "ai", "answers"), overwrite = TRUE))
tfl_ai_collect_answers(d)            # 5 reports in 4 requests (a shell sheet each, the figure its own): ok 5 ...
```

Keep the `ai/` folder (requests, answers) — share it only within your company.

## When it does not work

| What you see | Do |
|---|---|
| `shell$toc` is empty: the TOC sheet was not found | `tfl_ai_read_shell("TOC_SHELL.xlsx", toc_sheet = "<the TOC sheet's name>")` |
| `shell$toc$shell` is `NA`: no shell found by the report's number | add that sheet: `tfl_ai_read_shell("TOC_SHELL.xlsx", sheets = c("<shell sheet>", ...))` |
| the chat says the message is too long | `tfl_ai_write_requests(..., instructions = "once")`, or a smaller `size`, or `max_chars = 20000` |
| `error`, "no answer block found", in `tfl_ai_collect_answers()` | the answer was not copied whole: copy it again, from the first ```` ``` ```` to the last |
| `error`, "is not UTF-8" | save the answer again as UTF-8 (Notepad: *Save As*, *Encoding: UTF-8*) |
| "Not read (no request of that name)" | rename the answer to the request's name (`ai/requests/index.md`): `ae-1.md` |
| The shells as text look wrong (cells missed or out of order) | when the chat reads an attached Excel file only, attach the workbook: `tfl_ai_prepare(..., workbook = "both")` or `tfl_ai_write_requests(..., workbook = "both")`, and attach `TOC_SHELL.xlsx` to each chat |
| `print(shell)` ends with "Next: no TOC found" or "no shell found" | pass what it names: `toc_sheet = "<sheet>"` or `sheets = c(...)` |
| the same error after 2-3 repairs | correct `spec/outputs/<id>.yml` by hand, then run step 6 |
| `tfl_build_ard()` stops: `The package "broom" (>= 1.0.8) is required.` | `install.packages("broom")` (another package named so: install it too) |
| `tfl_build_ard()` stops | find the report: `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)` |
