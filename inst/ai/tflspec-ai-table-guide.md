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

Install once: `remotes::install_github(c("ichirio/rtfreporter", "ichirio/tflspec"))`
and `install.packages(c("cards", "cardx", "haven"))`.

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
one family each) and `ai/requests/index.md`.

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

A request is about 23,000 characters of instructions plus its own reports.
If the chat cannot take it in one message:
`tfl_ai_write_requests(..., instructions = "once")` writes the instructions
to `ai/requests/00-instructions.md`; paste that first in a new chat, then the
request. `max_chars = 30000` splits a long request further.

## 5. Where to put the AI's answer

Copy the **whole answer** (one `yaml` block) into
`ai/answers/<the same name>.md` (UTF-8): the answer to `ae-1.md` is
`ai/answers/ae-1.md`.

## 6. Check it, write the YAML, make the RTF

```r
res <- tfl_ai_collect_answers(".")   # parse, check, write spec/outputs/<id>.yml
res                                  # one row a report: ok / warning / error / no answer
```

An answer with errors writes no file; its repair prompt is
`ai/repairs/<request>.md`. Paste it into **the same chat**, save the new
answer over `ai/answers/<request>.md`, and run `tfl_ai_collect_answers(".")`
again (2-3 rounds are usual).

```r
st   <- tfl_read_study_spec(".")
outs <- tfl_read_outputs(".")
ard  <- tfl_build_ard(st, dir = ".", save = FALSE)
dir.create("output", showWarnings = FALSE)
for (id in res$output_id[res$status %in% c("ok", "warning")]) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, subset(ard, output_id == id))
  rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st),
                                  file.path("output", paste0(id, ".rtf")), overwrite = TRUE)
}
```

| Function | Does |
|---|---|
| `tfl_ai_write_requests()` | one file a request (`size`, `max_chars`, `instructions`) |
| `tfl_ai_collect_answers()` | reads `ai/answers/`, checks each against its request, writes the report files |
| `tfl_build_ard()` | the ARD; one report at a time to find an error: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | the table and its page → RTF |

**Read `spec/_assumptions.yml`**: the AI's guesses (arm variable, flag,
visit, denominator). `tfl_content_code(o)` and `tfl_ard_code(st, output_id = id)`
show the code.

## A whole study (up to ~200 tables)

1. **Start small**: `size = 2` or `3`. Do one request of each family
   (`demographics-1`, `ae-1`, `labs-1` ...) and check its RTF against the
   shell.
2. **Fix one table of each family by hand**, then write the requests again
   with them as examples, named by family — each request gets its own
   family's, and the AI follows it:
   `tfl_ai_write_requests(..., examples = c(demographics = "spec/outputs/T-14-1-1.yml", ae = "spec/outputs/T-14-3-1.yml"), overwrite = TRUE)`.
3. **Raise `size`** (5 to 8) while the answers stay right.
4. **How many requests**: the reports ÷ `size`, rounded up within each
   family — 200 tables at `size = 5` is about 40-45 requests, at `size = 8`
   about 25-30. `nrow(r)` says it.
5. `tfl_ai_collect_answers(".")` any time: it reads every answer there and
   lists what is left (`no answer`, `error`).

## Try it on the sample (10 minutes)

The files that come with tflspec: a TOC/SHELL workbook of four tables, the
sample ADaM as XPT, and a stored answer. A row `ok` or `warning` for the
report at the end means the installation is ready.

```r
library(tflspec)
d <- file.path(tempdir(), "sample-study")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
ex    <- system.file("ai", "example", package = "tflspec")
shell <- tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
r     <- tfl_ai_write_requests(d, shell, tfl_ai_adam_summary(file.path(d, "adam")),
                               overwrite = TRUE)
# the stored answer stands in for the chat's answer to the first request
dir.create(file.path(d, "ai", "answers"), showWarnings = FALSE)
file.copy(file.path(ex, "onco-answer.md"),
          file.path(d, "ai", "answers", paste0(r$request[1], ".md")), overwrite = TRUE)
tfl_ai_collect_answers(d)
```

Keep the `ai/` folder (requests, answers) — share it only within your company.

## When it does not work

| What you see | Do |
|---|---|
| `shell$toc` is empty: the TOC sheet was not found | `tfl_ai_read_shell("TOC_SHELL.xlsx", toc_sheet = "<the TOC sheet's name>")` |
| `shell$toc$shell` is `NA`: no shell found by the report's number | add that sheet: `tfl_ai_read_shell("TOC_SHELL.xlsx", sheets = c("<shell sheet>", ...))` |
| the chat says the message is too long | `tfl_ai_write_requests(..., instructions = "once")`, or a smaller `size`, or `max_chars = 20000` |
| `error`, "no answer block found", in `tfl_ai_collect_answers()` | the answer was not copied whole: copy it again, from the first ```` ``` ```` to the last |
| the same error after 2-3 repairs | correct `spec/outputs/<id>.yml` by hand, then run step 6 |
| `tfl_build_ard()` stops | find the report: `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)` |
