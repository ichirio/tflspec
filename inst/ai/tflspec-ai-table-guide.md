# Drafting tables' code spec with an AI chat

## 1. What it does

From a study's **TOC/SHELL workbook** and its **ADaM (XPT)**, a chat AI
writes each table's **code spec** (`spec/outputs/<id>.yml`). tflspec checks
it and makes the **RTF**. Everything runs in R on your PC; tflspec sends
nothing — you copy a prompt into the chat and the answer back.

Japanese: `tflspec-ai-table-guide-ja.md` in the same folder
(`system.file("ai", package = "tflspec")`).

## 2. What goes where

```text
mystudy/
  TOC_SHELL.xlsx        your TOC and shells (one workbook)
  adam/                 adsl.xpt, adae.xpt, adlb.xpt ...
```

Install once: `remotes::install_github(c("ichirio/rtfreporter", "ichirio/tflspec"))`
and `install.packages(c("cards", "cardx", "haven"))`.

## 3. What to run

```r
library(tflspec)
setwd("mystudy")
dir.create("ai", showWarnings = FALSE)

# writes study.yml, spec/data.yml, spec/defaults.yml
tfl_ai_study_init(".", adam = "adam", study_id = "ABC-123")
adam  <- tfl_ai_adam_summary("adam", file = "ai/adam_summary.md")
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx", file = "ai/toc_shell.md")
b     <- tfl_ai_batches(shell, size = 8)          # 5-10 tables of one family a request

ids <- b$output_id[b$batch == "demographics-1"]
ctx <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                      study_dir = ".", language = "Japanese")
writeLines(format(tfl_ai_prompt("table", ctx)), "ai/prompt_demographics-1.md",
           useBytes = TRUE)
```

Check before the first request:

| Look at | Why |
|---|---|
| `spec/data.yml` | the analysis sets (one per ADSL flag `SAFFL`, `FASFL` ...): the AI names them by id |
| `shell$toc` | the reports found; a TOC sheet not found: `tfl_ai_read_shell(..., toc_sheet = "Index")` |
| `shell$toc$shell` | `NA` = no shell sheet found: `tfl_ai_read_shell(..., outputs = ids, sheets = "DM shells")` |
| `ai/adam_summary.md` | no records, but short value lists (up to 15): remove what your company does not allow |

## 4. What to give the AI

| Give | Never |
|---|---|
| `ai/prompt_<batch>.md` — the whole file, as one message in a **new chat** (too long: attach it and write "Follow the attached instructions.") | the **XPT files**, or any data with records: subject ids, dates, results |

The prompt already holds the instructions, the TOC and shell text of these
reports, the ADaM summary, `spec/data.yml` and `spec/defaults.yml`.

## 5. Where to put the AI's answer

Copy the **whole answer** (one `yaml` block) into `ai/answer_<batch>.md` (UTF-8).

## 6. Check it, write the YAML, make the RTF

```r
a  <- tfl_ai_parse(readLines("ai/answer_demographics-1.md", encoding = "UTF-8"), "table")
pr <- tfl_ai_check(a, ctx)                 # schema, analyses, variables; nothing is run
pr                                         # no rows: no problems

# errors: send them back in the same chat, save the new answer over the old, check again
writeLines(tfl_ai_repair_prompt(a, pr), "ai/repair_demographics-1.md", useBytes = TRUE)

tfl_ai_unpack(a, ".")                      # spec/outputs/<id>.yml, spec/_assumptions.yml

st   <- tfl_read_study_spec(".")
outs <- tfl_read_outputs(".")
ard  <- tfl_build_ard(st, dir = ".", save = FALSE)
dir.create("output", showWarnings = FALSE)
for (id in ids) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, subset(ard, output_id == id))
  rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st),
                                  file.path("output", paste0(id, ".rtf")), overwrite = TRUE)
}
```

| Function | Does |
|---|---|
| `tfl_ai_parse()` | reads the answer: the reports and the AI's assumptions |
| `tfl_ai_check()` | checks every item against the schema, the analysis sets and the ADaM summary |
| `tfl_ai_repair_prompt()` | the errors as the next message (2-3 rounds are usual) |
| `tfl_ai_unpack()` | writes `spec/outputs/<id>.yml`; `overwrite = TRUE` writes a batch again |
| `tfl_build_ard()` | the ARD; one report at a time to find an error: `output_id = "T-14-1-1"` |
| `tfl_content_run()`, `tfl_report()` | the table and its page → RTF |

**Read `spec/_assumptions.yml`**: the AI's guesses (arm variable, flag,
visit, denominator). `tfl_content_code(o)` and `tfl_ard_code(st, output_id = id)`
show the code, to read or keep.

**The next tables of a family** follow a reviewed one:
`tfl_ai_context("table", outputs = rest, ..., examples = "spec/outputs/T-14-3-1.yml")`.

## Try it on the sample (10 minutes)

The files that come with tflspec: a TOC/SHELL workbook of four tables, the
sample ADaM as XPT, and a stored answer. Four RTF files at the end mean the
installation is ready.

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
ctx   <- tfl_ai_context("table", outputs = shell$toc$output_id, shell = shell,
                        datasets = tfl_ai_adam_summary(file.path(d, "adam")), study_dir = d)
a <- tfl_ai_parse(readLines(file.path(ex, "onco-answer.md")), "table")
tfl_ai_check(a, ctx)
tfl_ai_unpack(a, d, overwrite = TRUE)
# then step 6 from `st <- tfl_read_study_spec(d)`, with ids <- names(outs) and the RTF in d
```

Keep the `ai/` folder (prompts, answers) — share it only within your company.
