# Drafting tables' code spec with an AI chat -- a guide for the tester

This guide takes a study's **TOC/SHELL workbook** and its **ADaM (XPT)** to
**report files** (`spec/outputs/<id>.yml`, the code spec) and from them to
**RTF tables**, with a chat AI your company allows. Every step runs in R on
your PC; tflspec sends nothing anywhere -- you copy text into the chat and
copy the answer back.

The same guide in Japanese: `tflspec-ai-table-guide-ja.md` in the same
folder (`system.file("ai", package = "tflspec")`).

## What goes to the AI, and what never does

| Goes into the chat | Never |
|---|---|
| the prompt `tfl_ai_prompt()` writes: the instructions and schema, the **text of the TOC and shells** (or the workbook itself, if your chat reads Excel and you prefer it), the **ADaM summary** (dataset and variable names, labels, types, and the values of short code lists such as SEX or PARAMCD), your study's `spec/data.yml` and `spec/defaults.yml` | the **XPT files** or any other data with records; subject ids, dates, results |

Read `ai/adam_summary.md` once before the first request: it lists no
record, but a short list of values (up to 15, each under 40 characters) is
shown for character variables. Remove anything your company does not allow
to be sent.

## 0. Install (once)

```r
# from GitHub (or your company's mirror of it)
remotes::install_github("ichirio/rtfreporter")
remotes::install_github("ichirio/tflspec")
install.packages(c("cards", "cardx", "haven"))   # the ARD and the XPT reader
```

## 1. Try it on the sample first (10 minutes)

This runs the whole path with the files that come with tflspec: a made-up
TOC/SHELL workbook of four tables (demographics, best overall response and
ORR, AEs by SOC/PT, a laboratory shift), the sample ADaM written as XPT,
and a stored answer of the AI. If it ends with four RTF files, the
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
xlsx <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)
adam  <- tfl_ai_adam_summary(file.path(d, "adam"))
shell <- tfl_ai_read_shell(xlsx)
ctx   <- tfl_ai_context("table", outputs = shell$toc$output_id, shell = shell,
                        datasets = adam, study_dir = d)
p     <- tfl_ai_prompt("table", ctx)          # what you would paste
answer <- readLines(system.file("ai", "example", "onco-answer.md", package = "tflspec"))
a <- tfl_ai_parse(answer, "table")
tfl_ai_check(a, ctx)                          # no rows: no problems
tfl_ai_unpack(a, d, overwrite = TRUE)

st   <- tfl_read_study_spec(d)
outs <- tfl_read_outputs(d)
ard  <- tfl_build_ard(st, dir = d, save = FALSE)
for (id in names(outs)) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, subset(ard, output_id == id))
  rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st),
                                  file.path(d, paste0(id, ".rtf")), overwrite = TRUE)
}
list.files(d, pattern = "rtf$")
```

## 2. Make the study folder

```text
mystudy/
  TOC_SHELL.xlsx        your company's TOC and shells (one workbook)
  adam/                 the ADaM as XPT: adsl.xpt, adae.xpt, adlb.xpt ...
```

```r
library(tflspec)
setwd("mystudy")
dir.create("ai", showWarnings = FALSE)        # the prompts and answers
tfl_ai_study_init(".", adam = "adam", study_id = "ABC-123")
```

`tfl_ai_study_init()` writes `study.yml`, `spec/data.yml` (each XPT as a
dataset; an analysis set for each population flag of ADSL -- SAFFL, FASFL,
ITTFL ... -- as `<flag> == "Y"`) and `spec/defaults.yml` (the page header
and footer, the cell of a count). **Open `spec/data.yml` and check the
analysis sets** (their ids and labels): the AI names them by id.

## 3. Summarise the ADaM, read the workbook

```r
adam  <- tfl_ai_adam_summary("adam", file = "ai/adam_summary.md")
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx", file = "ai/toc_shell.md")
shell                       # the sheets, the reports found in the TOC, their shells
shell$toc[c("output_id", "title", "shell")]
```

Check what it found:

* **The TOC's reports** (`shell$toc`). If the TOC sheet was not found, say
  which it is: `tfl_ai_read_shell("TOC_SHELL.xlsx", toc_sheet = "Index")`.
* **Each report's shell** (`shell` column). `NA` means no sheet names the
  report's number; give the sheets yourself in step 5 (`sheets =`).
* `ai/toc_shell.md` is the text the AI reads: one line a row,
  `<row>: <column>=<text> | ...`, a merged cell once (`A1:E1=`), the indent
  as spaces.

## 4. Split the tables into batches

The prompt is made for **5 to 10 tables a request**, of one family:

```r
b <- tfl_ai_batches(shell, size = 8)
b                           # output_id, title, family, batch
table(b$batch)
```

Families: disposition, demographics, exposure, ae, labs, vitals, efficacy,
pk, other -- by the words of the title, English or Japanese. A family of
your company's own: `tfl_ai_batches(shell, families = c(pro = "(?i)EQ-5D|QLQ"))`.

## 5. Make the prompt of one batch

```r
ids <- b$output_id[b$batch == "demographics-1"]
ctx <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                      study_dir = ".", language = "Japanese")
p <- tfl_ai_prompt("table", ctx)
p$nchar                                   # its size
writeLines(format(p), "ai/prompt_demographics-1.md", useBytes = TRUE)
```

* The prompt holds only these reports' rows of the TOC and their shells.
  When a shell was not found, add its sheet:
  `tfl_ai_read_shell("TOC_SHELL.xlsx", outputs = ids, sheets = "DM shells")`
  and pass that as `shell`.
* `language = "Japanese"`: the AI writes its assumptions in Japanese (the
  prompt itself is English).
* **A reviewed example of the family.** Once the first table of a family
  is right (corrected by hand if need be), make the rest follow it:
  `tfl_ai_context("table", outputs = rest, ..., examples = "spec/outputs/T-14-3-1.yml")`.

## 6. Paste it into the chat, save the answer

1. Open `ai/prompt_demographics-1.md`, copy everything, paste it as one
   message into a **new chat**. If the message box is too small, attach the
   file and write "Follow the attached instructions."; or put the part
   before the line `---` into the chat's system / custom instructions and
   send the rest.
2. The AI answers with one `yaml` block. Copy the **whole answer** into a
   text file, `ai/answer_demographics-1.md` (UTF-8).

## 7. Check the answer

```r
a  <- tfl_ai_parse(readLines("ai/answer_demographics-1.md", encoding = "UTF-8"), "table")
a                                         # the reports read, the assumptions
pr <- tfl_ai_check(a, ctx)
pr                                        # where, problem, severity
```

`tfl_ai_check()` reads each report file as tflspec does, checks every
content item against the schema (an unknown function or argument, a wrong
type, a required argument missing), every analysis's fields and method,
the analysis sets, every dataset and variable against the ADaM summary,
and writes the ARD program and each table's code (nothing is run).

**When there are errors**, send them back in the same chat:

```r
writeLines(tfl_ai_repair_prompt(a, pr), "ai/repair_demographics-1.md", useBytes = TRUE)
```

Paste the file as the next message, save the new answer over
`ai/answer_demographics-1.md`, and check again. Two or three rounds are
usual; warnings are worth a look but do not block. If the same error comes
back, correct the YAML by hand after step 8.

## 8. Write the report files

```r
tfl_ai_unpack(a, ".")                     # spec/outputs/<id>.yml, spec/_assumptions.yml
```

Each batch adds its files; its assumptions join `spec/_assumptions.yml`
(each item: `output_id`, `where`, `item`, `assumption`, `question`).
**Read the assumptions**: they are the AI's guesses (the arm variable, a
flag, a visit, a denominator). `overwrite = TRUE` writes a batch again.

## 9. Run the tables

```r
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

* An error in `tfl_build_ard()`: run the reports one at a time to find it,
  `tfl_build_ard(st, dir = ".", output_id = "T-14-1-1", save = FALSE)`;
  paste the error message into the chat ("this error came from T-14-1-1;
  correct its file") or correct the file by hand.
* `cat(tfl_content_code(o), sep = "\n")` shows the table's code, and
  `tfl_ard_code(st, output_id = id)` the ARD program, to read or keep.

## 10. What to note for the feedback

For each batch: the family and the number of tables; how many check rounds;
the errors that came back more than once; what you corrected by hand; how
long it took; and whether the RTF matched the shell. Keep the `ai/` folder
(prompts, answers, repair prompts) -- but share it only within your company.
