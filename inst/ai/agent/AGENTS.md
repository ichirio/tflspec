# tflspec: this study's report specifications, by an agent

You draft the specification (YAML) of this study's tables from the requests
in `ai/requests/`. tflspec, an R package, turns it into the programs and the
RTF files. Work through **every** request without stopping to ask; what you
cannot settle, take the likeliest choice, say so, and go on -- the questions
are gathered for the person in `ai/questions.md`.

The paths below are from this folder: the one holding this file, `run.R`
and `ai/`. If the workspace is a folder above it, use the paths from here.

## Round 1: every request

1. Read `ai/decisions.md` first: it overrides your own choices. Then
   `ai/requests/index.md`: the requests, in order.
2. For each request `ai/requests/<request>.md`, in that order:
   1. Read it, and write the answer it asks for: one ```` ```yaml ```` block
      (the header, `reports`, `outputs`, `assumptions`). A request names the files it
      shares with the others -- `ai/instructions-table.md` or
      `ai/instructions-figure.md` (the instructions and the schema),
      `ai/adam_summary.md`, `spec/data.yml`, `spec/defaults.yml` -- read each
      once and follow it for every request.
   2. Save the whole answer as `ai/answers/<request>.md` (UTF-8).
   3. Run `Rscript run.R <request>` in a terminal (from this folder, or
      `Rscript <this folder>/run.R <request>`: it finds its folder). It
      reads every answer, checks it, writes `spec/outputs/<id>.yml`, builds
      the ARD and the RTF (`output/tfl/<id>.rtf`) of that request's
      reports and writes `ai/status.md`. (When the study has
      `ai/spec/outputs/`, your report files go there and their RTF under
      `ai/output/`: drafts, which the person adopts into `spec/outputs/`.
      Never write `spec/outputs/` yourself.) `Rscript run.R` alone builds every
      report that changed since the last run.
   4. Read `ai/status.md`. When it gives your request a repair
      (`ai/repairs/<request>.md`) or a build problem
      (`ai/repairs/<request>-build.md`), fix the answer as it
      says, save the whole answer over `ai/answers/<request>.md`, and run
      again. Three tries at most; then leave it, add a question to
      `ai/questions.md` saying what is wrong, and go on.
3. The shells: use the text each request gives, read from the TOC / SHELL
   workbook {{workbook}}. Each request also says where its reports' shells
   are in the workbook file -- the file, the sheet, the cells, the TOC row.
   When you can open the workbook, use it to check what the text leaves
   unclear -- above all a shell's shape: several parts on one sheet, a
   later part that writes only what differs, merged cells, a text that
   looks cut. Read the place the request names with R:
   `Rscript -e "cat(format(tflspec::tfl_ai_read_shell('<file>', outputs = '<id>')))"`
   or `readxl::read_excel('<file>', sheet = '<sheet>', range = '<cells>')`.
   When you cannot open it (outside your workspace, no permission), decide
   from the request alone and write what you took as an assumption. When
   the place looks wrong (another report's shell, an empty range), ask it
   in `ai/questions.md`.
4. The variables and their values are in `ai/adam_summary.md`; work from
   it first. The ADaM is in {{adam}} (`spec/data.yml` names each file,
   `path:`). When you can open it, you may read the ADaM itself when a
   question about the data is not settled by the summary -- a value as the
   data write it, whether a variable is there, an arm's levels. Read it
   with R, never by opening a `.xpt` / `.sas7bdat` in the editor, and print
   only what settles the question -- a variable's values, labels, counts
   -- not subjects' records in bulk:
   `Rscript -e "d <- readRDS('<path>/adae.rds'); print(table(d$ACNDR, useNA = 'ifany'))"`
   (`haven::read_xpt()` for a `.xpt`, as `spec/data.yml`'s `path:` says).
   When you cannot open it, decide from the summary and the request.
5. A question you cannot settle from the request, its shell, the summary,
   the data or `ai/decisions.md`: **do not stop**. Take the likeliest
   choice and write it as an assumption with its `question` (the request's answer format says
   how). Not so for the analysis set and, for a dataset of parameters
   (PARAMCD), the parameter: they define the report, and the shell says
   them. When you cannot read them, never take a likely one: ask the
   question, and leave the report with the error `run.R` gives it (no
   repair by guessing). `run.R` gathers the questions into `ai/questions.md`, a question
   several reports ask once, with the reports listed. A question about the
   work rather than a report (a shell you cannot find, a file missing): add
   it to `ai/questions.md` yourself, in the same form (`## Q<n>. ...`,
   `- Reports:`, `- Where:`, `- The agent took, for now:`, `- Status: open`,
   `- Answer:`).
6. When every request is done (`ok`, `warning`, or left after three tries),
   stop and tell the person: how many reports the answers list (`ai/manifest.csv`,
   to check against the TOC), how many have an RTF, which do not and
   why, and that `ai/questions.md` waits for their answers.

## Round 2: the person's answers and corrections

When the person says the answers and corrections are written:

1. Read `ai/corrections.md` (when there is none, read `ai/questions.md` for
   the questions answered, as below). For each section with
   `- Status: open`:
   - `add <id>`: write the report from its sheet and TOC item, as round 1
     does, and list it in the request's answer (`reports:` and `outputs`).
   - `drop <id>`: take the report out of the answer (both lists); do not
     write it.
   - `change <id>: ...`: change what it says in the report (a new id:
     rename the report and every mention of it).
   - `note <id>`: change the report as the note says.
   - `Q<n> (answered)`: add one line to `ai/decisions.md`,
     `- Q<n> (<the reports>): <the decision, in a sentence>`; set the
     question's `- Status: open` to `- Status: decided` in
     `ai/questions.md`; then change every report it names.
2. Save each changed answer whole over `ai/answers/<request>.md` and run
   `Rscript run.R <request>`; repair as in round 1. Then set the section's
   `- Status: open` to `- Status: done`. A section you cannot do: leave it
   `- Status: open` and add a line `- Agent:` saying why.
3. Tell the person what changed, the sections left and why, the questions
   still open, and the count of the reports against their list
   (`ai/status.md`, "Expected outputs").

## Never

- Do not open a data file (`.xpt`, `.sas7bdat`) in the editor, and do not
  print subjects' records in bulk: read the ADaM with R, only for a
  question the summary does not settle (round 1, step 4).
- Do not change the TOC / SHELL workbook or the ADaM: read them only to
  check (round 1, steps 3 and 4).
- Do not change `study.yml`, `spec/data.yml`, `spec/defaults.yml`, `run.R`,
  `AGENTS.md` or `.github/copilot-instructions.md`. When one seems wrong,
  ask in `ai/questions.md`.
- Write only `ai/answers/`, `ai/questions.md`, `ai/decisions.md` and the
  `- Status:` / `- Agent:` lines of `ai/corrections.md` (add lines; change
  nothing a person wrote). Never write `ai/expected_outputs.csv`: it is the
  person's list. Fix a report through its answer,
  not by editing `spec/outputs/`.
- Do not install packages or go online: `run.R` uses the R and the packages
  installed.
