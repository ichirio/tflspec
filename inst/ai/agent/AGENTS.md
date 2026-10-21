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
      (the header, `outputs`, `assumptions`). A request names the files it
      shares with the others -- `ai/instructions-table.md` or
      `ai/instructions-figure.md` (the instructions and the schema),
      `ai/adam_summary.md`, `spec/data.yml`, `spec/defaults.yml` -- read each
      once and follow it for every request.
   2. Save the whole answer as `ai/answers/<request>.md` (UTF-8).
   3. Run `Rscript run.R <request>` in a terminal (from this folder, or
      `Rscript <this folder>/run.R <request>`: it finds its folder). It
      reads every answer, checks it, writes `spec/outputs/<id>.yml`, builds
      the ARD and the RTF (`output/tfl/<id>.rtf`) of that request's
      reports and writes `ai/status.md`. `Rscript run.R` alone builds every
      report that changed since the last run.
   4. Read `ai/status.md`. When it gives your request a repair
      (`ai/repairs/<request>.md`) or a build problem, fix the answer as it
      says, save the whole answer over `ai/answers/<request>.md`, and run
      again. Three tries at most; then leave it, add a question to
      `ai/questions.md` saying what is wrong, and go on.
3. The shells: use the text each request gives (read from the TOC / SHELL
   workbook). Each request also says where its reports' shells are in the
   workbook file -- the file, the sheet, the cells, the TOC row. Only when
   the text looks cut or garbled, read **that place alone** with R:
   `Rscript -e "cat(format(tflspec::tfl_ai_read_shell('<file>', outputs = '<id>')))"`
   or `readxl::read_excel('<file>', sheet = '<sheet>', range = '<cells>')`.
   Never read another sheet on a guess; when the place looks wrong (another
   report's shell, an empty range), ask it in `ai/questions.md`.
4. The variables and their values are in `ai/adam_summary.md`; work from
   it. You may read the ADaM itself (where `spec/data.yml` says, `path:`)
   when a question about the data is not settled by the summary -- a value
   as the data write it, whether a variable is there, an arm's levels.
   There is no need to read it otherwise. Read it with R, never by opening
   a `.xpt` / `.sas7bdat` in the editor, and print only what settles the
   question -- a variable's values, labels, counts -- not subjects'
   records in bulk:
   `Rscript -e "d <- haven::read_xpt('<path>/adae.xpt'); print(table(d$ACNDR, useNA = 'ifany'))"`.
5. A question you cannot settle from the request, its shell, the summary,
   the data or `ai/decisions.md`: **do not stop**. Take the likeliest
   choice and write it as an assumption with its `question` (the request's answer format says
   how). `run.R` gathers the questions into `ai/questions.md`, a question
   several reports ask once, with the reports listed. A question about the
   work rather than a report (a shell you cannot find, a file missing): add
   it to `ai/questions.md` yourself, in the same form (`## Q<n>. ...`,
   `- Reports:`, `- Where:`, `- The agent took, for now:`, `- Status: open`,
   `- Answer:`).
6. When every request is done (`ok`, `warning`, or left after three tries),
   stop and tell the person: how many reports have an RTF, which do not and
   why, and that `ai/questions.md` waits for their answers.

## Round 2: the person's answers

When the person says the questions are answered:

1. Read `ai/questions.md`. For each question with `- Status: open` and an
   answer after `- Answer:`:
   1. Add one line to `ai/decisions.md`:
      `- Q<n> (<the reports>): <the decision, in a sentence>`.
   2. Change its `- Status: open` to `- Status: decided`.
2. Redo each request with a report a new decision touches: change its answer
   (`ai/answers/<request>.md`) as `ai/decisions.md` says, save the whole
   answer, and run `Rscript run.R`; repair as in round 1.
3. Tell the person what changed, and the questions still open.

## Never

- Do not open a data file (`.xpt`, `.sas7bdat`) in the editor, and do not
  print subjects' records in bulk: read the ADaM with R, only for a
  question the summary does not settle (round 1, step 4).
- Do not read a sheet of the TOC / SHELL workbook that no request names
  for its report (round 1, step 3).
- Do not change `study.yml`, `spec/data.yml`, `spec/defaults.yml`, `run.R`,
  `AGENTS.md` or `.github/copilot-instructions.md`. When one seems wrong,
  ask in `ai/questions.md`.
- Write only `ai/answers/`, `ai/questions.md` and `ai/decisions.md` (add
  lines; change nothing a person wrote). Fix a report through its answer,
  not by editing `spec/outputs/`.
- Do not install packages or go online: `run.R` uses the R and the packages
  installed.
