# Run the study's AI answers through: collect, build, status, questions

What a coding agent runs after each answer it saves (`run.R` of
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)`(mode = "agent")`
calls it):
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
(read, check, write the report files, a repair prompt for an answer with
errors), then each report written (`ok`, `warning`) built – its ARD
([`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md))
and its RTF (rtfreporter) into the study's `output_path` – one report's
failure not stopping the others. It writes `ai/status.md` (each report's
status, its RTF or why none, each request's next step, the questions
open) and gathers the answers' questions (an assumption's `question`)
into `ai/questions.md`: one question once, with every report that asks
it; a question there already is kept as it is – its answer, its status –
with the new reports added to its list. Nothing is sent anywhere.

## Usage

``` r
tfl_ai_run(study_dir = ".", rtf = TRUE, requests = NULL, rebuild = FALSE)
```

## Arguments

- study_dir:

  The study folder.

- rtf:

  `FALSE` to build the ARDs only (no rtfreporter).

- requests:

  The requests whose reports are built (`"ae-1"`); `NULL` (the default):
  every request's. Every answer is still read and checked, and
  `ai/status.md` lists them all. `Rscript run.R ae-1` passes them.

- rebuild:

  `TRUE` to build every report again, changed or not.

## Value

A `tfl_ai_collected` (as
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md))
whose `reports` also have `rtf` (the file made, else `NA`),
`rtf_problem` and `built` (`"built"`, `"kept"` – unchanged since the
last run – or `NA`), and a `questions` data frame (`n`, `question`,
`reports`, `assumptions`, `status`, `answer`), and `expected` (the
comparison with the person's list: `output_id`, `result` – `missing`,
`still there`, `differs`, `not expected` –, `detail`; its counts in its
attribute `text`; `NULL` without a list), invisibly.

## Details

A report is built again only when it changed: its report file, the
study's files (`study.yml`, `spec/data.yml`, `spec/defaults.yml`, its
templates), the ADaM it reads or the packages' versions – else its RTF
(and PNG) of the last run is kept, as `ai/built.csv` says. A report that
passed its checks but could not be built gets a repair
(`ai/repairs/<request>-build.md`) with why, which stays until the
request's reports are built; its RTF (and PNG) of an earlier run are
removed, and `ai/built.csv` keeps why, so that a run of other requests
still says it. A report not built yet (only other requests run) says so
in `ai/status.md`.

When the person keeps a list of the reports they expect
(`ai/expected_outputs.csv`,
[`tfl_ai_expected_write()`](https://ichirio.github.io/tflspec/reference/tfl_ai_expected_write.md)),
the reports made are compared with it, and `ai/status.md` says what
differs (a report missing, one dropped still there, a title or analysis
set not the list's, one the list does not have): told, never refused.

## See also

[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
(`mode = "agent"`),
[`tfl_ai_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_ai_outputs.md).
