# Read the agent's drafts, check them, write the report files

The person's check of what the agent wrote
([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)):
each request's drafts (`ai/spec/outputs/<id>.yml`, one a report) are
read as the request's answer, checked against the request
([`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md):
the schema, the analyses, the ADaM summary, the study's files) and, when
they have no error, written anew as the study's report files – the
column widths filled, the assumptions numbered (`spec/_assumptions.yml`,
[`tfl_ai_assumptions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumptions.md)),
the draft's `ai:` block (its assumptions, what is marked to confirm:
[`tfl_ai_confirm()`](https://ichirio.github.io/tflspec/reference/tfl_ai_confirm.md))
kept. A draft with errors is left as it is: its repair prompt
([`tfl_ai_repair_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_repair_prompt.md))
goes to `repairs`, to paste into the agent's chat; check again when the
agent has fixed it. A draft YAML cannot read is refused with why, for
the agent to fix its form.

## Usage

``` r
tfl_ai_collect_answers(
  study_dir,
  requests = file.path("ai", "requests"),
  repairs = file.path("ai", "repairs"),
  overwrite = TRUE
)
```

## Arguments

- study_dir:

  The study folder.

- requests, repairs:

  The folders, under `study_dir`.

- overwrite:

  `TRUE` (default) to write over report files already there: a draft
  checked again replaces its report.

## Value

A `tfl_ai_collected`: a list with `reports` (a data frame, one row a
report: `request`, `output_id`, `status` – `ok`, `warning`, `error`,
`no answer` (no draft yet) –, `problems` (how many), `first` (the first
problem), `problems_text` (all of them, one a line), `file` (the
report's `ai/spec/outputs/<id>.yml` when written, else `NA`), `repair`
(the request's repair prompt, else `NA`)), `next_steps` (a request's
next step, named by the request), `next_kind` and `next_args` (the same
step as a kind and its values, for a screen to say in its own words –
see *The next step's kinds*), `assumptions` (the study's
`spec/_assumptions.yml` for these reports: `output_id`, `where`, `item`,
`assumption`, `value`, `question`, `id` – the assumption's `A<n>` – and
`kind` – its `K<n>`,
[`tfl_ai_assumptions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumptions.md)),
`stray` (drafts no request asks for), `requests` (the requests' folder),
`templates` (each request's setting,
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)'s
`templates`: a logical named by the request), `manifest` and
`manifest_file` (below).

## The next step's kinds

`next_steps` is English text; `next_kind` names the same step by a word,
with its values in `next_args`, so a screen can say it in its own
language. The kinds, and the values each carries (`drafts`: the
request's draft files, comma-separated):

- `answer`:

  no draft yet: `request` (the request file), `drafts` (the files the
  agent writes).

- `fix_answer`:

  a draft could not be read (its YAML, or no `id`): `drafts` (the files
  refused).

- `answer_again`:

  the drafts could not be checked: `drafts`.

- `repair`:

  the drafts have errors and a repair prompt was written: `repair` (the
  prompt file), `drafts`.

- `see_problem`:

  the report files could not be written (the problem is in `reports`).

- `warnings`:

  written, with warnings to read.

- `done`:

  written, nothing to do.

- `sheet_missing`:

  a sheet's request: written, but `missing` (the TOC items) have no
  draft.

- `add_missing`:

  written, but `missing` have no draft: `drafts` (theirs).

- `none_asked`:

  the drafts are none of the reports asked for: `asked`, `has`.

[`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md)
adds three, for a report that passed its checks: `not_built` (`reports`,
not built in this run: `Rscript run.R <request>`), `no_package`
(`report`, `package`: R has no such package), `build_failed` (`report`,
`problem`, `repair`).

The manifest is every report the drafts make, for the person to check
the count against the TOC (one TOC row can be several reports): a data
frame, one row a report: `request`, `toc`, `output_id`, `title`,
`split`, `listed` (`NA`: a request not answered or not read, its reports
as asked) and `status` (the report's, as in `reports`). It is written to
`ai/manifest.csv` (UTF-8) beside the requests' folder, anew at each
collection. The paths are relative to `study_dir`. Its print is a
summary: each request's status, its reports and the next step.

## See also

[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md),
[`tfl_ai_confirm()`](https://ichirio.github.io/tflspec/reference/tfl_ai_confirm.md),
[`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md).
