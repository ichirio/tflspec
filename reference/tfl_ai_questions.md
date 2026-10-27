# The questions of a study's AI drafts, and the person's answers to them

`ai/questions.md` holds what the AI could not settle – an assumption's
`question`, gathered by
[`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md):
one question asked by several reports once, the reports listed.
`tfl_ai_questions()` reads it; a screen shows the questions and takes
the answers, and `tfl_ai_answer_questions()` writes them back – each
into its question's `- Answer:`, everything else as it is. The agent (or
the person) then moves an answered question to `ai/decisions.md`, sets
its status to `decided`, and redoes the reports it touches.

## Usage

``` r
tfl_ai_questions(study_dir)

tfl_ai_answer_questions(study_dir, answers)
```

## Arguments

- study_dir:

  The study folder.

- answers:

  The answers: a data frame with `id` (`"Q1"`) and `answer`, or a
  character vector named by the ids. `NA` or `""` empties an answer.

## Value

`tfl_ai_questions()`: a data frame, one row a question: `id`,
`output_id` (the reports it touches, comma-separated), `where`,
`question`, `provisional` (what the AI took for now), `answer` (`NA`
when none), `status` (`open`, `decided`); no row when there is no
question. `tfl_ai_answer_questions()`: the file's path, invisibly.

## Details

The file: each question a block from its heading `## Q<n>. <question>`
to the next heading, holding `- Reports:`, `- Assumptions:` (the ids of
the assumptions asking it,
[`tfl_ai_assumptions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumptions.md)),
`- Where:`, `- The agent took, for now:`, `- Status: open` (or
`decided`) and `- Answer:` (one line or more, to the block's end). A
question is answered through its assumptions too
([`tfl_ai_assumption_answers_write()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumption_answers_read.md)).

## See also

[`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md),
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
(`mode = "agent"`).
