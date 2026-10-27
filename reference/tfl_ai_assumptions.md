# The AI's assumptions, their ids and the person's answers

What the AI took on a guess, from `spec/_assumptions.yml` (written as
its answers are collected,
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)),
one row an assumption, for a screen to show and the person to answer
([`tfl_ai_assumption_answers_write()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumption_answers_read.md)).

## Usage

``` r
tfl_ai_assumptions(study_dir)
```

## Arguments

- study_dir:

  The study folder.

## Value

A data frame, one row an assumption: `id`, `kind`, `n_reports` (the
reports of its kind), `output_id`, `where`, `item`, `assumption`,
`question`, `verdict` and `correct` (its answer, its own or its kind's;
`NA` when none), `status`: `open` (no answer), `confirmed` (`ok`),
`answered` (`wrong`, for the agent to apply) or `done` (the agent
applied it: its section of `ai/corrections.md` is done, or the
assumption is `status: fixed` in `spec/_assumptions.yml`).

## Details

Each assumption has an id, `A<n>`, kept across collections: the same
report, `where` and `item` (the item compared without case, spaces or
punctuation) keep their id, and a new one takes the next number
(`ai/assumption_ids.csv` keeps them: columns `type`, `key`, `id`). Each
also has a kind, `K<n>`: the assumptions asking the same question (as
`ai/questions.md` groups them), or, with no question, saying the same
item and assumption – an answer to the kind is an answer for every
report of it, unless a report's assumption has its own.

## See also

[`tfl_ai_assumption_answers_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumption_answers_read.md),
[`tfl_ai_corrections()`](https://ichirio.github.io/tflspec/reference/tfl_ai_corrections.md),
[`tfl_ai_questions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_questions.md)
