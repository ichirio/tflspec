# The person's answers to the AI's assumptions

Each assumption the AI wrote (`spec/_assumptions.yml`) has an id,
`A<n>`, and a kind, `K<n>`
([`tfl_ai_assumptions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumptions.md)).
The person says an assumption is right (`ok`) or `wrong` and what is
right instead (`correct`), for its report (`id` `A<n>`, `scope`
`report`) or for every report of its kind (`id` `K<n>`, `scope` `all`):
an answer to an `A<n>` wins over its kind's. The answers are kept in
`ai/assumption_answers.csv` (UTF-8), which a screen writes or
`tfl_ai_assumption_answers_write()` does;
[`tfl_ai_corrections()`](https://ichirio.github.io/tflspec/reference/tfl_ai_corrections.md)
then hands the wrong ones to the agent.

## Usage

``` r
tfl_ai_assumption_answers_read(study_dir)

tfl_ai_assumption_answers_write(study_dir, x, overwrite = TRUE)
```

## Arguments

- study_dir:

  The study folder.

- x:

  The answers: a data frame with `id` and any of `scope`, `output_id`,
  `verdict`, `correct` (a missing one is empty; `scope` and `output_id`
  are filled from the id).

- overwrite:

  `TRUE` (default) to answer anew an id answered already; `FALSE` makes
  that an error.

## Value

`tfl_ai_assumption_answers_read()`: a data frame of the columns above,
one row an answer (none: no row), or `NULL` when the study has no
`ai/assumption_answers.csv`. `tfl_ai_assumption_answers_write()`: the
file's path, invisibly.

## Details

The file: one row an answer, the columns `id`, `scope` (`report` /
`all`), `output_id` (the report, for scope `report`), `verdict` (`ok` /
`wrong`) and `correct` (the right content, for `wrong`). A row with
`correct` and no verdict is `wrong`.

`tfl_ai_assumption_answers_write()` adds the rows of `x` to the file: an
id answered there already is answered anew (`overwrite = TRUE`), and a
row with neither `verdict` nor `correct` removes that id's answer (the
assumption is open again). An unknown id, a verdict not `ok` / `wrong`
or `wrong` with no `correct` is an error, and nothing is written. An
answer completes a question of `ai/questions.md` when every assumption
of it is answered: its `- Answer:` is written from them
(`A3, A7: <the right content>`, `A4: confirmed`), unless the person has
written one there.

## See also

[`tfl_ai_assumptions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumptions.md),
[`tfl_ai_corrections()`](https://ichirio.github.io/tflspec/reference/tfl_ai_corrections.md)
