# Compare a study's AI answers: with templates or the code SPEC only

One row a report of the study's requests, for setting the two forms side
by side
([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)'s
`templates`): the setting its request was written with, what the answer
used (a template, or a code SPEC), how many values its report file holds
(the leaves of its YAML: what a person reviews), its checks' result and
how many answers needed a repair. Run after
[`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md)
(or
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)):
the answers are read and checked again, and the table written to
`ai/compare.csv`.

## Usage

``` r
tfl_ai_compare(study_dir, file = file.path("ai", "compare.csv"))
```

## Arguments

- study_dir:

  The study folder.

- file:

  Where the table goes, under `study_dir`; `NULL` writes none.

## Value

A data frame, one row a report: `output_id`, `request`, `templates`
(`TRUE`, `FALSE`), `answered_with` (`template:<name>` or `code`; `NA`
with no report file), `values`, `check` (`ok`, `warning`, `error`,
`no answer`) and `repairs`.

## See also

[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md),
[`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md).
