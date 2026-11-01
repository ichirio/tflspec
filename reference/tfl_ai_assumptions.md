# The AI's assumptions and their ids

What the agent took on a guess or from a decision, from
`spec/_assumptions.yml` (written as its drafts are checked,
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)),
one row an assumption, for a screen to show and the person to read. What
is wrong, the person tells the agent in its chat, or changes in the
builder.

## Usage

``` r
tfl_ai_assumptions(study_dir)
```

## Arguments

- study_dir:

  The study folder.

## Value

A data frame, one row an assumption: `id`, `kind`, `n_reports` (the
reports of its kind), `output_id`, `where`, `item`, `assumption` (one
sentence: what the AI decided and how), `value` (the value it used, as
text; `NA` when the item has none), `question`, `from` (the decision it
was taken from, `D<n>`; `NA` when its own).

## Details

Each assumption has an id, `A<n>`, kept across collections: the same
report, `where` and `item` (the item compared without case, spaces or
punctuation) keep their id, and a new one takes the next number
(`ai/assumption_ids.csv` keeps them: columns `type`, `key`, `id`). Each
also has a kind, `K<n>`: the assumptions asking the same question, or,
with no question, saying the same item and assumption.

## See also

[`tfl_ai_confirm()`](https://ichirio.github.io/tflspec/reference/tfl_ai_confirm.md),
[`tfl_ai_decisions_read()`](https://ichirio.github.io/tflspec/reference/tfl_ai_decisions_read.md)
