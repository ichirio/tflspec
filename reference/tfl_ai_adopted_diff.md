# What the AI changed in a report since it was adopted

A report adopted from the AI's draft
([`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md))
and redrafted by the AI since
([`tfl_ai_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_ai_outputs.md)'s
`adopted` `yes, draft newer`): its study file and its draft compared
field by field, for the person to see before adopting again. The two
YAML files are read as they are written (an `!r` value as its text),
each value under its path: `label`, `page.titles.2.center`,
`ard.analyses[AE].where` (an item of a list named by its `id`, else by
its place, `[2]`).

## Usage

``` r
tfl_ai_adopted_diff(study_dir = ".", output_id = NULL)
```

## Arguments

- study_dir:

  The study folder.

- output_id:

  The report; `NULL`, every report whose draft is newer than its
  adoption.

## Value

A data frame, one row a value that differs: `output_id`, `where` (the
path), `change` (`changed`, `added` – in the draft only –, `removed` –
in the study's file only), `adopted` and `draft` (the value, one line: a
short YAML; `NA` where there is none); no row when they are the same.
Its attribute `text` says how many, for a screen.

## See also

[`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md),
[`tfl_ai_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_ai_outputs.md)
