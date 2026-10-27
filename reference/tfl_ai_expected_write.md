# The list of reports the person expects

The person's own list of the study's reports, `ai/expected_outputs.csv`:
first a copy of
[`tfl_ai_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_ai_outputs.md),
then corrected by the person (on a screen, or in a spreadsheet) – a
row's `action` says what to do: blank, keep it; `add`, a report to add
(its sheet and TOC item, and what is known of its id, title, analysis
set and part); `drop`, a report not wanted (the row stays, with its
`reason`); `change`, a row whose title or analysis set the person
changed (`was`: the values before,
`title: Demographics | population: SAF`). `note` is what the report
should show, free text. The AI never writes it; `run.R` compares the
reports with it
([`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md)).

## Usage

``` r
tfl_ai_expected_write(study_dir = ".", x = NULL, overwrite = FALSE)

tfl_ai_expected_read(study_dir = ".")
```

## Arguments

- study_dir:

  The study folder.

- x:

  The rows to write: a data frame with the list's columns (`sheet`,
  `toc_id`, `output_id`, `title`, `population`, `split`, `action`,
  `was`, `reason`, `note`; a column it lacks is blank). `NULL`: the AI's
  list now.

- overwrite:

  `TRUE` to write over the list there.

## Value

`tfl_ai_expected_write()`: the file's path, invisibly.
`tfl_ai_expected_read()`: the list (a data frame of those columns), or
`NULL` when the study has none.

## Details

`tfl_ai_expected_write()` writes it: `x` (the person's rows), or, when
`NULL`, the first copy of the AI's list – only when there is none yet
(`overwrite = TRUE` to start again). `tfl_ai_expected_read()` reads it.

## See also

[`tfl_ai_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_ai_outputs.md)
