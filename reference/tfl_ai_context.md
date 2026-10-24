# The study facts a drafting prompt carries

One constructor for what
[`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md)
tells the model about the study and the report, and what
[`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md)
checks the answer against, so a caller cannot pass the wrong shape and a
prompt cannot read a field that is not there. Each task reads its own
fields; a field another task reads is an error. Every field is optional
but `output_id` for a figure.

## Usage

``` r
tfl_ai_context(
  task,
  study = NULL,
  output_id = NULL,
  titles = NULL,
  population = NULL,
  datasets = NULL,
  design = NULL,
  templates = NULL,
  toc_fields = NULL,
  reports = NULL,
  language = NULL,
  outputs = NULL,
  shell = NULL,
  study_dir = NULL,
  examples = NULL
)
```

## Arguments

- task:

  One of
  [`tfl_ai_tasks()`](https://ichirio.github.io/tflspec/reference/tfl_ai_tasks.md):
  `"toc"`, `"figure"` or `"table"`.

- study:

  The study: a named list or character vector with `study_id` and
  `title` (either may be missing).

- output_id:

  `figure`: the report's id (required).

- titles:

  `figure`: the report's title lines, as the TOC has them.

- population:

  `figure`: its analysis set: one label, or a named list or vector with
  `id`, `label` and `where` (the condition, such as `FASFL == "Y"`).

- datasets:

  `figure`, `toc` and `table`: the datasets and variables the answer may
  name. A
  [`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md)
  (its `variables`); or a data frame, one row a variable, with `dataset`
  and `variable` and optionally `label`, `kind`, `levels` (`|` between
  the values) and `dataset_label` –
  [`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md)'s
  result is one (its `codelist` and `source` are not sent); or the ADaM
  datasets themselves, a named list of data frames, of which only the
  names, the labels (the `label` attribute) and the kinds are taken.

- design:

  `figure`: the current design (a
  [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)),
  when there is one: the prompt shows it and asks for it whole, changed.

- templates:

  `figure`: the templates the answer may start from (names of
  [`tfl_fig_templates()`](https://ichirio.github.io/tflspec/reference/tfl_fig_templates.md));
  `NULL` for all.

- toc_fields:

  `toc`: the columns the answer fills, among `output_id` (always),
  `type`, `title`, `population`, `footnote`, `program`, `file`, `note`,
  `section`, `datasets`, `label`
  ([`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)'s
  fields); `NULL` for `output_id`, `type`, `title`, `population`,
  `footnote`, `section`, `datasets`, `label`.

- reports:

  `toc`: the reports already in the TOC: their ids, or a data frame with
  `output_id` and optionally `title`.

- language:

  The language the model writes its assumptions in, such as
  `"Japanese"`; `NULL`: the language of the user's message. The prompt
  itself is English.

- outputs:

  `table`: the reports to draft (5 to 10: one batch of
  [`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md)),
  as the TOC numbers them.

- shell:

  `table`: the TOC and shells as text – a
  [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)
  result (read again for `outputs` alone when it holds the whole
  workbook) or a string; `NULL` when the workbook is attached to the
  chat instead.

- study_dir:

  `table`: the study folder
  ([`tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.md)):
  its `spec/data.yml` (the analysis sets the reports name) and
  `spec/defaults.yml` (what every report extends) go into the prompt.

- examples:

  `table`: reviewed report files of this study (`spec/outputs/<id>.yml`
  paths) that the new reports follow – a family's first table, corrected
  by hand, for the rest of the family.

## Value

A `tfl_ai_context`: a list with `task` and the fields given.

## Details

Nothing here is sent anywhere: the context becomes text in the prompt,
which the user reads before copying it. Only names and labels are
carried, never the data's values: a dataset's `levels` (a variable's
values, such as its PARAMCDs) are there only when the caller gives them.

## See also

[`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md).

## Examples

``` r
tfl_ai_context("toc", study = list(
  study_id = "XYZ-ONC-01", title = "Drug X versus control in advanced solid tumours"
))
#> <tfl_ai_context: toc>
#> ## The study
#> 
#> XYZ-ONC-01: Drug X versus control in advanced solid tumours
#> 
#> ## The TOC's columns
#> 
#> Fill these columns (the schema describes them): `output_id`, `type`, `title`, `population`, `footnote`, `section`, `datasets`, `label`.
#> 
#> ## Reports already in the TOC
#> 
#> None: the TOC is empty.

adam <- tfl_example_adam()
tfl_ai_context("figure", output_id = "F-14-2-1",
               titles = "Kaplan-Meier Plot of Time to Death",
               population = list(id = "FAS", label = "Full Analysis Set",
                                 where = "FASFL == \"Y\""),
               datasets = adam["ADTTE"])
#> <tfl_ai_context: figure>
#> ## The report
#> 
#> - Report: F-14-2-1
#> - Titles: Kaplan-Meier Plot of Time to Death
#> - Analysis set: FAS, Full Analysis Set (`FASFL == "Y"`)
#> 
#> ## The datasets
#> 
#> Name only these datasets and variables.
#> 
#> ### ADTTE
#> 
#> | variable | label | kind |
#> |---|---|---|
#> | USUBJID |  | character |
#> | TRT01P |  | character |
#> | TRT01PN |  | numeric |
#> | FASFL | Full Analysis Set Population Flag | character |
#> | PARAMCD |  | character |
#> | PARAM |  | character |
#> | AVAL | Analysis Value | numeric |
#> | CNSR | Censor | numeric |
#> | SEX |  | character |
#> | AGEGR1 |  | character |
#> 
#> ## The current design
#> 
#> There is none yet: start from the template that fits best.
```
