# Adopt the AI's drafts into the study's report files

The AI drafts a study's report files apart from the study's own (in a
study with `ai/spec/outputs/`,
[`tfl_ai_migrate()`](https://ichirio.github.io/tflspec/reference/tfl_ai_migrate.md)):
every collection
([`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md),
`run.R`) writes the drafts there and builds them into `ai/output/`. A
draft the person finds finished is adopted: copied to `spec/outputs/`,
where the study's lists, programs and runs read it, and the AI never
writes over it. Adopting again after the AI redrafted a report replaces
the adopted file only with `overwrite = TRUE` – a person may have
changed it since.

## Usage

``` r
tfl_ai_adopt(study_dir, outputs = NULL, overwrite = FALSE, dry_run = FALSE)
```

## Arguments

- study_dir:

  The study folder.

- outputs:

  The reports to adopt (their ids); `NULL`, every draft.

- overwrite:

  `TRUE` to write over a report already adopted (or written by hand)
  that differs from its draft. A report adopted that the study's
  `spec/toc.yml` does not list (a part the AI wrote, a report it named
  otherwise) gets an entry there – after its number's, else at the end –
  so the study's report list shows it.

- dry_run:

  `TRUE` to copy nothing and say what adopting would do: `status` `new`
  (no study file yet), `replace` (the study's file differs from the
  draft: adopting writes over it, with `overwrite = TRUE`), `same`,
  `no draft` – for a screen's question before it adopts.

## Value

A data frame, one row a report: `output_id`, `status` (`adopted`, `same`
– the study's file is the draft already –, `kept` – the study's file
differs and was left, pass `overwrite = TRUE` –, `no draft`), `file`
(the study's file), `toc` (`added`: its entry was added to
`spec/toc.yml`; with `dry_run`, `add`). The adoptions are recorded in
`ai/adopted.csv` (the report, when, the draft's md5).

## See also

[`tfl_ai_migrate()`](https://ichirio.github.io/tflspec/reference/tfl_ai_migrate.md),
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
