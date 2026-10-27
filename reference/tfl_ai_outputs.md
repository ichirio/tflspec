# The AI's reports, one row a report

The list a person checks the AI's work by: every report the answers make
([`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)'s
manifest, `ai/manifest.csv`) and every draft report file there, with
where it is from and how far it got. A screen shows it in three levels –
the shell sheets, the TOC items, the reports – by its `sheet` and
`toc_id`.

## Usage

``` r
tfl_ai_outputs(study_dir = ".")
```

## Arguments

- study_dir:

  The study folder.

## Value

A data frame, one row a report: `sheet` (its request's shell sheets, `|`
between them), `shell_code` (the id written at the top of its shell, the
TOC entry's), `toc_id` (its TOC row's number, as the answer lists it;
else its TOC entry's id – a part's, its number's), `output_id`, `title`
(as the answer lists it, else its report file's label), `population`
(its analysis set), `split` (how its TOC row is split for it),
`request`, `status` (`ok`, `warning`, `error`, `no answer`,
`not answered`; `draft` for a report file no answer lists), `rtf` (its
RTF, `NA` when none), `problem` (why it has none), `questions` (the open
questions in `ai/questions.md` naming it), `adopted` (`no`; `yes`;
`yes, draft newer` – the draft differs from the study's file adopted
from it) and `file` (its draft report file).

## See also

[`tfl_ai_expected_write()`](https://ichirio.github.io/tflspec/reference/tfl_ai_expected_write.md)
for the list the person expects.
