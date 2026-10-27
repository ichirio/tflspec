# Check a drafting answer in its context

Runs tflspec's own checks on a read answer
([`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)),
with the study's context
([`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)),
and adds their problems to the answer's. Errors block applying the
answer; warnings do not.

## Usage

``` r
tfl_ai_check(answer, context = NULL, adam = NULL)
```

## Arguments

- answer:

  A `tfl_ai_answer`
  ([`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)).

- context:

  The
  [`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
  the prompt was made with; `NULL` for none (only what needs no context
  is checked).

- adam:

  `figure`: the study's data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)),
  for the checks that look into it; `NULL` for none.

## Value

A data frame of the problems: `where` (`toc row 2 field type`,
`layers[3] point field size` ...), `problem`, `severity` (`error` or
`warning`); no rows when there are none.

## Details

- `toc`: the rows are written to a temporary `.csv` and read back with
  [`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)
  (its errors – a row with no output id but other cells filled, an id
  twice – become problems on their rows); a kind it had to guess, and a
  row with no id it passes over as a heading, are warnings; a column
  outside the context's `toc_fields` is an error.

- `table`: each report against the schema – a key a report file does not
  have, a content function that is not a table's, an argument the
  function does not have
  ([`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md),
  an error here: the AI's invention), a value of the wrong type, a
  required argument missing; its `ard:` fields and method; the analysis
  sets of `spec/data.yml` (with `study_dir`); with the context's
  `datasets` (the ADaM summary), every dataset and variable an analysis
  names (made by `derive` or `add` aside); a report asked for and
  missing. Then the answer is put in a copy of the study folder and its
  ARD program
  ([`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md))
  and each table's code
  ([`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md))
  are written: what fails there is a problem too. Nothing is run.

- `figure`:
  [`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  on the design (with `adam`, the data-aware checks too: the variables
  and PARAMCDs it names); an `output_id` other than the context's is an
  error; a template not known, or not among the context's `templates`,
  and a dataset not in the context's `datasets` are warnings.

## See also

[`tfl_ai_repair_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_repair_prompt.md)
to send them back.

## Examples

``` r
answer <- c(
  "```yaml",
  "tflspec_ai: {task: toc, version: 1}",
  "toc:",
  "- {output_id: T-14-1-1, type: table, title: Demographics}",
  "- {output_id: T-14-1-1, type: tbl, title: Disposition}",
  "- {output_id: L-16-2-1, type: list, title: Deaths}",
  "assumptions: []",
  "```")
tfl_ai_check(tfl_ai_parse(answer, "toc"), tfl_ai_context("toc"))
#>                       where                   problem severity
#> 1 toc row 1 field output_id 'T-14-1-1' is given twice    error
#> 2 toc row 2 field output_id 'T-14-1-1' is given twice    error
```
