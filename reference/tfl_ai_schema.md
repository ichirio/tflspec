# The schema text of a drafting task

What a drafting prompt
([`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md))
tells the model it may write: every column or field, what it means and
an example, generated from the definitions tflspec's own readers and
checks use –
[`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)'s
fields for `toc`;
[`tfl_fig_parts()`](https://ichirio.github.io/tflspec/reference/tfl_fig_parts.md)
and
[`tfl_fig_templates()`](https://ichirio.github.io/tflspec/reference/tfl_fig_templates.md)
for `figure`; the spec's schema
([`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md)),
the ARD's columns
([`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md))
and methods
([`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md))
for `table` – so the prompt cannot name a column, a piece or a function
that does not exist.

## Usage

``` r
tfl_ai_schema(task, parts = NULL)
```

## Arguments

- task:

  One of
  [`tfl_ai_tasks()`](https://ichirio.github.io/tflspec/reference/tfl_ai_tasks.md).

- parts:

  The parts to describe: for `toc`, `"toc"`; for `figure`, sections
  among `"template"`, `"data"`, `"plot"`, `"layers"`, or single pieces
  by name (`"censor_mark"`, `"join"`; see
  [`tfl_fig_parts()`](https://ichirio.github.io/tflspec/reference/tfl_fig_parts.md));
  for `table`, `"report"` (the report file's keys), `"ard"` (the
  analyses) or content functions by name (`"plan_cells"`; from
  [`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md)).
  `NULL` for all. The repair prompt describes only the parts with
  problems.

## Value

The schema as one string (markdown), of class `tfl_ai_text`.

## Examples

``` r
cat(tfl_ai_schema("toc"))
#> `toc` is a list of rows, one a report. Its columns:
#> 
#> - `output_id`: The report's number as the documents give it, unique in the TOC (required). Example: `T-14-1-1`
#> - `type`: The kind of report: table, figure or listing. Example: `table`
#> - `title`: The title lines in order, without the report number and the analysis set; ` | ` between lines. Example: `Summary of Demographic and Baseline Characteristics`
#> - `population`: The analysis set the report is on, as the documents name it; it becomes the last title line. Example: `Safety Population`
#> - `footnote`: The footnote lines in order; ` | ` between lines. Example: `Percentages are based on the number of subjects in the analysis set.`
#> - `program`: The name of the program that makes the report. Example: `t_14_1_1.R`
#> - `file`: The name of the report's output file. Example: `t_14_1_1.rtf`
#> - `note`: A remark for people; never read by tflspec.
#> - `section`: The heading the report is listed under in the documents. Example: `14.1 Demographic and Baseline Data`
#> - `datasets`: The ADaM datasets the report reads; ` | ` between them. Example: `ADSL`
#> - `label`: The report's number as printed in its title. Example: `Table 14.1.1`
cat(substr(tfl_ai_schema("figure", parts = "survfit"), 1, 600))
#> A field marked * is required. Leave out a field to take its default.
#> 
#> ### data
#> 
#> One ordered list of steps from the ADaM datasets to what is drawn: the figure's data `df`, and the objects made from it, each with the `name` the layers refer to (a step after one changes that object); each piece starts with `step`.
#> 
#> - `survfit`: Kaplan-Meier fit. survfit2(Surv(time, censor == 0) ~ group). Fields:
#>   - `name` (text; default `fit`)
#>   - `time`* (variable; default `AVAL`)
#>   - `censor`* (variable; default `CNSR`)
#>   - `by` (variable): Empty = one curve.
#>   - `conf_type` (choice; one of log | log-log | pla
```
