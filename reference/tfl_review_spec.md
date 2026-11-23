# Review a study's definition

Lists, for the whole study, what is wrong with its definition (`error`:
it cannot be used – every problem the constructors
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md),
[`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md),
[`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
and
[`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
stop or report on), what is valid but probably wrong (`check`), and what
is missing and has to be set by hand (`hand`). Nothing stops: a
definition that cannot be built is reviewed as the sheets it is. With
`facts`, the definition is also checked against what the data hold
([`tfl_data_facts()`](https://ichirio.github.io/tflspec/reference/tfl_data_facts.md):
a column an analysis reads that its data do not have, a condition that
keeps nothing, a code list against the values) and against what each
report's ARD holds (`facts$ard[[output_id]]`,
[`tfl_ard_facts()`](https://ichirio.github.io/tflspec/reference/tfl_ard_facts.md)).
The rules are
[`tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.md).

## Usage

``` r
tfl_review_spec(
  spec = NULL,
  ard = NULL,
  listings = NULL,
  figures = NULL,
  output_id = NULL,
  facts = NULL,
  rules = NULL
)
```

## Arguments

- spec:

  The table definition: a
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
  or a list of its sheets (`tables`, `variables`, `cells` ...).

- ard:

  The ARD definition: a
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  or a list of its five sheets.

- listings:

  The listing definition: a
  [`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
  or a list of its two sheets.

- figures:

  The figures' designs: a list of
  [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)s
  (or their lists) named by their reports.

- output_id:

  Only these reports; `NULL` reviews every report the sheets name, and
  the study-wide rows (`output_id` `NA`: the defaults, the data).

- facts:

  The facts of the data
  ([`tfl_data_facts()`](https://ichirio.github.io/tflspec/reference/tfl_data_facts.md)),
  with the reports' ARD facts in `facts$ard`; `NULL` runs the rules that
  need the definition only.

- rules:

  Only these rules: their ids (`"T03"`) or areas (`"table"`).

## Value

A `tfl_review`: a data frame, one row per item – `output_id`, `level`
(`error`, `check`, `hand`), `area` (`report`, `codelists`, `ard`,
`ard_run`, `table`, `listing`, `figure`, `data`, `spec`), `sheet`, `row`
(the row's key, as the sheet is keyed: the variable, the analysis_id,
`variable / context / row` on `cells`), `field` (the column), `message`,
`template` (the sentence the values fill: the rule's message or one of
[`tfl_review_templates()`](https://ichirio.github.io/tflspec/reference/tfl_review_templates.md)),
`hint`, `rule`, `draft` (`FALSE`: set by an import for the rows it
brings), `args` (the values that fill `template`, for a translation) and
`fix` (a one-step fix, where there is one:
[`tfl_fig_apply_fix()`](https://ichirio.github.io/tflspec/reference/tfl_fig_advice.md)).
Errors first, then checks, then what to set by hand; within a level by
report and sheet.

## See also

[`tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.md),
[`tfl_data_facts()`](https://ichirio.github.io/tflspec/reference/tfl_data_facts.md),
[`tfl_ard_facts()`](https://ichirio.github.io/tflspec/reference/tfl_ard_facts.md)

## Examples

``` r
ard <- list(
  datasets = data.frame(dataset = "ADSL", path = "adsl.rds"),
  populations = data.frame(population_id = "SAF", dataset = "ADSL",
                           where = 'SAFFL == "Y"'),
  analyses = data.frame(output_id = "T-1", analysis_id = "AGE",
                        method = "continuous", population_id = "SAF",
                        dataset = "ADSL", by = "TRT01A",
                        variables = "AGE | TRT01A"))
r <- tfl_review_spec(ard = ard)
r[, c("output_id", "level", "rule", "message")]
#> <tfl_review> 0 errors, 1 to check, 0 to set by hand
#>   
summary(r)
#>   output_id error check hand
#> 1       T-1     0     1    0
```
