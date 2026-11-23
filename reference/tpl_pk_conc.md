# PK concentrations by subject with their summary: a table plan from the template `pk_conc`

One table, a page per group (`by`: the analyte `PARAM`, the arm
`TRT01A`): the subjects' concentrations above, one row a subject
(`{USUBJID}/{AGE}/{SEX}`), the time points across (`ATPT`, in the order
of `ATPTN`), and their summary statistics below over the same columns
(N, Mean, SD, SE, CV(%), Median, Min, Max). A record below the limit of
quantification (`blq`, an R condition) prints `BLQ` in the listing; the
summary reads the non-missing values. The summary is laid under the
listing by rtfreporter's `plan_append()`; `split_at` starts a second
block of columns at a time point.

## Usage

``` r
tpl_pk_conc(data, ard, population, ..., codelists = NULL)
```

## Arguments

- data:

  The listing's records: the concentrations, one row a sample.

- ard:

  The summary's ARD: the statistics by the groups and the time point
  (the template's analysis, not normalized).

- population:

  The analysis set's id (`"PK"`).

- ...:

  The template's other parameters: `where`, `by`, `heading`, `time`,
  `time_order`, `times`, `value`, `blq`, `blq_label`, `cell`, `subject`,
  `subject_heading`, `time_heading`, `summary_label`, `stats`,
  `split_at` ...

- codelists:

  The report's code lists; `NULL` takes the data's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
x <- tfl_template_expand("pk_conc", list(population = "PK", blq = "BLQFL == \"Y\""))
vapply(x$content, `[[`, "", "fn")
#> [1] "table_plan"         "plan_cells"         "plan_append"       
#> [4] "plan_stub"          "plan_blanks"        "plan_col_header"   
#> [7] "plan_paginate_rows" "plan_columns"      
```
