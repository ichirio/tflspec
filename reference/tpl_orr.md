# Objective response: a table plan from the template `orr`

The best overall response (n (%)) and the objective response rate with
its 95% CI, by arm.

## Usage

``` r
tpl_orr(ard, population = "ITT", ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `by`, `by_levels`, `data`, `param`,
  `category`, `responders`, `ci_method`, `conf_level` (0.95, the CI's
  level and its row's label), `stat_layout`, `heading`.
  `stat_layout = "rows"` (the default) puts the rate and its CI under
  each other in the arm's column; `"cols"` gives each arm two columns,
  the rate and the CI (the layout
  [`tpl_stat_block()`](https://ichirio.github.io/tflspec/reference/tpl_stat_block.md)).

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
