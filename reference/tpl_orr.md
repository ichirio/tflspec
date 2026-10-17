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
  `category`, `responders`, `ci_method`, `heading`.

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
