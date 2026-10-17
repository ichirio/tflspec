# A vital sign's change: a table plan from the template `vs_change`

A display template on the `summary` layout: a parameter's baseline,
value and change from baseline at one visit (n, mean (SD), SE, 95% CI of
the mean, median, min, max).

## Usage

``` r
tpl_vs_change(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's parameters: `param`, `visit`, `param_label`, `when`,
  `visit_label`, `unit`, `by`.

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
