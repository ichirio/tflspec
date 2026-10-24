# One record's statistics over rows and columns: a table plan from the template `stat_block`

A layout template: each arm's block holds one analysis record's
statistics – its N in one column, its rate and interval in the next, or
under each other – as `cells` says (`plan_stat_groups()`). The ARD is
the report's own; display templates such as
[`tpl_orr()`](https://ichirio.github.io/tflspec/reference/tpl_orr.md)
build on it.

## Usage

``` r
tpl_stat_block(ard, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- ...:

  The template's parameters: `variable` and `cells` (required), `by`,
  `by_levels`, `rows`, `direction`, `label`. `cells` is a named list,
  heading = template, one element a column (`direction = "cols"`) or a
  row (`"rows"`), a vector its lines
  ([`tpl_orr()`](https://ichirio.github.io/tflspec/reference/tpl_orr.md)'s
  `cells`: an N column, then the rate over its interval).

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
