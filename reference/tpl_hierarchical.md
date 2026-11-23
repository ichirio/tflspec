# A hierarchy's counts: a table plan from the layout `hierarchical`

A hierarchy's counts: a table plan from the layout `hierarchical`

## Usage

``` r
tpl_hierarchical(ard, population, hierarchy, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- hierarchy:

  The levels, outer first (`c("AEBODSYS", "AEDECOD")`).

- ...:

  The template's other parameters: `by`, `data`, `where`, `any_row`,
  `any_row_label`, `sort`, `heading` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
