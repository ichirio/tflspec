# Shift tables: table plans from the templates `shift` and `lab_shift`

`shift` is the layout: a category at baseline (the rows, a block a
parameter) by the same category after it (the columns, under each arm).
`lab_shift` is the display template on it: the laboratory normal range
indicator (`BNRIND` by `ANRIND`) at one visit, for some parameters.

## Usage

``` r
tpl_shift(ard, population, ..., codelists = NULL)

tpl_lab_shift(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters (`tpl_shift()`: `by`, `data`, `where`,
  `param`, `baseline`, `post`, `levels`, `param_labels`, `heading` ...;
  `tpl_lab_shift()`: `visit`, `params`, `by`, `param_labels` ...).

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).
