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

## Details

The header N and the percentage's denominator are set separately.
`header_n` says the N printed in each arm's heading: by default
(`"set"`) the arm's N in the analysis set (with `subset`); `"tested"`
the arm's subjects in the Total row for the page's parameter (with
`param_page`); `"none"` none. `percent_of` says what a cell's percentage
is of: by default (`"total"`) the subjects with both a baseline and a
post-baseline category for the parameter, so the Total row is 100%;
`"row"` the subjects of the baseline category.

A subject with no baseline category is left out of the table (in no row,
not in the Total or any N) unless `missing_label` is given (for example
`"Missing"`): then it has a row of its own, and is counted in the Total
and in the percentage's denominator. A subject with no post-baseline
category has nothing to shift and is never in the table.
