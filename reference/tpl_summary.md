# Continuous variables by a column key: a table plan from the layout `summary`

Continuous variables by a column key: a table plan from the layout
`summary`

## Usage

``` r
tpl_summary(ard, population, continuous = NULL, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- continuous:

  The continuous variables, in order; `NULL` for none.

- ...:

  The template's other parameters: `categorical` (the categorical
  variables), `by`, `labels`, `order`, `cells`, `count_cell`, `total`
  ...: `tfl_tpl_help("summary")`. The look (the stub, the blank rows,
  the widths, the page) is the study standard's; change it after the
  template with `|> plan_stub()`, `|> plan_paginate_rows()` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = c(AGE, TRTDURD)))
  plan <- tpl_summary(ard, population = "SAF", continuous = c("AGE", "TRTDURD"))
  head(rtfreporter::plan_apply(plan, "table"))
}
#> widen_ard(): 14 ARD rows were not used.
#>   summary        p25              4  no template named it
#>   summary        p75              4  no template named it
#>   tabulate       N                2  a key variable's own tabulation
#>   tabulate       n                2  a key variable's own tabulation
#>   tabulate       p                2  a key variable's own tabulation
#>   (notes = FALSE to silence; attr(, "ard_ignored") has the detail)
#>     group     label         Drug X       Control
#> 1     AGE         n             80            78
#> 2     AGE Mean (SD)    60.1 (9.59)   62.7 (9.76)
#> 3     AGE    Median           59.5          62.5
#> 4     AGE  Min, Max         39, 82        42, 85
#> 5 TRTDURD         n             80            78
#> 6 TRTDURD Mean (SD) 188.7 (135.04) 123.8 (84.01)
```
