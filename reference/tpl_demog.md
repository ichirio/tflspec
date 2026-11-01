# Demographics: a table plan from the template `demog`

The A use of a template SPEC: one call for the report's table plan. The
template `demog` is a display template on the `summary` layout:
continuous variables as rows of statistics, categorical ones as n (%).

## Usage

``` r
tpl_demog(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `data`, `by`, `continuous`,
  `categorical`, `labels`, `cells`. `labels` given by name change those
  names only (`labels = c(RACE = "Race, n (%) [a]")`); in YAML,
  `RACE: ~` drops one. The default `continuous` and `categorical` are
  the variables the data has of them (`any_of()`); the report's own are
  as written. Every table and figure template also takes `subset` (which
  subjects) and, with records data, `where` (which records): see
  [`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md).

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html):
the template's items run on the ARD. A study's defaults are not in it;
add them after the call.

## See also

[`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
for the items (the B use).

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = AGE),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
  plan <- tpl_demog(ard, population = "SAF")
  head(rtfreporter::plan_apply(plan, "table"))
}
#> widen_ard(): 10 ARD rows were not used.
#>   tabulate       N                2  a key variable's own tabulation
#>   tabulate       n                2  a key variable's own tabulation
#>   tabulate       p                2  a key variable's own tabulation
#>   summary        p25              2  no template named it
#>   summary        p75              2  no template named it
#>   (notes = FALSE to silence; attr(, "ard_ignored") has the detail)
#>         group     label      Drug X     Control
#> 1 Age (years)         n          80          78
#> 2 Age (years) Mean (SD) 60.1 (9.59) 62.7 (9.76)
#> 3 Age (years)    Median        59.5        62.5
#> 4 Age (years)  Min, Max      39, 82      42, 85
```
