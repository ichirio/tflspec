# An ANCOVA of the change from baseline: a table plan from the template `ancova`

A parameter's change from baseline (`CHG`) at one visit, by an analysis
of covariance: `<response> ~ <by> + <covariates>`
(`CHG ~ TRT01A + BASE`), a linear model. Each arm's n and LS mean (SE,
95% CI), and the difference in LS means of each other arm minus the
reference arm (SE, 95% CI, p-value) in that arm's column, from cardx's
`ard_emmeans_emmeans()` and `ard_emmeans_contrast()` (emmeans). The
reference arm is `ref`, else the last arm in the code list's order; the
program says which (a message). The p-value is written as the program's
`fmt_pvalue()` writes it (`<0.001`, else 3 decimals).

## Usage

``` r
tpl_ancova(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `data`, `param`, `visit` (required),
  `by`, `response`, `covariates`, `where`, `ref`.

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
x <- tfl_template_expand("ancova", list(population = "SAF", data = "ADVS",
                         param = "SYSBP", visit = "CYCLE 2 DAY 1"))
x$ard$analyses[[2]]$args
#> [1] "formula = CHG ~ TRT01A + BASE, method = \"lm\""
#> attr(,"class")
#> [1] "tfl_r"
#> attr(,"tag")
#> [1] "!r"
```
