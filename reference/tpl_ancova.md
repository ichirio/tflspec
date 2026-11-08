# An ANCOVA of the change from baseline: a table plan from the template `ancova`

A parameter's change from baseline (`CHG`) at one visit, by an analysis
of covariance: `<response> ~ <by> + <covariates>`
(`CHG ~ TRT01A + BASE`), a linear model. Each arm's n and LS mean (SE,
95% CI), and the difference in LS means of each other arm minus the
reference arm (SE, 95% CI, p-value) in that arm's column. The LS means
are cardx's `ard_emmeans_emmeans()`; the differences are emmeans' own
(`emmeans::contrast(method = "trt.vs.ctrl")`, each arm against the
reference arm). The reference arm is `ref`, else the last arm in the
code list's order; the program says which (a message). The p-value is
written as the program's `fmt_pvalue()` writes it (`<0.001`, else 3
decimals).

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
  `by`, `response`, `covariates`, `where`, `ref`, `adjust`.

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Details

`adjust` is the multiplicity adjustment of the differences' CIs and
p-values, one of emmeans' `adjust` values (see "P-value adjustments" in
[`emmeans::summary.emmGrid()`](https://rvlenth.github.io/emmeans/reference/summary.emmGrid.html)):
`"none"`, `"tukey"`, `"dunnettx"`, `"sidak"`, `"bonferroni"`, `"holm"`,
`"hommel"`, `"hochberg"`, `"BH"`, `"BY"`, `"fdr"`, `"mvt"` or
`"scheffe"`. **The default is `"none"`: no adjustment.** Before this
argument existed, a table with three arms or more was Tukey-adjusted
without saying so (cardx's `ard_emmeans_contrast()` takes emmeans'
pairwise contrast, whose default is Tukey); with two arms nothing was
adjusted, and `"none"` gives the same table. Pass `adjust` always, even
`"none"`: emmeans' own default for these differences is `"dunnettx"`.
For differences against one reference arm, `"tukey"` is the Tukey
adjustment over all pairs of arms (emmeans' pairwise contrast), of which
the table shows the differences against the reference arm, and it gives
the table as it was before this argument for three arms or more. `"mvt"`
is a Monte Carlo method: its numbers change a little from run to run
unless the seed is set.

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
