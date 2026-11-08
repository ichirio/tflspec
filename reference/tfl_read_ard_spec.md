# Read and check an ARD definition workbook

Read and check an ARD definition workbook

## Usage

``` r
tfl_read_ard_spec(path, check = TRUE, statistics = NULL, methods = NULL)

tfl_ard_spec(x, statistics = NULL, methods = NULL)
```

## Arguments

- path:

  An `ard_spec.xlsx`.

- check:

  `FALSE` reads a definition still being written without refusing it.

- statistics, methods:

  The catalogs
  ([`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md),
  [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md))
  to use instead of the current ones.

- x:

  A list of the sheets (data frames).

## Value

An `tfl_ard_spec`: a list of the seven sheets (a workbook without
`analysis_data`, written before it was added, reads it empty).

## Problems

`tfl_ard_spec()` stops with every problem it finds, one line each; the
condition (class `tflspec_spec_error`) carries them as rows too,
`cnd$problems`: `output_id`, `sheet`, `row` (the row's key: the
`analysis_id`, the `data_id`, the `population_id`), `field` and
`message`.
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
lists them without stopping.

What depends on the installed cards / cardx is a warning: an argument in
`args` or in a `post` step its function does not have, and a value that
does not fit the argument's kind
([`tfl_ard_args()`](https://ichirio.github.io/tflspec/reference/tfl_ard_args.md)):
`TRUE` or `FALSE` for a logical, a number (`conf.level` between 0 and
1), one of the choices (`method = "wilson"`), text in quotes, a formula,
a column, a list (`method.args = list(...)`), a list or formula of
selectors (`fmt_fun = everything() ~ 1L`), an expression not in quotes
(a `post` step's `filter = n >= 2`). A name or a call is the program's
to judge. A name a cardx function passes on through `...` to a stats or
survival function that has no such argument either is said too
(`ard_stats_t_test(var.eqaul = TRUE)`:
[`t.test()`](https://rdrr.io/r/stats/t.test.html) would ignore it), and
one `ard_car_anova()` passes on to the
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) method of the
model its `x` fits (`x = lm(...), tpye = "III"`: `Anova.lm()` would
ignore it and give type II). The same for the cards / cardx calls in a
`custom` row's `code`, written with their package
(`cards::ard_tabulate(data, denominatr = ...)`,
`cardx::ard_categorical_ci(..., method = "wilson_cc")`) or by the plain
name of a function cards or cardx exports (`ard_tabulate(...)` after
[`library(cards)`](https://github.com/pharmaverse/cards), unless the
code binds that name itself), inside a pipe or an `ard_stack()` too; the
rest of the code is the program's.

## The analyses sheet

`strata` names the variables the analysis is repeated within (cards'
strata: a subgroup, a parameter by visit), `|` between them.
`denominator` says what the percentages are of: `population`, `row`,
`column` or `cell`, another population, a dataset, an analysis data, an
object, or numbers (`86`, or `c("Drug X" = 86, "Placebo" = 85)`, each
named by a value of the one `by`); blank is the method's default. A
`custom` analysis takes neither (its code says it), and a method whose
function has no such argument is refused.

`method` is a keyword of
[`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md),
a `pkg::function`, or a function of the study's own: the study key
`source` (on the `study` sheet, `|` between files, relative to the study
folder) names the R files that define it.

`args` is read as the arguments of a call, in R (`conf.level = 0.9`),
and one that does not parse is refused. An argument a column also gives
(`by`, `variables`, `strata`, `denominator`, `statistics`, `overall`)
may not be in both: write it in one place.
