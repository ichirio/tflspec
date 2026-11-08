# The arguments of one ARD function, and how each is filled in

Read from the function's own
[`formals()`](https://rdrr.io/r/base/formals.html) (its data frame
method), so it is always the installed version's: every argument, its
default (as R text; `NA` when it has none and must be given) and, from
the catalog (`inst/ard/args.csv`, a company's rows on top), its `kind`
and a `hint`.

## Usage

``` r
tfl_ard_args(call)
```

## Arguments

- call:

  The function, as an analysis names it: `"cards::ard_summary"`.

## Value

A data frame, one row per argument (`...` left out, the arguments it
passes on through `...` after its own): `arg`, `default`, `required`,
`kind`, `hint`, `choices`, `column`, `passed_to` (`""` for the
function's own argument).

## Details

`kind` says what fills it: `data` (the analysis data), `columns` /
`column` (columns of the data), `levels` (levels of a column),
`denominator`, `statistics`, `number`, `logical`, `choice`, `text`,
`formula`, or `code` (R written as is). An argument the catalog does not
describe gets one from its default: `TRUE` / `FALSE` -\> `logical`, a
number -\> `number`, a character vector -\> `choice`, else `code`.
`choices` are the catalog's, or the values of a character vector default
(`method = c("waldcc", "wald", ...)`).

`column` says where an analysis row writes it: the `by`, `variables`,
`strata`, `denominator` or `statistics` column, or `args` (a parent
row's `ard_stack(.by =)`, `ard_strata(.by =, .strata =)` and
`ard_pairwise(variable =)` are its `by`, `strata` and `variables`). The
data is the row's own (`data`).

A `code` argument whose function takes one shape of R has its value
checked by that shape: a list or formula of cards' selectors (`fmt_fun`,
`stat_label`), a list (`method.args`), a function (`tidy_fun`), an
expression of the ARD's columns (a `post` step's `filter`, `expr`,
`rows`), the stratified Wilson `weights` (numbers between 0 and 1 that
add up to 1), the levels `ard_pairwise(include =)` compares (a vector),
the format of one statistic (`add_calculated_row(fmt_fun =)`,
`update_ard_fmt_fun(fmt_fun =)`: a whole number of decimals, `"xx.x"` /
`"xx.x%"`, or a function) and the value
`replace_null_statistic(value =)` gives (one value).

A company adds its own rows with
`options(tflspec.ard_args = <data frame>)`: `call` (or `*` for every
function), `arg`, `kind`, `hint`, `choices`; a row for the same call and
argument wins, and a row for the function wins over a `*` row.

The arguments a function passes on through `...` to the function that
does the work follow its own, with `passed_to` naming that function:
`ard_regression(exponentiate = TRUE)` (hazard or odds ratios, to
[`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html)),
`ard_car_anova(type = "III")` (to
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)),
`ard_stats_t_test(var.equal = TRUE)`,
`ard_stats_chisq_test(correct = FALSE)`,
`ard_stats_mantelhaen_test(exact = TRUE)` ... (to `stats`' tests),
`ard_continuous_ci(exact = FALSE)` (the median's CI, to
[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)),
`ard_smd_smd(gref = 2L)` (to
[`smd::smd()`](https://bsaul.github.io/smd/reference/smd.html)),
`ard_effectsize_cohens_d(pooled_sd = FALSE)` (to `effectsize`),
`ard_survival_survdiff(timefix = FALSE)` (to
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html)).
They are written in `args` like the function's own, and their values are
checked by their kind. The catalog is `inst/ard/passed_args.csv`
(`call`, `arg`, `passed_to`, `default`, `kind`, `hint`, `choices`); a
company adds rows with
`options(tflspec.ard_passed_args = <data frame>)`.

The arguments of a model cardx fits itself, written in
`method.args = list(...)` (`list(conf.type = "log-log")` of a
Kaplan-Meier estimate, `list(ties = "breslow")` of a Cox model), are the
model function's:
[`tfl_ard_model_args()`](https://ichirio.github.io/tflspec/reference/tfl_ard_model_args.md)
lists them.

## Examples

``` r
tfl_ard_args("cardx::ard_categorical_ci")
#>               arg
#> 1            data
#> 2       variables
#> 3              by
#> 4          method
#> 5     denominator
#> 6      conf.level
#> 7           value
#> 8          strata
#> 9         weights
#> 10 max.iterations
#>                                                                                                                        default
#> 1                                                                                                                         <NA>
#> 2                                                                                                                         <NA>
#> 3                                                                                                      dplyr::group_vars(data)
#> 4  c("waldcc", "wald", "clopper-pearson", "wilson", "wilsoncc", "strat_wilson", "strat_wilsoncc", "agresti-coull", "jeffreys")
#> 5                                                                                                   c("column", "row", "cell")
#> 6                                                                                                                         0.95
#> 7                                                                        list(where(is_binary) ~ 1L, where(is.logical) ~ TRUE)
#> 8                                                                                                                         NULL
#> 9                                                                                                                         NULL
#> 10                                                                                                                          10
#>    required        kind
#> 1      TRUE        data
#> 2      TRUE     columns
#> 3     FALSE     columns
#> 4     FALSE      choice
#> 5     FALSE denominator
#> 6     FALSE      number
#> 7     FALSE      levels
#> 8     FALSE     columns
#> 9     FALSE        code
#> 10    FALSE      number
#>                                                                                                                                                              hint
#> 1                                                                                   The analysis data (the data of the analysis row: its dataset and population).
#> 2                                                                                            The analysed columns; several may be given (one call analyses each).
#> 3                                        The group columns, e.g. the treatment (TRT01A): the analysis is done within each group (in a test: the groups compared).
#> 4                                                                                                                                              The method to use.
#> 5               What a percentage divides by: the population, or cards' row / column / cell. The population gives the usual "n (%) of the subjects of the group".
#> 6                                                                                                              Confidence level, a number between 0 and 1 (0.95).
#> 7                                        The level whose proportion is estimated, e.g. list(AVALC = "CR"). Use it for a rate with its CI, e.g. the response rate.
#> 8                             The strata of the stratified Wilson interval (method = strat_wilson / strat_wilsoncc): one interval over them, not one per stratum.
#> 9  Stratum weights for the stratified Wilson methods (strat_wilson): numbers between 0 and 1 that add up to 1, one a stratum, c(0.5, 0.3, 0.2); blank: estimated.
#> 10                                                                                          Iterations for estimating the stratified Wilson weights (default 10).
#>                                                                                                           choices
#> 1                                                                                                                
#> 2                                                                                                                
#> 3                                                                                                                
#> 4  waldcc | wald | clopper-pearson | wilson | wilsoncc | strat_wilson | strat_wilsoncc | agresti-coull | jeffreys
#> 5                                                                                             column | row | cell
#> 6                                                                                                                
#> 7                                                                                                                
#> 8                                                                                                                
#> 9                                                                                                                
#> 10                                                                                                               
#>         column passed_to
#> 1         data          
#> 2    variables          
#> 3           by          
#> 4         args          
#> 5  denominator          
#> 6         args          
#> 7         args          
#> 8       strata          
#> 9         args          
#> 10        args          
```
