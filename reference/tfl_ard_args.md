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

A data frame, one row per argument (`...` left out): `arg`, `default`,
`required`, `kind`, `hint`, `choices`, `column`.

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

A company adds its own rows with
`options(tflspec.ard_args = <data frame>)`: `call` (or `*` for every
function), `arg`, `kind`, `hint`, `choices`; a row for the same call and
argument wins, and a row for the function wins over a `*` row.

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
#>                                                                                                                                                 hint
#> 1                                                                      The analysis data (the data of the analysis row: its dataset and population).
#> 2                                                                               The analysed columns; several may be given (one call analyses each).
#> 3                           The group columns, e.g. the treatment (TRT01A): the analysis is done within each group (in a test: the groups compared).
#> 4                                                                                                                                 The method to use.
#> 5  What a percentage divides by: the population, or cards' row / column / cell. The population gives the usual "n (%) of the subjects of the group".
#> 6                                                                                                 Confidence level, a number between 0 and 1 (0.95).
#> 7                           The level whose proportion is estimated, e.g. list(AVALC = "CR"). Use it for a rate with its CI, e.g. the response rate.
#> 8                The strata of the stratified Wilson interval (method = strat_wilson / strat_wilsoncc): one interval over them, not one per stratum.
#> 9                                                          Stratum weights for the stratified Wilson methods (strat_wilson), as R; blank: estimated.
#> 10                                                                             Iterations for estimating the stratified Wilson weights (default 10).
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
#>         column
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
