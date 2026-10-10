# Start an ARD function of one's own from a template

Writes the skeleton of an `ard_*()` function an analysis row can name as
its method, ready to edit, in one of three shapes – the three ways cards
and cardx write their own:

## Usage

``` r
tfl_ard_function_template(
  name,
  type = c("summary", "test", "free"),
  file = NULL,
  test = FALSE,
  overwrite = FALSE
)
```

## Arguments

- name:

  The function's name (`ard_riskdiff`).

- type:

  `"summary"`, `"test"` or `"free"`.

- file:

  Where to write it (`programs/ard/functions/ard_riskdiff.R`); `NULL`:
  not written, the code is returned.

- test:

  With `file`, also write `test-<name>.R` next to it.

- overwrite:

  Replace a file that is there?

## Value

The function's code, one element per line (invisibly when written).

## Details

- `"summary"`: statistics of one's own on numeric variables, by group
  (`cards::ard_summary(statistic = )`; a coefficient of variation and a
  geometric mean as the example);

- `"test"`: a test across groups made an ARD with
  [`cards::tidy_as_ard()`](https://pharmaverse.github.io/cards/latest-tag/reference/tidy_as_ard.html)
  (a Wilcoxon rank-sum test as the example); its errors and warnings go
  into the ARD, as cardx's tests do;

- `"free"`: any calculation, group by group, with
  [`cards::ard_strata()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_strata.html)
  and
  [`cards::ard_identity()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_identity.html).

Each declares the statistics it gives
(`cards::as_cards_fn(stat_names = )`), so
[`tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.md)
checks that it gives them. With `test = TRUE` a testthat file is written
next to it, which runs that check on cards' example data. The function
is called as an analysis row calls it:
`fun(data, by = , variables = , <args>)`.

## See also

[`tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.md)

## Examples

``` r
cat(tfl_ard_function_template("ard_cv", "summary"), sep = "\n")
#> #' Statistics of one's own on numeric variables
#> #'
#> #' Statistics cards does not have -- here the coefficient of variation and
#> #' the geometric mean -- for numeric variables, by group, as an ARD.  An
#> #' analysis row names it as its method (ard_cv, by TRT01A, variables
#> #' AGE | BMIBL).  Write your statistics in `statistic = ` and name them in
#> #' `stat_names`, so a check can see that it gives them
#> #' (tflspec::tfl_check_ard_function()).
#> #'
#> #' @param data The analysis data (its dataset and analysis set).
#> #' @param by The group columns, e.g. TRT01A.
#> #' @param variables The numeric variables.
#> #' @param ... Passed to cards::ard_summary().
#> ard_cv <- cards::as_cards_fn(
#>   function(data, by = NULL, variables, ...) {
#>     cards::ard_summary(
#>       data,
#>       by = {{ by }},
#>       variables = {{ variables }},
#>       statistic = ~ list(
#>         cv = function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100,
#>         geo_mean = function(x) exp(mean(log(x[!is.na(x) & x > 0])))
#>       ),
#>       ...
#>     )
#>   },
#>   stat_names = c("cv", "geo_mean")
#> )

f <- file.path(tempdir(), "ard_cv.R")
tfl_ard_function_template("ard_cv", "summary", file = f, test = TRUE)
source(f)
if (requireNamespace("cards", quietly = TRUE)) {
  tfl_check_ard_function(ard_cv, cards::ADSL, by = ARM, variables = AGE)
}
#> [1] level   check   message
#> <0 rows> (or 0-length row.names)
```
