# What went wrong while an ARD was made

cards does not stop when one analysis fails: a test that cannot be run
(two groups needed, three given), a statistic whose function stops or
warns, leaves its message in the ARD's `error` / `warning` column and
the other analyses carry on. `tfl_ard_conditions()` lists those
messages, one row per analysis, variable, groups and message, so a
screen can show them and a program can stop on them. It is what
[`cards::print_ard_conditions()`](https://pharmaverse.github.io/cards/latest-tag/reference/print_ard_conditions.html)
prints, as a table, with the study ARD's `output_id` and `analysis_id`.

## Usage

``` r
tfl_ard_conditions(ard)
```

## Arguments

- ard:

  An ARD: the study's
  ([`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)),
  one report's
  ([`tfl_ard_for()`](https://ichirio.github.io/tflspec/reference/tfl_ard_for.md)),
  or any cards ARD.

## Value

A data frame: `output_id` and `analysis_id` (when the ARD has them),
`variable`, `groups` (the group columns, `|` between them; empty for
none), `level` (`"error"` or `"warning"`), `message`, and `statistics`
(the statistics it is about, `, ` between them). Errors first. No rows:
nothing went wrong.

## See also

[`tfl_check_ard()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard.md)
for whether a report can be made from an ARD.

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_summary(cards::ADSL, variables = AGE,
    statistic = ~ list(mean = mean, bad = function(x) stop("no data")))
  tfl_ard_conditions(ard)
}
#>   variable groups level message statistics
#> 1      AGE        error no data        bad
```
