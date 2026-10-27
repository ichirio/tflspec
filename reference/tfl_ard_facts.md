# What an ARD holds, for the review

The groups (and their levels), the variables (their levels, statistics
and contexts) and the statistics of an ARD, and what went wrong while it
was made
([`tfl_ard_conditions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_conditions.md)):
what
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
checks a report's table definition against when the report's ARD exists.
Put a report's facts in `facts$ard[[output_id]]`.

## Usage

``` r
tfl_ard_facts(ard)
```

## Arguments

- ard:

  An ARD (a cards ARD or a data frame with its columns).

## Value

A list: `groups` (a named list: each group variable's levels),
`variables` (a named list: each variable's `levels`, `stats` and
`contexts`), `stats` (every statistic) and `conditions`.

## See also

[`tfl_data_facts()`](https://ichirio.github.io/tflspec/reference/tfl_data_facts.md),
[`tfl_check_ard()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_summary(cards::ADSL, by = ARM, variables = AGE)
  str(tfl_ard_facts(ard)$variables$AGE$stats)
}
#>  chr [1:8] "N" "mean" "sd" "median" "p25" "p75" "min" "max"
```
