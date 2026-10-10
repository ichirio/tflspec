# The functions a study's generated programs call

The R code of the functions the programs tflspec writes call:
`set_levels()` (the code lists on a data: each listed column a factor in
the list's order, its values as the list has them in the ARD; a value
not listed stops the program), `tag_ard()` (the report, analysis and
analysis set in front of an analysis's ARD), `fmt_ard()` and
`fmt_pvalue()` (formats after a call), `keep_stats()` (only the
statistics asked for), `save_ard()` (one report's rows into the study
ARD, and what was built in `ard_status.csv`), and what a figure reads an
ARD with: `ard_value()` (one statistic, as text), `ard_stats()`
(statistics as a data frame) and `ard_fingerprint()` (the fingerprint
`ard_status.csv` recorded for a report). Base R, cards and dplyr only: a
study writes them once into a file of its own (tflplanner:
`programs/study_helpers.R`, sourced by its setup) and its programs run
without tflspec.
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)'s
whole program (`part = "all"`) carries them itself.

## Usage

``` r
tfl_helpers_code()
```

## Value

The code, one element per line.

## Examples

``` r
cat(head(tfl_helpers_code(), 12), sep = "\n")
#> # The functions a study's generated programs call: the code lists on a
#> # data, the ids in front of an analysis's ARD, the formats after a call,
#> # the statistics asked for, one report's rows into the study ARD, and a
#> # figure's reading of an ARD (one statistic, statistics as a data frame,
#> # the fingerprint of what was built).
#> # Base R, cards and dplyr only: the programs run without tflspec.
#> 
#> # The code lists on a data: each listed column a factor, in its list's
#> # order, each value as the list has it in the ARD (a list without names:
#> # the values themselves).  A value its list does not have stops the
#> # program and says which (a missing value does not: it stays missing).
#> #   set_levels(adsl, SEX = c(F = "Female", M = "Male"), TRT01A = cl_trt01a)
```
