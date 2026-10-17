# The ARD functions an analysis may call

Every `ard_*()` of cards and cardx that is installed, with what a person
reads to choose one: its `category` (the group it is listed under), a
`label` (the heading) and a one-line `description`, and its `shape` –
how it takes its input:

## Usage

``` r
tfl_ard_functions(installed = TRUE)
```

## Arguments

- installed:

  `TRUE` (default) only the functions that can be called here (their
  package installed); `FALSE` every row of the catalog.

## Value

A data frame: `call`, `category`, `label`, `description`, `shape`,
`replaced_by`, `offered`, `installed`, `in_catalog`.

## Details

- `variables`: columns (`by`, `variables`, `strata`) of the data;

- `formula`: a model formula (`AVAL ~ TRT01P`) and the data;

- `model`: a fitted model, written in `args`
  (`x = lm(..., data = data)`);

- `columns`: named columns of its own (`time`, `count`; `postbaseline`);

- `wrapper`: runs other analyses (`ard_stack()`, `ard_strata()`,
  `ard_pairwise()`);

- `data`: the data alone.

An analysis row names one of them as its `method`
([`cards::ard_summary`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_summary.html)),
as it names any function; this list is the catalog that describes them.
A function the catalog does not describe (one a newer cards adds) is
listed too, with `in_catalog = FALSE` and its help page's title as its
label. `replaced_by` names the new name of an old one. `offered` is
`FALSE` for what a new analysis is not to choose: the old names and the
functions not offered by design (below); a screen lists them only for an
analysis that already names one.

The versions it is written for: cards \>= 0.8.0 and cardx \>= 0.3.1 (the
oldest are tested, and every week what CRAN has now).

## Not offered, by design (decided 2026-10-04)

Listed, but not for an analysis row:

- the **old names** (`ard_continuous()`, `ard_categorical()`,
  `ard_dichotomous()`, `ard_complex()`, `ard_categorical_max()`,
  `ard_emmeans_mean_difference()`): the function in `replaced_by` gives
  the same result;

- the **survey design** functions (`ard_survey_svychisq()`,
  `ard_survey_svyranktest()`, `ard_survey_svyttest()`, and the
  survey.design methods of the summaries): their input is a survey
  design object, not the data frame an analysis row reads;

- **`ard_formals()`**: it records a function's argument values, a
  building block for writing an ARD function, not an analysis.

A company adds its own rows – its own ARD functions, other headings –
with `options(tflspec.ard_functions = <data frame>)` (the same columns;
a row with the same `call` replaces the built-in one).

## See also

[`tfl_ard_args()`](https://ichirio.github.io/tflspec/reference/tfl_ard_args.md)
for one function's arguments;
[`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md)
for the keywords an analysis may name.

## Examples

``` r
f <- tfl_ard_functions()
f[f$category == "Summaries", c("call", "label")]
#>                         call                          label
#> 1         cards::ard_summary             Summary statistics
#> 2        cards::ard_tabulate            Counts and percents
#> 3  cards::ard_tabulate_value             Count of one level
#> 4         cards::ard_missing                 Missing counts
#> 5   cards::ard_tabulate_rows                 Number of rows
#> 59     cards::ard_continuous  Summary statistics (old name)
#> 60    cards::ard_categorical Counts and percents (old name)
#> 61    cards::ard_dichotomous  Count of one level (old name)
```
