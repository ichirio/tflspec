# The data each analysis reads, by its name in the ARD program

The names
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)
gives the data it makes itself: an analysis set's (`adsl_sub`) and an
analysis's own records (`adae_sub`), numbered from a report's second of
a dataset (`adsl_sub2`); an analysis data a person named (`data_id`) by
its name; a dataset read as it is by the dataset's. For a screen that
shows what an analysis reads under the program's name, without writing
the rule again.

## Usage

``` r
tfl_ard_data_names(spec, output_id = NULL, codelists = NULL)
```

## Arguments

- spec:

  An
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  (or the path of one), or a study of the YAML spec
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)):
  its data catalog and its reports' `ard.analysis_data` /
  `ard.analyses`, the reports in the TOC's order, its code lists the
  reports' own (when `codelists` is `NULL`).

- output_id:

  Only these outputs' analyses; `NULL` for all.

- codelists:

  The reports' code lists: a table definition (its `codelists` sheet) or
  a data frame with `output_id`, `variable`, `value`, `label` and
  `order`. A code list is a report's: every row names its report (a
  blank `output_id` stops). A report's rows of the variables its
  analyses read (`by`, `strata`, `variables`, and the names in `args`,
  `code` and `post`) count: the program writes them at its head
  (`cl_sex <- c(F = "Female", M = "Male")`) and puts them on the data
  the analyses read (`set_levels(SEX = cl_sex)`): each column a factor
  in the list's order, its values the labels (a value without one stays
  itself) – the ARD holds `"Female"`, and counts a level no record has
  (`n = 0`). A value the list does not have (not NA) stops the program.
  An analysis's `where` on such a column is written in its labels: one
  that names a value whose label differs stops here. In the study's
  program, each report's part reads its data again with its own. `NULL`
  (default): the data as read.

## Value

A data frame: `output_id`, `analysis_id` (an analysis that runs inside
another – a stack's, a strata's – is not listed: it reads its parent's),
`data` (the name; `"NULL"` for an analysis that reads none).

## Examples

``` r
tfl_ard_data_names(tfl_ard_spec_template())
#> [1] output_id   analysis_id data       
#> <0 rows> (or 0-length row.names)
```
