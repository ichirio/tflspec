# Make the study's ARD from its definition

Runs
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)
from the study folder: the ARD is exactly what the code gives.

## Usage

``` r
tfl_build_ard(
  spec,
  dir = ".",
  output_id = NULL,
  save = TRUE,
  statistics = NULL,
  methods = NULL,
  codelists = NULL
)
```

## Arguments

- spec:

  An
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  (or the path of one).

- dir:

  The study folder (the code's working directory).

- output_id:

  Only these outputs' analyses; `NULL` for all.

- save:

  `FALSE` leaves out the final
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). For one report's
  `"body"`, `TRUE` ends it in `save_ard()` (its rows into the study ARD,
  with the definition's fingerprint), `FALSE` in `ard`.

- statistics, methods:

  The catalogs
  ([`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md),
  [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md))
  to use instead of the current ones.

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

The ARD (with `output_id`, `analysis_id`, `population_id`), invisibly;
with `save`, also written where the study key `output` says.
