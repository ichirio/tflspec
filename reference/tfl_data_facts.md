# What the data hold, for the review

Reads the datasets once and keeps the facts
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
checks a definition against: every column's class, label, number of
distinct values and, for a character or factor column with at most
`max_levels` of them, the values; each dataset's rows and subjects; each
analysis set's subjects and the values of its flag; and each condition's
rows and subjects (or why it cannot be evaluated). The facts are small
(a few KB a dataset) and hold no records, so they can be kept between
sessions.

## Usage

``` r
tfl_data_facts(
  data,
  populations = NULL,
  conditions = NULL,
  max_levels = 200L,
  listings = NULL,
  id = "USUBJID"
)
```

## Arguments

- data:

  The datasets: a named list of data frames (names as the ARD
  definition's `datasets` sheet gives them: `ADSL`, `ADAE` ...).

- populations:

  The analysis sets: the `populations` sheet (`population_id`,
  `dataset`, `where`), or a whole ARD definition
  ([`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  or its sheets), whose analysis data and analyses then give the
  conditions too.

- conditions:

  More conditions to count: a data frame with `dataset`, `where` and
  optionally `population_id` (the analysis set the rows are first
  narrowed to). The ARD definition's own are added when `populations` is
  one.

- max_levels:

  A column with more distinct values than this keeps only their number
  (200, as many as a condition builder offers).

- listings:

  A listing definition
  ([`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)):
  its `where` are counted too.

- id:

  The subject key.

## Value

A `tfl_data_facts`: a list of `datasets` (each `n`, `n_subjects` and
`columns`, a data frame with `name`, `class`, `label`, `n_distinct` and
the list column `values`), `populations` (each `dataset`, `n_subjects`,
`flag`, `flag_values` and the values of the columns the definition reads
among its subjects, `values`), `conditions` (a data frame: `dataset`,
`population_id`, `where`, `n_rows`, `n_subjects`, `error`, and the list
column `values`), `ard` (empty: what an ARD holds,
[`tfl_ard_facts()`](https://ichirio.github.io/tflspec/reference/tfl_ard_facts.md),
is added by name) and `made` (the time).

## See also

[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md),
[`tfl_ard_facts()`](https://ichirio.github.io/tflspec/reference/tfl_ard_facts.md)

## Examples

``` r
adsl <- data.frame(USUBJID = c("1", "2", "3"), SAFFL = c("Y", "Y", "N"),
                   SEX = c("F", "M", "F"), AGE = c(50, 61, 47))
f <- tfl_data_facts(list(ADSL = adsl),
  populations = data.frame(population_id = "SAF", dataset = "ADSL",
                           where = 'SAFFL == "Y"'))
f$populations$SAF$n_subjects
#> [1] 2
f$datasets$ADSL$columns[, c("name", "class", "n_distinct")]
#>      name     class n_distinct
#> 1 USUBJID character          3
#> 2   SAFFL character          2
#> 3     SEX character          2
#> 4     AGE   numeric          3
```
