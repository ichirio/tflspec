# Read and check an ARD definition workbook

Read and check an ARD definition workbook

## Usage

``` r
tfl_read_ard_spec(path, check = TRUE, statistics = NULL, methods = NULL)

tfl_ard_spec(x, statistics = NULL, methods = NULL)
```

## Arguments

- path:

  An `ard_spec.xlsx`.

- check:

  `FALSE` reads a definition still being written without refusing it.

- statistics, methods:

  The catalogs
  ([`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md),
  [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md))
  to use instead of the current ones.

- x:

  A list of the sheets (data frames).

## Value

An `tfl_ard_spec`: a list of the seven sheets (a workbook without
`analysis_data`, written before it was added, reads it empty).

## Problems

`tfl_ard_spec()` stops with every problem it finds, one line each; the
condition (class `tflspec_spec_error`) carries them as rows too,
`cnd$problems`: `output_id`, `sheet`, `row` (the row's key: the
`analysis_id`, the `data_id`, the `population_id`), `field` and
`message`.
[`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
lists them without stopping.

What depends on the installed cards / cardx is a warning: an argument in
`args` or in a `post` step its function does not have, and a value that
does not fit the argument's kind
([`tfl_ard_args()`](https://ichirio.github.io/tflspec/reference/tfl_ard_args.md)):
`TRUE` or `FALSE` for a logical, a number (`conf.level` between 0 and
1), one of the choices (`method = "wilson"`), text in quotes, a formula,
a column, a list (`method.args = list(...)`), a list or formula of
selectors (`fmt_fun = everything() ~ 1L`), an expression not in quotes
(a `post` step's `filter = n >= 2`). A name or a call is the program's
to judge.
