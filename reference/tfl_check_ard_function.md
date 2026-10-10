# Try an ARD function of one's own

Calls `fun(data, ...)` and checks what it gives: a cards ARD (class
`card`,
[`cards::check_ard_structure()`](https://pharmaverse.github.io/cards/latest-tag/reference/check_ard_structure.html)
as notes), the columns every ARD has, and the statistics it says it
gives among its rows. An error or a warning in the call is reported, not
raised.

## Usage

``` r
tfl_check_ard_function(fun, data, ..., stat_names = NULL)
```

## Arguments

- fun:

  The function, or its name.

- data:

  The data to try it on.

- ...:

  Its other arguments, as an analysis row would give them (unquoted
  column names are passed as they are).

- stat_names:

  The statistics it should give; `NULL` (default): the ones the function
  declares (`attr(fun, "stat_names")`, as
  [`cards::as_cards_fn()`](https://pharmaverse.github.io/cards/latest-tag/reference/as_cards_fn.html)
  sets it), if any.

## Value

A data frame of problems as
[`tfl_check_ard()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard.md)
(`level`, `check`, `message`); no rows: it behaves. The ARD it gave is
the attribute `ard`.

## Details

A function says which statistics it gives the way cards' own do:
`cards::as_cards_fn(fun, stat_names = c("estimate", "p.value"))` (the
templates of
[`tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.md)
are written so). One that says nothing is tried all the same, with a
note.

An analysis row calls the function as
`fun(<the analysis data>, by = , variables = , <args>)` (see
[`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md)),
so try it the same way:
`tfl_check_ard_function(ard_riskdiff_mn, adsl, by = TRT01A, variables = AEFL)`.
