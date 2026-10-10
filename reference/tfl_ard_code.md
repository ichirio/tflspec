# The R code that makes the study's ARD

One cards / cardx call per analysis row, each result under a name of its
own (`ard_<analysis_id>`) and tagged with its `output_id`, `analysis_id`
and `population_id` (`tag_ard()`), bound into one ARD and saved to the
study key `output` (default `output/ard/ard.rds`). The code runs from
the study folder, and reads as a person writes it: the tidyverse layout
(a call on one line when it fits in 80 characters, else an argument a
line), dplyr verbs for the data
([`filter()`](https://rdrr.io/r/stats/filter.html), `mutate()`,
`select()`), and a `custom` analysis's code with the program's names in
place of `data` and `population` (in
[`local()`](https://rdrr.io/r/base/eval.html) only when it needs a scope
of its own: it defines a function, or assigns a name the program has).

## Usage

``` r
tfl_ard_code(
  spec,
  output_id = NULL,
  save = TRUE,
  part = c("all", "setup", "body"),
  statistics = NULL,
  methods = NULL,
  dir = ".",
  codelists = NULL
)
```

## Arguments

- spec:

  An
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  (or the path of one).

- output_id:

  Only these outputs' analyses; `NULL` for all.

- save:

  `FALSE` leaves out the final
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). For one report's
  `"body"`, `TRUE` ends it in `save_ard()` (its rows into the study ARD,
  with the definition's fingerprint), `FALSE` in `ard`.

- part:

  `"all"` (the whole program), `"setup"` or `"body"`.

- statistics, methods:

  The catalogs
  ([`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md),
  [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md))
  to use instead of the current ones.

- dir:

  The study folder: the fingerprints saved with the ARD
  ([`tfl_ard_spec_hash()`](https://ichirio.github.io/tflspec/reference/tfl_ard_spec_hash.md))
  read the study's own function files from it.

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

The code, one element per line.

## Details

The formats (the method's and the row's `formats`, over each statistic's
default in
[`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md))
are in the cards call itself, its `fmt_fun` argument, where a reader
sees them next to the statistics:
`fmt_fun = everything() ~ modifyList(fmt_default, list(mean = 2L))`, a
variable's own (`BMIBL:sd=3`) with its own list. An integer is that many
decimals, `xx.x%` is `label_round(1, scale = 100)` and `pvalue` the
program's `fmt_pvalue()` (`<0.001` or 3 decimals);
[`cards::apply_fmt_fun()`](https://pharmaverse.github.io/cards/latest-tag/reference/apply_fmt_fun.html)
then fills `stat_fmt`. What takes no `fmt_fun` gets the same formats
after the call, from `fmt_ard()`: cardx's tests, CIs and models,
[`cards::ard_stack_hierarchical()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_stack_hierarchical.html),
a study's own function, `custom` code, `ard_pairwise()`, `ard_stack()`'s
own rows (the by counts, the total N), an analysis whose `args` gives
`fmt_fun` or whose `post` changes the ARD, a variable's own format for
`ard_hierarchical()` or `ard_tabulate_rows()`. `stat_fmt` is the same
either way.

The functions the program calls – `set_levels()`, `tag_ard()`,
`fmt_ard()`, `keep_stats()`, `fmt_pvalue()`, `save_ard()` – are
[`tfl_helpers_code()`](https://ichirio.github.io/tflspec/reference/tfl_helpers_code.md)'s:
the whole program (`part = "all"`) carries them, and a study keeps them
in a file of its own, so its programs run without tflspec.

`part` gives a piece of it instead, for a program layout of one's own
(tflplanner writes one program per output that sources a shared setup):
`"setup"` is what every piece starts with –
[`library(cards)`](https://github.com/pharmaverse/cards),
[`library(dplyr)`](https://dplyr.tidyverse.org), every computed
statistic of the catalog (`tfl_stats`) and each statistic's format
(`fmt_default`); `"body"` is the analyses of `output_id`, starting with
`report_id <- "..."` and its code lists (`cl_<variable>`), and ending in
`save_ard()` (one report, `save = TRUE`) or `ard`.
