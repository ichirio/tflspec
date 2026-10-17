# R objects of the YAML spec

The values a spec may hold that are not plain data, and the pieces a
report is made of:

## Usage

``` r
tfl_r(code)

tfl_unset()

tfl_call(fn, args = list(), package = NULL)

tfl_item(fn, args = list(), from = "own")

tfl_output(
  id,
  kind = c("table", "figure", "listing"),
  label = NA_character_,
  population = NA_character_,
  extends = "defaults",
  data = NULL,
  codelists = list(),
  content = list(),
  page = list(),
  ard = NULL
)
```

## Arguments

- code:

  One string of R code.

- fn:

  A function's name (`plan_cells`,
  [`cards::ard_summary`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_summary.html),
  `rtf_border`).

- args:

  The arguments, a named list.

- package:

  The package `fn` is in, when its name does not say.

- from:

  Where an item came from: `"own"`, `"defaults"`, `"pattern:<name>"`.

- id:

  The report's id (letters, digits, `.`, `_`, `-`); the file is
  `spec/outputs/<id>.yml`.

- kind:

  `"table"`, `"figure"` or `"listing"`.

- label, population:

  The report's label and analysis set (`NA`: none).

- extends:

  The files layered under this one, nearest first (`"defaults"`,
  `"patterns/<name>"`); `"none"` for none.

- data:

  What the report reads, its step 1 (a list: `source` – `"ard"`,
  `"code"` or `"dataset"` – and `object`, `steps`, `code`, `dataset`
  ...).

- codelists:

  The report's code lists, by variable.

- content:

  The layers, a list of `tfl_item`.

- page:

  The document: `titles`, `footnotes`, `header`, `footer` (maps by
  line), `tokens`, and the `rtf_*` calls' arguments.

- ard:

  The report's part of the study ARD, made in the ARD stage before any
  report (a list: `analysis_data`, `analyses`); `NULL` none.

## Value

`tfl_r()`: a string of class `tfl_r`; `tfl_unset()`: an object of class
`tfl_unset`; the rest: lists of class `tfl_call`, `tfl_item`,
`tfl_output`.

## Details

- `tfl_r()`: a string of R code, written as the `!r` tag
  (`where: !r "p > 0.5"`); kept as it is and put into the program
  unquoted.

- `tfl_unset()`: the `!unset` tag – a value a report sets to nothing,
  overriding its defaults (writing nothing leaves the default). It is
  resolved when the defaults are layered; the generators never see one.

- `tfl_call()`: a nested call (`{fn: rtf_border, args: {top: single}}`),
  for an argument that is itself a constructor's result.

- `tfl_item()`: one layer of `content:` – a function (`fn`), its
  arguments by name (`args`), and where it came from (`from`: `"own"`,
  `"defaults"` or `"pattern:<name>"`).

- `tfl_output()`: one report: `id`, `kind`, `label`, `population`,
  `extends`, `data`, `codelists`, `content` (a list of `tfl_item`) and
  `page`.

## Examples

``` r
it <- tfl_item("plan_cells", list(continuous = list(n = "{N}")))
out <- tfl_output("T-1", "table", content = list(it))
out$content[[1]]$fn
#> [1] "plan_cells"
```
