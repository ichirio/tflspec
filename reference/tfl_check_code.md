# Check a report's own code against the rules

A report's data step or content step may be the user's own R code (the
report sheet's `data_code` / `content_code`: a step whose code is not
blank is its code). The code follows rules the program around it relies
on: the datasets it reads are the report's (registered in its data step:
the program reads them first, by their names in lower case), this
report's ARD rows are `ard` when the report reads its ARD, and the step
leaves the object the next one reads – the data step `data` (a table or
a listing; a table's may leave `ard` alone, which the program
normalizes) or `df` (a figure), the content step `content`. The check
reads the code; it does not run it.

## Usage

``` r
tfl_check_code(
  code,
  step = c("data", "content"),
  type = c("table", "listing", "figure"),
  datasets = character(),
  catalog = character(),
  ard = FALSE,
  output_id = NA_character_
)
```

## Arguments

- code:

  The code: a string (lines joined by newlines) or a character vector of
  lines.

- step:

  `"data"` or `"content"`.

- type:

  The report's type: `"table"`, `"listing"` or `"figure"`.

- datasets:

  The datasets the report registers (any case).

- catalog:

  The datasets of the study's catalog (any case): a name of these the
  code uses and does not make itself must be registered.

- ard:

  Whether the report reads its ARD (`ard` is in reach).

- output_id:

  The report, for the messages.

## Value

A `tfl_review`
([`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)'s
rows: sheet `report`, field `data_code` / `content_code`), with the
attribute `datasets`: the catalog's datasets the code uses (upper case),
registered or not.

## Details

The rules
([`tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.md),
area `program`):

- U01 (error): the code does not read as R.

- U02 (error): it names a dataset of the catalog the report does not
  register.

- U03 (error): it leaves nothing for the next step (no assignment to
  `data` / `df` / `content`).

- U04 (check): it reads files itself, attaches packages or changes the
  session ([`readRDS()`](https://rdrr.io/r/base/readRDS.html),
  [`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html),
  [`library()`](https://rdrr.io/r/base/library.html),
  [`setwd()`](https://rdrr.io/r/base/getwd.html),
  [`options()`](https://rdrr.io/r/base/options.html) ...).

- U05 (error): it uses `ard`, but the report does not read its ARD.

## Examples

``` r
code <- "df <- advs |> dplyr::filter(PARAMCD == 'SYSBP')"
r <- tfl_check_code(code, "data", "figure", datasets = "ADSL",
                    catalog = c("ADSL", "ADVS"), output_id = "F-1")
r[, c("rule", "level", "message")]
#> <tfl_review> 1 error, 0 to check, 0 to set by hand
#>   
attr(r, "datasets")
#> [1] "ADVS"
```
