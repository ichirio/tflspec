# Read a define spec workbook (an import profile)

Reads the workbook a define is made from – by default in the Pinnacle 21
layout
([`tfl_define_profile()`](https://ichirio.github.io/tflspec/reference/tfl_define_profile.md))
– and gives the same `tfl_define` as
[`tfl_read_define()`](https://ichirio.github.io/tflspec/reference/tfl_read_define.md),
so
[`tfl_define_codelists()`](https://ichirio.github.io/tflspec/reference/tfl_define_codelists.md)
takes either.

## Usage

``` r
tfl_read_define_spec(path, map = NULL, profile = "p21")
```

## Arguments

- path:

  An `.xlsx` workbook (needs readxl).

- map:

  Changes to `profile`: a list of parts, each a list of fields, e.g.
  `list(codelists = list(sheet = "CT", decode = "Decode"))`.

- profile:

  The profile: a name for
  [`tfl_define_profile()`](https://ichirio.github.io/tflspec/reference/tfl_define_profile.md),
  or a whole profile list.

## Value

A `tfl_define` (see
[`tfl_read_define()`](https://ichirio.github.io/tflspec/reference/tfl_read_define.md)),
with `attr(, "unread")`.

## Details

- Sheet and column names are matched ignoring case and surrounding
  blanks. A sheet the profile names that is not there is an error (but
  `external`, read when there); so is a required column. An optional
  column that is not there is read as empty and listed in
  `attr(, "unread")` (part, field, column), for the user to see.

- A term is `extended` when the profile maps an `extended` column
  (`Yes`, `Y`, `TRUE`, `1`); else when its code list has an NCI code and
  the term has none (how Pinnacle 21 marks a sponsor's term).

- A code list's `kind` is `"item"` when its values have decodes, else
  `"enumerated"`.

- `order`, `length` and `digits` must be whole numbers.

## Examples

``` r
if (requireNamespace("readxl", quietly = TRUE)) {
  xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL",
                          Description = "Subject-Level Analysis Dataset"),
    Variables = data.frame(Order = 1:2, Dataset = "ADSL",
                           Variable = c("SEX", "AGE"),
                           Label = c("Sex", "Age"),
                           Codelist = c("CL.SEX", NA)),
    Codelists = data.frame(ID = "CL.SEX", Name = "Sex", Order = 1:2,
                           Term = c("M", "F"),
                           `Decoded Value` = c("Male", "Female"),
                           check.names = FALSE)), xlsx)
  def <- tfl_read_define_spec(xlsx)
  def$codelists
  attr(def, "unread")
}
#>         part         field             column
#> 1   datasets         class              Class
#> 2   datasets     structure          Structure
#> 3   datasets       purpose            Purpose
#> 4   datasets          keys      Key Variables
#> 5   datasets     repeating          Repeating
#> 6   datasets     reference     Reference Data
#> 7  variables          type          Data Type
#> 8  variables        length             Length
#> 9  variables        digits Significant Digits
#> 10 variables        format             Format
#> 11 variables     mandatory          Mandatory
#> 12 variables          role               Role
#> 13 variables        origin             Origin
#> 14 codelists          type          Data Type
#> 15 codelists codelist_code  NCI Codelist Code
#> 16 codelists          code      NCI Term Code
```
