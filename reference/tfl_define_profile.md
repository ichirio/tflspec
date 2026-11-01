# An import profile for a define spec workbook

Says which sheet and which column of a define spec workbook is what, for
[`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md).
`"p21"` is the common Pinnacle 21 layout: sheets `Datasets`,
`Variables`, `Codelists` and `Dictionaries`, with their usual column
names. A company whose workbook differs changes the entries that differ,
e.g. `map = list(variables = list(label = "Variable Label"))` in
[`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md),
or edits the whole list and passes it as `profile`.

## Usage

``` r
tfl_define_profile(name = "p21")
```

## Arguments

- name:

  The profile: `"p21"`.

## Value

A list of four lists.

## Details

Each part is a list: `sheet` (the sheet's name), then field = column
name. A field set to `NA` is not read. The fields:

- `datasets`: `dataset` (required), `label`, `class`, `structure`,
  `purpose`, `keys`, `repeating`, `reference`;

- `variables`: `dataset`, `variable` (both required), `label`, `type`,
  `length`, `digits`, `format`, `codelist`, `mandatory`, `order`,
  `role`, `origin`;

- `codelists`: `codelist` (its ID; required), `name`, `type`,
  `codelist_code` (the code list's NCI code), `order`, `value` (the
  term; required), `code` (the term's NCI code), `decode`, `extended`;

- `external` (read when the sheet is there): `codelist`, `name`,
  `dictionary`, `version`.

## Examples

``` r
str(tfl_define_profile()$codelists)
#> List of 10
#>  $ sheet        : chr "Codelists"
#>  $ codelist     : chr "ID"
#>  $ name         : chr "Name"
#>  $ type         : chr "Data Type"
#>  $ codelist_code: chr "NCI Codelist Code"
#>  $ order        : chr "Order"
#>  $ value        : chr "Term"
#>  $ code         : chr "NCI Term Code"
#>  $ decode       : chr "Decoded Value"
#>  $ extended     : logi NA
```
