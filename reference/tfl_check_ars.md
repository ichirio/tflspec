# Check a CDISC ARS reporting event

Checks what ARS requires and what the reporting event refers to: each
required field is there (an analysis's `purpose` above all: it is the
SAP's decision, so
[`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
leaves it blank when the spec does), each id is used once in its class,
and each reference (`methodId`, `analysisSetId`, `dataSubsetId`,
`groupingId`, an operation, an output or analysis of the list of
contents) names something that is there. With `schema = TRUE` and the
jsonvalidate package installed, the JSON is also checked against CDISC's
JSON Schema for ARS v1.0
(`system.file("ars", "ars_ldm.json", package = "tflspec")`).

## Usage

``` r
tfl_check_ars(ars, schema = TRUE, profile = NULL)
```

## Arguments

- ars:

  A
  [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md),
  or the path of an ARS `.json` file.

- schema:

  Check against the JSON Schema too (needs jsonvalidate; skipped with a
  message when it is not installed).

- profile:

  `"siera"` checks also what siera needs to run it (three analyses an
  output, one analysis set an output and of one condition, a code
  template for each method, ids of letters, digits and `_`). Default:
  the profile the
  [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
  was written with, else `"cdisc"`.

## Value

A data frame, one row per problem (none: zero rows): `part` (the class
and id), `field` and `problem`.

## Examples

``` r
f <- system.file("ars", "ars_ldm.json", package = "tflspec")
file.exists(f)
#> [1] TRUE
```
