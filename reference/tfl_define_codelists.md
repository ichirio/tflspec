# A define's code lists as a report's `codelists` rows

Turns the code lists of the variables a report uses into rows of the
spec's `codelists` sheet: `value` the coded value, `label` its decode
(blank for an enumerated value: it prints as itself), `order` its order
number. With `labels`, the variables' labels too, as rows of the
`variables` sheet (`attr(, "variables")`: `output_id`, `variable`,
`label`, `note`): a variable's heading is that sheet's `label` (the code
list of `variable` is an earlier form, moved there when a table spec is
read). The rows are a draft: nothing is written.

## Usage

``` r
tfl_define_codelists(
  define,
  variables = NULL,
  output_id,
  datasets = NULL,
  labels = TRUE
)
```

## Arguments

- define:

  A `tfl_define`
  ([`tfl_read_define()`](https://ichirio.github.io/tflspec/reference/tfl_read_define.md),
  [`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)),
  or the path of a `define.xml` or of a define spec `.xlsx` (read with
  the Pinnacle 21 profile).

- variables:

  The variables (names, matched ignoring case); `NULL`: every variable
  of `datasets` that has a code list.

- output_id:

  The report(s) the rows are for (required).

- datasets:

  The datasets to look in; `NULL` for all.

- labels:

  Add the variables' labels, as `variables` sheet rows in
  `attr(, "variables")`.

## Value

A data frame of the `codelists` sheet's columns, all character:
`output_id`, `variable`, `value`, `label`, `order`, `note`; with
`attr(, "set_by_hand")` and, with `labels`, `attr(, "variables")`.

## Details

Code lists are per report: every row carries an `output_id`, so it is
required. Several output ids give the rows once for each.

A variable is looked up in the datasets given (all when `datasets` is
`NULL`). When it is in several with different code lists (`PARAMCD` of
ADVS and of ADLB) that is an error naming them: give `datasets`. What
cannot be taken from the define is listed in `attr(, "set_by_hand")`
(`variable`, `reason`): a variable it does not have (also a warning),
one with no code list, one whose code list is an external dictionary
(MedDRA ...). Each row's `note` says where it came from
(`define: CL.SEX`; `extended` for a sponsor's value), and is never read
by the spec.

## Examples

``` r
if (requireNamespace("xml2", quietly = TRUE)) {
  def <- tfl_read_define(system.file("extdata", "define", "define.xml",
                                     package = "tflspec"))
  tfl_define_codelists(def, c("SEX", "AGEGR1", "AGE"),
                       output_id = "T-14-1-1", datasets = "ADSL")
}
#>   output_id variable value  label order              note
#> 1  T-14-1-1      SEX     F Female     2    define: CL.SEX
#> 2  T-14-1-1      SEX     M   Male     1    define: CL.SEX
#> 3  T-14-1-1   AGEGR1   <65   <NA>     1 define: CL.AGEGR1
#> 4  T-14-1-1   AGEGR1  >=65   <NA>     2 define: CL.AGEGR1
```
