# A table's plan from its definition

Builds the
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
a table definition stands for: the roles of its `tables` sheet go into
`table_plan()`, and the other sheets become the plan's first layers
through the same verbs
[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md)
writes — so a verb written afterwards still wins, which is how one
report departs from the study's workbook in a line of code. Like
`table_plan()`, the data comes first, so it pipes.

## Usage

``` r
tfl_table_plan(data, spec, output_id = NULL, ...)
```

## Arguments

- data:

  The normalized ARD, as
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  takes it.

- spec:

  A table definition
  ([`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md),
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)),
  or the path(s) of its workbook(s).

- output_id:

  The table, when the definition has several.

- ...:

  Roles for
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  (`cols`, `rows`, `label`, `stat`); one given here wins over the
  workbook's. Everything else is a layer: add its verb after this call.

## Value

An
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## See also

[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md),
the same steps as code;
[`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md),
the other way round.

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE) &&
    requireNamespace("rtfreporter", quietly = TRUE)) {
  spec <- tfl_read_table_spec(system.file("extdata", "ard-spec", "DM.xlsx",
                                          package = "tflspec"))
  ard <- cards::ard_stack(adsl[adsl$SAFFL == "Y", ], .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = c(AGEGR1, SEX)))
  plan <- tfl_table_plan(rtfreporter::normalize_ard(ard), spec)
  # this report's own change, under what the spec says
  plan <- rtfreporter::plan_paginate_rows(plan, max_rows = 40)
  head(rtfreporter::plan_apply(plan, "table"))
}
#> widen_ard(): 18 ARD rows were not used.
#>   categorical    N                8  no template named it
#>   tabulate       N                2  a key variable's own tabulation
#>   tabulate       n                2  a key variable's own tabulation
#>   tabulate       p                2  a key variable's own tabulation
#>   continuous     p25              2  no template named it
#>   continuous     p75              2  no template named it
#>   (notes = FALSE to silence; attr(, "ard_ignored") has the detail)
#>              group     label      Drug X     Control
#> 1      Age (years)         n          80          78
#> 2      Age (years) Mean (SD) 60.1 (9.59) 62.7 (9.76)
#> 3      Age (years)    Median        59.5        62.5
#> 4      Age (years)  Min, Max      39, 82      42, 85
#> 5 Age group, n (%)       <65   57 (71.3)   44 (56.4)
#> 6 Age group, n (%)      >=65   23 (28.8)   34 (43.6)
```
