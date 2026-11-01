# The line that normalizes a table's ARD

A table's program makes `data` from its rows of the study ARD with
[`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)
before the plan. What the normalization needs to know is the report's,
so it is written from its definitions: the nested variables of its
`hierarchical` analysis (the ARD definition's `variables`,
`AEBODSYS | AEDECOD`) as its `hierarchy` – each level's own row, SOC and
PT, with its counts – and the row of the hierarchy's overall counts (the
table definition's `tables$overall_label`, "Subjects with at least one
TEAE", from the analysis's `args` `over_variables = TRUE`) as its
`overall`; and the parallel subgroups stacked into one pair of keys
(`tables$stack`, `subgroup = ALL, SEX, AGEGR1`) as its `stack`. A report
with none: `data <- normalize_ard(ard)`.

## Usage

``` r
tfl_normalize_code(
  spec,
  ard_spec = NULL,
  output_id = NULL,
  ard = "ard",
  data = "data"
)
```

## Arguments

- spec:

  A table definition
  ([`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)),
  or the path(s) of its workbook(s).

- ard_spec:

  The ARD definition
  ([`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)),
  for the hierarchy; `NULL` for none.

- output_id:

  The table, when the definitions have several.

- ard, data:

  The names of the ARD and of the normalized data.

## Value

The line, one string.

## Examples

``` r
sp <- tfl_table_spec(tables = data.frame(
  output_id = "T-2", cols = "TRTA", rows = "group1 = AEBODSYS",
  overall_label = "Subjects with at least one TEAE"))
ard <- tfl_ard_spec(list(
  datasets = data.frame(dataset = "ADAE", path = "adae.rds"),
  analyses = data.frame(output_id = "T-2", analysis_id = "AE",
                        method = "hierarchical", dataset = "ADAE", by = "TRTA",
                        variables = "AEBODSYS | AEDECOD",
                        args = "over_variables = TRUE")))
tfl_normalize_code(sp, ard, "T-2")
#> [1] "data <- normalize_ard(ard, hierarchy = c(\"AEBODSYS\", \"AEDECOD\"), overall = \"Subjects with at least one TEAE\")"
```
