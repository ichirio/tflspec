# The code of a table's plan, from its definition

Writes the
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
pipeline a table definition stands for: the roles of its `tables` sheet
in `table_plan()`, and one `plan_*()` verb for each thing the other
sheets say – the same verbs, with the same values, that
`tfl_table_plan(data, spec)` applies. The program then no longer reads
the workbook, and what the workbook cannot say (a cell style, a step
after the pages are made) is written under it by hand.
[`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md)
goes the other way.

## Usage

``` r
tfl_table_code(
  spec,
  output_id = NULL,
  data = "data",
  plan = "plan",
  pipe = NULL
)
```

## Arguments

- spec:

  A table definition
  ([`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md),
  [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)),
  or the path(s) of its workbook(s).

- output_id:

  The table, when the definition has several.

- data:

  The name of the normalized ARD in the program.

- plan:

  The name the plan is assigned to.

- pipe:

  `"|>"` or `"%>%"`; `NULL` follows `getOption("rtfreporter.ard_pipe")`
  (see
  [`rtfreporter::plan_template()`](https://ichirio.github.io/rtfreporter/reference/plan_template.html)).

## Value

The code, one element per line.

## See also

[`tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.md)
for the report around it.

## Examples

``` r
spec <- tfl_read_table_spec(
  system.file("extdata", "ard-spec", "DM.xlsx", package = "tflspec"))
cat(tfl_table_code(spec), sep = "\n")
#> plan <- table_plan(data, cols = "TRT01A", rows = c(group = "variable")) %>%
#>   plan_digits(rounding = "sas") %>%
#>   plan_levels(AGEGR1 = c("<65", "65-80", ">80"), SEX = c("F", "M")) %>%
#>   plan_labels(
#>     AGE = "Age (years)",
#>     AGEGR1 = "Age group, n (%)",
#>     SEX = "Sex, n (%)",
#>     RACE = "Race, n (%)",
#>     WEIGHTBL = "Weight at baseline (kg)"
#>   ) %>%
#>   plan_cells(
#>     continuous = list(
#>       n = "{N:.0f}",
#>       "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
#>       Median = "{median:.1f}",
#>       "Min, Max" = "{min:.0f}, {max:.0f}"
#>     ),
#>     categorical = "{n:.0f} ({p:.1f%})"
#>   ) %>%
#>   plan_stub(name = "row_label", before = TRUE) %>%
#>   plan_blanks(where = "between_groups", first = TRUE, last = FALSE) %>%
#>   plan_paginate_rows(max_rows = 30L, split = "group_safe") %>%
#>   plan_style(align_count_pct = TRUE) %>%
#>   plan_col_header(
#>     header = data.frame(
#>       line = c("1", "1", "2", "2"),
#>       cols = c("row_label", ".values", "row_label", ".values"),
#>       span = c(NA, "each", NA, "each"),
#>       text = c(NA, "{col}", "Parameter", "(N={n})"),
#>       check.names = FALSE
#>     )
#>   ) %>%
#>   plan_columns(widths = c(row_label = 4, .values = 2))
```
