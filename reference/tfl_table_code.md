# The code of a table's plan, from its definition

Writes the
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
pipeline a table definition stands for, one pipe from the ARD:
[`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)
(the hierarchy, the overall row and the stack the definitions say), the
roles of its `tables` sheet in `table_plan()`, and one `plan_*()` verb
for each thing the other sheets say – the same verbs, with the same
values, that `tfl_table_plan(data, spec)` applies. The program then no
longer reads the workbook, and what the workbook cannot say (a cell
style, a step after the pages are made) is written under it by hand.
[`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md)
goes the other way.

## Usage

``` r
tfl_table_code(
  spec,
  output_id = NULL,
  data = "ard",
  plan = "plan",
  pipe = NULL,
  ard_spec = NULL,
  normalize = TRUE
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

  The name of the ARD in the program: the pipe starts from it
  (`plan <- ard |> normalize_ard(...) |> table_plan(...)`). With
  `normalize = FALSE`, the name of the data already normalized.

- plan:

  The name the plan is assigned to.

- pipe:

  `"|>"` or `"%>%"`; `NULL` follows `getOption("rtfreporter.ard_pipe")`
  (see
  [`rtfreporter::plan_template()`](https://ichirio.github.io/rtfreporter/reference/plan_template.html)).

- ard_spec:

  The ARD definition
  ([`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)),
  for
  [`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)'s
  `hierarchy`; `NULL` for none.

- normalize:

  `TRUE` to write
  [`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)
  at the head of the pipe (its `hierarchy`, `overall` and `stack` from
  the definitions); `FALSE` when the program normalizes the ARD itself
  ([`tfl_normalize_code()`](https://ichirio.github.io/tflspec/reference/tfl_normalize_code.md)).

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
#> # the code lists: each value in its order, as the table shows it
#> cl_agegr1 <- c("<65", ">=65")
#> cl_sex <- c("F", "M")
#> cl_ecogbl <- c("0", "1")
#> 
#> plan <- ard %>%
#>   normalize_ard() %>%
#>   table_plan(cols = "TRT01A", rows = c(group = "variable")) %>%
#>   plan_cells(
#>     continuous = list(
#>       n = "{N:.0f}",
#>       "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
#>       Median = "{median:.1f}",
#>       "Min, Max" = "{min:.0f}, {max:.0f}"
#>     ),
#>     categorical = "{n:.0f} ({p:.1f%})"
#>   ) %>%
#>   plan_labels(
#>     AGE = "Age (years)",
#>     AGEGR1 = "Age group, n (%)",
#>     SEX = "Sex, n (%)",
#>     RACE = "Race, n (%)",
#>     ECOGBL = "Baseline ECOG performance status, n (%)",
#>     BSLD = "Baseline sum of diameters (mm)"
#>   ) %>%
#>   plan_levels(
#>     variable = c("AGE", "AGEGR1", "SEX", "RACE", "ECOGBL", "BSLD"),
#>     AGEGR1 = cl_agegr1,
#>     SEX = cl_sex,
#>     ECOGBL = cl_ecogbl
#>   ) %>%
#>   plan_digits(rounding = "sas") %>%
#>   plan_col_header(
#>     data.frame(
#>       line = c("1", "1", "2", "2"),
#>       cols = c("row_label", ".values", "row_label", ".values"),
#>       span = c(NA, "each", NA, "each"),
#>       text = c(NA, "{col}", "Parameter", "(N={n})")
#>     )
#>   ) %>%
#>   plan_stub(name = "row_label", before = TRUE) %>%
#>   plan_blanks(where = "between_groups", first = TRUE, last = FALSE) %>%
#>   plan_paginate_rows(max_rows = 30, split = "group_safe") %>%
#>   plan_columns(widths = c(row_label = 4, .values = 2)) %>%
#>   plan_style(align_count_pct = TRUE)
```
