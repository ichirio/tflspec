# The table spec

The table spec says how an ARD becomes a table: which keys go across and
down, the label and order of each variable, the template and digits of
each cell, the pages, the column widths, the header. It stands for an
[rtfreporter](https://github.com/ichirio/rtfreporter) **plan**:
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
with the roles, and one `plan_*()` verb for each thing the other sheets
say.

- `tfl_table_code(spec)` writes that plan as code – the program a study
  keeps, which no longer reads the workbook;
- `tfl_table_plan(data, spec)` builds the same plan from an ARD.

``` r

library(tflspec)
library(rtfreporter)
```

## The sheets

One sheet per grain, so each fact is written once. Column names are
rtfreporter’s argument names: a `layout` column is the verb’s prefix and
its argument (`pages_max_rows` is `plan_paginate_rows(max_rows = )`).

| Sheet | One row per | Goes to |
|----|----|----|
| `study` | study fact | `rounding`, `output_path`, `program_dir` |
| `tables` | table | `table_plan(cols, rows, label)`, [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `variables` | variable | [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `codelists` | value of a variable, of a report | the ARD’s factors (its values the labels), then [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `cells` | line of a cell | [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `digits` | statistic | the decimals of `{mean}`, `{sd}`, `{p}` … in every template that says none |
| `layout` | table | [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `columns` | printed column | [`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html), [`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `style` | table | [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `cell_styles` | styling | [`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |
| `col_header` | header cell | [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html) |

The **`output_id` rule**: a row with a blank `output_id` is the study’s
default; a report’s own row replaces it, per sheet (and, on the keyed
sheets, per key). The exception is `codelists`, which has no defaults:
every row names its report. Every column is described on its header
cell’s comment in the workbook and by
[`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md).

## The example workbooks

The package comes with the specs of five tables – demographics (`DM`),
adverse events by SOC and PT (`AE`), response by subgroup (`ORR`), a lab
shift table (`LB`) and PK concentrations (`PK`) – each in its own
workbook, and all five in `study.xlsx`:

``` r

ex <- system.file("extdata", "ard-spec", package = "tflspec")
spec <- tfl_read_table_spec(file.path(ex, "study.xlsx"))
spec$tables[, c("output_id", "cols", "rows", "label", "sort")]
#>   output_id              cols
#> 1        DM            TRT01A
#> 2        AE      TRT01A | SEX
#> 3       ORR TRT01A | variable
#> 4        LB            BGRADE
#> 5        PK           NOMTIME
#>                                                    rows                  label
#> 1                                      group = variable                   <NA>
#> 2                                     group1 = AEBODSYS        label = AEDECOD
#> 3             family = group2 | subgroup = group2_level                     NA
#> 4 PARAM = PARAM | group1 = "Worst grade after baseline"         label = .label
#> 5                                     Analyte = ANALYTE Statistic = stat_label
#>                                      sort
#> 1                                    <NA>
#> 2 .overall | group1 | .depth | -n | label
#> 3                                   FALSE
#> 4                                    <NA>
#> 5                                    <NA>
```

The AE table: treatment groups across, SOC as row groups, PT as rows,
sorted by frequency.

``` r

cat(tfl_table_code(spec, "AE", pipe = "|>"), sep = "\n")
# the code lists: each value in its order, as the table shows it
cl_trt01a <- c("Drug X", "Control")
cl_sex <- c("F", "M")

plan <- ard |>
  normalize_ard(hierarchy = c("AEBODSYS", "AEDECOD")) |>
  table_plan(
    cols = c("TRT01A", "SEX"),
    rows = c(group1 = "AEBODSYS"),
    label = c(label = "AEDECOD")
  ) |>
  plan_cells(default = "{n:.0f} ({p:.1f%})") |>
  plan_levels(TRT01A = cl_trt01a, SEX = cl_sex) |>
  plan_digits(rounding = "sas") |>
  plan_sort(c(".overall", "group1", ".depth", "-n", "label")) |>
  plan_col_header(
    data.frame(
      line = c("1", "1", "2", "2"),
      cols = c("row_label", ".values", "row_label", ".values"),
      span = c(NA, "TRT01A", NA, "each"),
      text = c(
        NA_character_,
        "{col1} (N={n:sum})",
        "System organ class\n  Preferred term",
        "{col2}\n(N={n})\nn (%)"
      ),
      border_bottom = c(NA, "single", NA, NA)
    )
  ) |>
  plan_stub(name = "row_label", before = TRUE) |>
  plan_blanks(
    where = "between_groups",
    first = TRUE,
    last = TRUE,
    counted = TRUE
  ) |>
  plan_paginate_rows(max_rows = 28, split = "group_force") |>
  plan_columns(widths = c(row_label = 36, .values = 8)) |>
  plan_style(align_count_pct = TRUE, row_height_twips = 220)
```

## Cells

`cells` rows are templates: `{mean} ({sd})`, `{n} ({p:.1f%})`. A row
with a blank `variable` is the table’s default template. Rows with the
same variable / context / row form one chain tried in order, and `when`
is an R guard (`n == 0`). In a table of one statistic per row
(`tables$stats = rows`), a row with no template gives one statistic’s
digits.

``` r

spec$cells[spec$cells$output_id %in% "ORR",
           c("variable", "row", "when", "template")]
#>    variable row          when                            template
#> 6         n   1          <NA>                             {N:.0f}
#> 7      resp   1        n == 0                                   0
#> 8      resp   1 estimate == 1                       {n:.0f} (100)
#> 9      resp   1          <NA>           {n:.0f} ({estimate:.1f%})
#> 10     resp   2          <NA> [{conf.low:.1f%}, {conf.high:.1f%}]
```

## Numbers or the ARD’s text: `tables$value`

`tables$value` says which of the ARD’s values a table prints:

- `stat` (the default): the numbers, rounded here – by the `digits`
  sheet below, or a template’s own format (`{mean:.1f}`);
- `stat_fmt`: the ARD’s own text, as the ARD program formatted it
  (`analyses$formats`); nothing is rounded here, and a template still
  puts two in one cell (`{mean} ({sd})`).

A template may say otherwise for one value: `{mean:stat_fmt}`,
`{mean:.2f}`.

## A Total column

A column for all subjects comes from cards’ own overall rows, not from a
group the data does not have. On the `analyses` sheet, `overall = TRUE`
runs the analysis again without its `by` and binds it under the grouped
one; on the `tables` sheet, `total` is the column’s heading (`Total`)
and `total_position` puts it `last` (the default) or `first`. The table
is written with rtfreporter’s
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html),
whose `(N={n})` is the study total. In the ARS the analysis is written
twice, by its groups and over all subjects.

## Decimals, written once: the `digits` sheet

The `digits` sheet gives each statistic’s decimals once, for every
analysis variable (`variable` blank), and a variable’s exceptions (a
variable named). They are written into the templates’ tokens that say no
format: `{mean} ({sd})` becomes `{mean:.1f} ({sd:.2f})`. A template’s
own format, or a `cells` row’s `digits`, wins.

## A report’s code lists, and the variables’ labels

A report’s `codelists` rows say what each value is in the data its
program makes: the ARD program puts the labels on the data (see [The ARD
spec](https://ichirio.github.io/tflspec/articles/ard-spec.md)), so the
ARD already holds “Female”, and the table only orders the ARD’s values
([`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html),
in the labels). A variable’s heading is the **`variables`** sheet’s
`label`
([`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)).
The code list of `variable` (an earlier form of the headings) is moved
to the `variables` sheet’s `label` when the table spec is read, with a
message. Labels are a report’s, like the rest of its code lists.

``` r

adsl <- adsl[adsl$SAFFL == "Y", ]    # the sample study's ADSL, safety set
adsl$SEX <- factor(adsl$SEX, c("F", "M"), c("Female", "Male"))  # as set_levels()
ard2 <- cards::ard_stack(
  adsl, .by = TRT01A,
  cards::ard_summary(variables = AGE),
  cards::ard_tabulate(variables = c(SEX, AGEGR1)))
df <- function(...) data.frame(..., stringsAsFactors = FALSE)
sp <- tfl_table_spec(list(
  tables = df(output_id = "T-DM", cols = "TRT01A", rows = "group = variable"),
  variables = df(output_id = "T-DM", variable = c("AGE", "SEX", "AGEGR1"),
                 order = 1:3, label = c("Age (years)", "Sex", "Age group")),
  codelists = df(output_id = "T-DM", variable = "SEX", value = c("F", "M"),
                 label = c("Female", "Male"), order = 1:2),
  cells = df(output_id = "T-DM", variable = c("continuous", "categorical"),
             row = c("Mean (SD)", NA), template = c("{mean} ({sd})", "{n} ({p})")),
  digits = df(output_id = "T-DM", statistic = c("mean", "sd", "p"),
              digits = c(1, 2, 1))))
```

``` r

cat(tfl_table_code(sp, "T-DM", pipe = "|>"), sep = "\n")
# the code lists: each value in its order, as the table shows it
cl_sex <- c("Female", "Male")

plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    continuous = list("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
    categorical = "{n} ({p:.1f%})"
  ) |>
  plan_labels(AGE = "Age (years)", SEX = "Sex", AGEGR1 = "Age group") |>
  plan_levels(variable = c("AGE", "SEX", "AGEGR1"), SEX = cl_sex)
```

``` r

knitr::kable(plan_apply(tfl_table_plan(normalize_ard(ard2), sp, output_id = "T-DM") |>
                          plan_cells(notes = FALSE), stage = "table"))
```

| group       | label     | Drug X      | Control     |
|:------------|:----------|:------------|:------------|
| Age (years) | Mean (SD) | 60.1 (9.59) | 62.7 (9.76) |
| Sex         | Female    | 41 (51.2)   | 30 (38.5)   |
| Sex         | Male      | 39 (48.8)   | 48 (61.5)   |
| Age group   | \<65      | 57 (71.2)   | 44 (56.4)   |
| Age group   | \>=65     | 23 (28.7)   | 34 (43.6)   |

## From an ARD: a template to start from

[`tfl_table_spec_template()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec_template.md)
walks an ARD and writes the `tables`, `variables` and `cells` sheets –
the variables with their levels, a template for each kind of row – to be
edited:

``` r

ard <- cards::ard_stack(
  adsl, .by = TRT01A,
  cards::ard_summary(variables = AGE),
  cards::ard_tabulate(variables = AGEGR1))
tmpl <- tfl_table_spec_template(ard, cols = "TRT01A", output_id = "T-DM")
tmpl$variables
#>   output_id variable  label order     levels empty_levels under
#> 1      T-DM      AGE    AGE     1       <NA>         <NA>  <NA>
#> 2      T-DM   AGEGR1 AGEGR1     2 <65 | >=65         <NA>  <NA>
tmpl$cells[, c("variable", "row", "template")]
#>   variable       row      template
#> 1      AGE         n           {N}
#> 2      AGE Mean (SD) {mean} ({sd})
#> 3      AGE    Median      {median}
#> 4      AGE  Min, Max  {min}, {max}
#> 5   AGEGR1      <NA>     {n} ({p})
```

Write it with `tfl_write_table_spec(tmpl, "spec/tables.xlsx")`, edit it
in Excel, read it back with
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md).

## A plan, and one report’s own change

[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
returns a plan, so any rtfreporter verb piped after it wins over the
workbook (“last wins”) – one report departs from the study’s workbook in
one line, and the workbook is not touched:

``` r

plan <- tfl_table_plan(normalize_ard(ard), tmpl) |>
  plan_cells(notes = FALSE) |>
  plan_labels(AGE = "Age (years)", AGEGR1 = "Age group, n (%)")
knitr::kable(plan_apply(plan, stage = "table"))
```

| group            | label     | Drug X     | Control    |
|:-----------------|:----------|:-----------|:-----------|
| Age (years)      | n         | 80         | 78         |
| Age (years)      | Mean (SD) | 60.1 (9.6) | 62.7 (9.8) |
| Age (years)      | Median    | 59.5       | 62.5       |
| Age (years)      | Min, Max  | 39.0, 82.0 | 42.0, 85.0 |
| Age group, n (%) | \<65      | 57 (71.3)  | 44 (56.4)  |
| Age group, n (%) | \>=65     | 23 (28.8)  | 34 (43.6)  |

## A plan written in code, back to a workbook

[`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md)
goes the other way: a plan written in code becomes a workbook. It
compares the RTF the two make (byte by byte) and lists what a sheet
cannot carry (`attr(, "not_converted")`) – a look computed row by row, a
step after the pages are made – which stays in code, after the plan:

``` r

coded <- table_plan(normalize_ard(ard), cols = "TRT01A",
                    rows = c(group = "variable")) |>
  plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n} ({p:.1f%})", notes = FALSE) |>
  plan_paginate_rows(max_rows = 30)
back <- tfl_as_table_spec(coded, output_id = "T-DM")
back$cells[, c("variable", "row", "template")]
#>      variable       row       template
#> 1  continuous Mean (SD)  {mean} ({sd})
#> 2 categorical      <NA> {n} ({p:.1f%})
#> 3         AGE Mean (SD)  {mean} ({sd})
#> 4      AGEGR1      <NA> {n} ({p:.1f%})
back$layout[, "pages_max_rows", drop = FALSE]
#>   pages_max_rows
#> 1             30
```

## Reports and several workbooks

A table spec is usually read together with its report spec (the page,
titles, footnotes):
`tfl_read_report_spec(c("report.xlsx", "tables.xlsx"), output_id = )`
reads both, and one object serves
[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
and
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
alike. A sheet may be in any of the workbooks given, but in only one.
See [Reports](https://ichirio.github.io/tflspec/articles/reports.md).
