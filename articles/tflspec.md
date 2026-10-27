# Get started with tflspec

tflspec reads the specifications of a study’s tables, listings and
figures (Excel workbooks, YAML for figures) and writes the **R program**
each one stands for. The program is the deliverable: short, readable,
written the way a person would write it, and needing only cards / cardx
and [rtfreporter](https://github.com/ichirio/rtfreporter) to run – not
tflspec. People copy it into the study, read it, and finish it.

``` text
ADaM ──(ARD spec)──> ARD ──(table spec)──> plan ──(report spec)──> RTF
          tfl_ard_code()      tfl_table_code()       tfl_report_code()
          tfl_build_ard()     tfl_table_plan()       tfl_report()
```

Each spec can be **written out as code** (the `*_code()` functions) or
**run**
([`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md),
[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md),
[`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)).
Both come from the same list of steps, so the object and the program
cannot disagree.

This article walks the whole flow for one table, a demographics table
(`DM`), using the workbooks that come with the package and its oncology
sample study’s ADaM data (`adsl`, `adae` …; see
[`?adsl`](https://ichirio.github.io/tflspec/reference/adsl.md)).

``` r

library(tflspec)
library(rtfreporter)
```

## A study folder

Every path in a spec is relative to the study folder, and the generated
programs run from it. Ours holds two ADaM datasets:

``` r

study <- file.path(tempdir(), "study")
dir.create(file.path(study, "data"), recursive = TRUE, showWarnings = FALSE)
saveRDS(adsl, file.path(study, "data", "adsl.rds"))
saveRDS(adae, file.path(study, "data", "adae.rds"))
```

## 1. The ARD spec: what is analysed

The ARD spec is a workbook of a few sheets: `study` (keys such as the
subject key), `datasets` (what is read, and columns derived on reading),
`populations` (the analysis sets), `analysis_data` (named data the
analyses read, each a report’s own; optional) and `analyses`, **one
cards / cardx call per row**. A study normally keeps it as an `.xlsx`;
here it is written from R so the whole article runs on its own.

``` r

df <- function(...) data.frame(..., stringsAsFactors = FALSE)

ard_spec <- tfl_ard_spec(list(
  study = df(key = c("id", "output"),
             value = c("USUBJID", "output/ard/ard.rds")),
  datasets = df(dataset = c("ADSL", "ADAE"),
                path = c("data/adsl.rds", "data/adae.rds"),
                derive = c('BSLDGR1 = ifelse(BSLD < 60, "<60 mm", ">=60 mm")', NA)),
  populations = df(population_id = "SAF", dataset = "ADSL",
                   where = 'SAFFL == "Y"'),
  analyses = df(output_id = "DM",
                analysis_id = c("BIGN", "ARMN", "CONT", "CAT"),
                method = c("total_n", "categorical", "continuous",
                           "categorical"),
                population_id = "SAF",
                by = c(NA, NA, "TRT01A", "TRT01A"),
                variables = c(NA, "TRT01A", "AGE | BSLD",
                              "AGEGR1 | SEX | RACE | ECOGBL"))))

path <- tfl_write_ard_spec(ard_spec, file.path(study, "ard_spec.xlsx"))
ard_spec <- tfl_read_ard_spec(path)
ard_spec$analyses[, c("output_id", "analysis_id", "method", "by", "variables")]
#>   output_id analysis_id      method     by                    variables
#> 1        DM        BIGN     total_n   <NA>                         <NA>
#> 2        DM        ARMN categorical   <NA>                       TRT01A
#> 3        DM        CONT  continuous TRT01A                   AGE | BSLD
#> 4        DM         CAT categorical TRT01A AGEGR1 | SEX | RACE | ECOGBL
```

`ARMN` counts the subjects of each arm: the column header’s `(N=xx)` is
read from it. `method` is a keyword
([`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md)),
any `pkg::function` of cards or cardx, or a function of the study’s own.
The article [The ARD
spec](https://ichirio.github.io/tflspec/articles/ard-spec.md) goes
through every sheet.

### The ARD program

[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)
writes the program. It reads the data, makes the analysis set, and calls
cards once per analysis:

``` r

cat(tfl_ard_code(ard_spec, part = "body"), sep = "\n")
report_id <- "DM"

# ---- data ----
adsl <- readRDS("data/adsl.rds") |> mutate(BSLDGR1 = ifelse(BSLD < 60, "<60 mm", ">=60 mm"))

# ---- populations ----
# adsl_sub: SAF (SAFFL == "Y")
adsl_sub <- filter(adsl, SAFFL == "Y")

# ---- analyses ----
# BIGN
ard_bign <- adsl_sub |>
  ard_total_n() |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "BIGN", population = "SAF")

# ARMN
ard_armn <- adsl_sub |>
  ard_tabulate(variables = TRT01A, fmt_fun = everything() ~ fmt_default) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "ARMN", population = "SAF")

# CONT
ard_cont <- adsl_sub |>
  ard_summary(
    by = TRT01A,
    variables = c(AGE, BSLD),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CONT", population = "SAF")

# CAT
ard_cat <- adsl_sub |>
  ard_tabulate(
    by = TRT01A,
    variables = c(AGEGR1, SEX, RACE, ECOGBL),
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "CAT", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_rows(ard_bign, ard_armn, ard_cont, ard_cat) |>
  save_ard(report_id, definition = "8e9ceecd4874efbfa7321bfdc46e2809")
```

`part = "body"` leaves out the setup every program starts with (the
packages, the helpers that tag each result with its `output_id` /
`analysis_id`); `tfl_ard_code(ard_spec)` writes the whole file, ready to
save as `programs/ard.R`.

### The ARD

[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
runs that same program from the study folder and saves the study ARD
where the `study` key `output` says:

``` r

ard <- tfl_build_ard(ard_spec, dir = study)
ard[1:6, c("output_id", "analysis_id", "variable", "stat_name", "stat")]
#> # An ARD data frame: 6 × 5
#>   output_id analysis_id variable        stat_name    stat
#>   <chr>     <chr>       <chr>           <chr>      <list>
#> 1 DM        BIGN        ..ard_total_n.. N         158    
#> 2 DM        ARMN        TRT01A          n          80    
#> 3 DM        ARMN        TRT01A          N         158    
#> 4 DM        ARMN        TRT01A          p           0.506
#> 5 DM        ARMN        TRT01A          n          78    
#> 6 DM        ARMN        TRT01A          N         158
```

## 2. The table spec: how the ARD becomes a table

The table spec says which keys go across (`cols`) and down (`rows`), the
labels and order of each variable, the cell templates and digits, the
pages, the column header. The package’s example `DM.xlsx` defines this
table:

``` r

ex <- system.file("extdata", "ard-spec", package = "tflspec")
spec <- tfl_read_report_spec(file.path(ex, c("report.xlsx", "DM.xlsx")),
                             output_id = "DM")
spec$cells[, c("variable", "row", "template", "digits")]
#>      variable       row           template digits
#> 1  continuous         n                {N}      0
#> 2  continuous Mean (SD)      {mean} ({sd})    1,2
#> 3  continuous    Median           {median}      1
#> 4  continuous  Min, Max       {min}, {max}      0
#> 5 categorical      <NA> {n:.0f} ({p:.1f%})   <NA>
```

[`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md)
reads the table sheets and the report sheets together (one object serves
both);
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)
reads the table sheets alone.

### The table program

[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md)
writes the rtfreporter plan: the roles in
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html),
then one `plan_*()` verb for each thing the sheets say.

``` r

cat(tfl_table_code(spec, pipe = "|>"), sep = "\n")
# the code lists: each value in its order, as the table shows it
cl_agegr1 <- c("<65", ">=65")
cl_sex <- c("F", "M")
cl_ecogbl <- c("0", "1")

plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    continuous = list(
      n = "{N:.0f}",
      "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
      Median = "{median:.1f}",
      "Min, Max" = "{min:.0f}, {max:.0f}"
    ),
    categorical = "{n:.0f} ({p:.1f%})"
  ) |>
  plan_levels(
    variable = c("AGE", "AGEGR1", "SEX", "RACE", "ECOGBL", "BSLD"),
    AGEGR1 = cl_agegr1,
    SEX = cl_sex,
    ECOGBL = cl_ecogbl
  ) |>
  plan_labels(
    variable = c(
      AGE = "Age (years)",
      AGEGR1 = "Age group, n (%)",
      SEX = "Sex, n (%)",
      RACE = "Race, n (%)",
      ECOGBL = "Baseline ECOG performance status, n (%)",
      BSLD = "Baseline sum of diameters (mm)"
    )
  ) |>
  plan_digits(rounding = "sas") |>
  plan_col_header(
    data.frame(
      line = c("1", "1", "2", "2"),
      cols = c("row_label", ".values", "row_label", ".values"),
      span = c(NA, "each", NA, "each"),
      text = c(NA, "{col}", "Parameter", "(N={n})")
    )
  ) |>
  plan_stub(name = "row_label", before = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = FALSE) |>
  plan_paginate_rows(max_rows = 30, split = "group_safe") |>
  plan_columns(widths = c(row_label = 4, .values = 2)) |>
  plan_style(align_count_pct = TRUE)
```

### The table

[`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
builds that same plan from an ARD. Being a plan, a verb piped after it
still wins – one report’s own change, in one line, without touching the
workbook:

``` r

data <- normalize_ard(tfl_ard_for(ard, "DM"))
plan <- tfl_table_plan(data, spec) |>
  plan_cells(notes = FALSE)
```

The table it makes (`plan_apply(stage = "table")` stops before the
pages):

``` r

knitr::kable(plan_apply(plan, stage = "table"))
```

| group | label | Drug X | Control |
|:---|:---|:---|:---|
| Age (years) | n | 80 | 78 |
| Age (years) | Mean (SD) | 60.1 (9.59) | 62.7 (9.76) |
| Age (years) | Median | 59.5 | 62.5 |
| Age (years) | Min, Max | 39, 82 | 42, 85 |
| Age group, n (%) | \<65 | 57 (71.3) | 44 (56.4) |
| Age group, n (%) | \>=65 | 23 (28.8) | 34 (43.6) |
| Sex, n (%) | F | 41 (51.3) | 30 (38.5) |
| Sex, n (%) | M | 39 (48.8) | 48 (61.5) |
| Race, n (%) | ASIAN | 26 (32.5) | 23 (29.5) |
| Race, n (%) | BLACK OR AFRICAN AMERICAN | 9 (11.3) | 6 (7.7) |
| Race, n (%) | OTHER | 4 (5.0) | 0 (0.0) |
| Race, n (%) | WHITE | 41 (51.3) | 49 (62.8) |
| Baseline ECOG performance status, n (%) | 0 | 39 (48.8) | 37 (47.4) |
| Baseline ECOG performance status, n (%) | 1 | 41 (51.3) | 41 (52.6) |
| Baseline sum of diameters (mm) | n | 80 | 78 |
| Baseline sum of diameters (mm) | Mean (SD) | 69.2 (33.57) | 61.6 (30.42) |
| Baseline sum of diameters (mm) | Median | 62.0 | 50.0 |
| Baseline sum of diameters (mm) | Min, Max | 25, 139 | 25, 136 |

## 3. The report spec: the document around the table

The report spec dresses the page: paper and margins, the running header
and footer, titles and footnotes, tokens such as `{PAGE}` and
`{PROGRAM}`. The study’s header is written once, as default rows (a
blank `output_id`), and each report adds its own lines:

``` r

spec$header[, c("line", "left", "center", "right")]
#>   line             left                                    center
#> 1    1   Example Pharma                                      <NA>
#> 2    2 Study XYZ-ONC-01                                      <NA>
#> 3    3             <NA>                                      <NA>
#> 4    4             <NA>                              Table 14.1.2
#> 5    5             <NA> Demographics and Baseline Characteristics
#> 6    6             <NA>                         Safety Population
#>                          right
#> 1        Clinical Study Report
#> 2 Page {PAGE} of {TOTAL_PAGES}
#> 3                         <NA>
#> 4                         <NA>
#> 5                         <NA>
#> 6                         <NA>
```

### The report program

``` r

cat(tfl_report_code(spec, content = "plan"), sep = "\n")
doc <- rtf_document(program = "C:\\study\\tfl\\DM", file = "output/DM.rtf") |>
  rtf_section(
    header = rtf_header(
      list(
        c(l = "Example Pharma", r = "Clinical Study Report"),
        c(l = "Study XYZ-ONC-01", r = "Page {PAGE} of {TOTAL_PAGES}"),
        c(c = ""),
        c(c = "Table 14.1.2"),
        c(c = "Demographics and Baseline Characteristics"),
        c(c = "Safety Population")
      )
    ),
    footer = rtf_footer(
      list(
        c(l = "SD: standard deviation; Min: minimum; Max: maximum; ECOG: Eastern Cooperative Oncology Group."),
        c(l = "{PROGRAM}       Generated on: {DATETIME}")
      )
    )
  ) |>
  rtf_tables(plan)
```

### The RTF

``` r

doc <- tfl_report(spec, content = plan)
rtf <- file.path(study, tfl_report_path(spec))
dir.create(dirname(rtf), showWarnings = FALSE)
generate_rtfreport(doc, rtf, overwrite = TRUE)
file.exists(rtf)
#> [1] TRUE
```

## The program a study keeps

Put together, the three pieces are one short program per output that a
person can read top to bottom:

``` r
library(rtfreporter)
# 1 DATA    the ARD (programs/ard.R, written by tfl_ard_code())
ard <- readRDS("output/ard/ard.rds")
ard <- ard[ard$output_id == "DM", ]
# 2 TABLE   tfl_table_code(): the ARD normalized, then the plan
plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01A", rows = c(group = "variable")) |>
  plan_labels(AGE = "Age (years)", ...) |>
  ...
# 3 REPORT  tfl_report_code()
doc <- rtf_document(program = "DM")
doc <- rtf_tables(doc, plan)
generate_rtfreport(doc, "output/DM.rtf", overwrite = TRUE)
```

## When the spec cannot say it

A spec says the common things in columns. What it cannot express is
written as R **in the spec**, in its code columns, so the program is
still written from the spec and never edited by hand afterwards:

- `analysis_data$code`: R that makes an analysis data;
- `analyses$code` with method `custom`: R that makes an analysis’s ARD
  ([`tfl_ard_as_custom()`](https://ichirio.github.io/tflspec/reference/tfl_ard_as_custom.md)
  writes an existing row as such code, to start from);
- `args`, `derive`, `where`: R expressions where a value is code;
- a figure design’s `code` steps and `layer_code` layers, and `!r`
  values.

And what is truly one report’s own – a cell style computed row by row, a
step after the pages are made – is one more verb piped after the plan.

## Where next

- [The ARD
  spec](https://ichirio.github.io/tflspec/articles/ard-spec.md):
  datasets, populations, a report’s analysis data (`subjects`, `keep`,
  `code`), its code lists, the analyses.
- [The table
  spec](https://ichirio.github.io/tflspec/articles/table-spec.md): the
  table sheets, numbers or the ARD’s text, the decimals written once, a
  report’s code lists and the variables’ labels, and a plan written in
  code back to a workbook.
- [Reports](https://ichirio.github.io/tflspec/articles/reports.md):
  headers, footers, titles, footnotes and tokens written once for the
  study, each report’s own tokens, the company’s font and the setup code
  the report programs share.
- [Listings](https://ichirio.github.io/tflspec/articles/listings.md),
  [Figures](https://ichirio.github.io/tflspec/articles/figures.md) and
  [CDISC ARS](https://ichirio.github.io/tflspec/articles/ars.md).
- Working with an AI assistant? Attach
  [`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md),
  the manual of the installed version.
