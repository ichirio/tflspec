# The ARD spec

The ARD spec is the study’s analyses as a workbook.
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)
writes the R program it stands for – cards / cardx calls, one per
analysis row – and
[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
runs that program and saves one **study ARD** (analysis results data)
that every table then reads.

The program is meant to be read. A dataset is read in one line, an
analysis set is one [`subset()`](https://rdrr.io/r/base/subset.html), an
analysis data is one pipe, an analysis is one call. It is written the
way a programmer would write it, so that it can be reviewed, copied into
a study and finished by hand when needed.

``` r

library(tflspec)

study <- file.path(tempdir(), "ard-study")
dir.create(file.path(study, "data"), recursive = TRUE, showWarnings = FALSE)
saveRDS(adsl, file.path(study, "data", "adsl.rds"))   # the sample study's ADaM
saveRDS(adae, file.path(study, "data", "adae.rds"))

df <- function(...) data.frame(..., stringsAsFactors = FALSE)
```

## The sheets

| Sheet | One row per | Holds |
|----|----|----|
| `study` | key | `id` (the subject key), `output` (where the ARD goes), `source` (R files of the study’s own functions) |
| `datasets` | dataset | `dataset`, `level`, `path` (relative to the study folder), `derive` (columns made on reading) |
| `populations` | analysis set | `population_id`, `dataset`, `where`, `derive` |
| `analysis_data` | named data of a report | `output_id`, `data_id`, `label`, `from`, `population_id`, `subjects`, `where`, `add`, `derive`, `keep`, `distinct`, `code` |
| `analyses` | analysis | `output_id`, `analysis_id`, `method`, `data` or `dataset` + `population_id`, `where`, `by`, `strata`, `variables`, `statistics`, `denominator`, `formats`, `args`, `code`, `parent`, `post`, … |

Every column is described on its header cell’s comment in the workbook,
and by
[`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md):

``` r

cols <- tfl_spec_columns("analysis_data")
knitr::kable(cols[!is.na(cols$column), c("column", "description")])
```

|  | column | description |
|:---|:---|:---|
| 2 | data_id | The data’s name, and its object’s name in the ARD program: a lower-case letter first, then letters, digits, \_ or . – not a dataset’s or another data’s (\_sub, \_sub2 … are the program’s own). |
| 3 | label | A name a person reads; blank: none. |
| 4 | from | What it is made from: a dataset, or an analysis data of a row above. |
| 5 | population_id | Keeps the subjects of this population; blank: no restriction here (made from an analysis data: that one’s population). |
| 6 | subjects | Keeps the subjects of an analysis data above (a report’s own subjects: the safety set in phase 1 …): the numerator kept to the denominator’s subjects. Instead of population_id; blank: no restriction here. |
| 7 | where | An R condition on the records (the numerator’s filter); blank: every record. |
| 8 | add | Columns of the population’s data (its dataset, ADSL) – or of the data of `subjects` – added by the subject key, \| between them; a column of the same name is replaced. Needs a population or subjects, here or above; blank: none. |
| 9 | derive | New columns, NAME = R expression, \| between them (transform()); a column the code lists list becomes a factor in their order; blank: none. |
| 10 | keep | The columns kept, \| between them (the subject key always); with derive, what transmute() does; blank: every column. |
| 11 | distinct | One row for each set of values of these columns, \| between them (dplyr::distinct(), the first row kept): a denominator per subject and phase; blank: every row. |
| 12 | code | R that makes the data itself, when the columns cannot say it: its value is the data; the datasets (adae), the analysis set’s data (\_sub: adsl_sub, adsl_sub2 for a report’s second) and the analysis data above are in reach. With it, the columns but from are left blank; blank: the columns make the data. |

A new workbook starts from
[`tfl_ard_spec_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_spec_template.md);
it is read with
[`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
and written with
[`tfl_write_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard_spec.md).
The examples below build the sheets in R with
[`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md),
which checks them exactly as the reader does.

## Datasets and populations

``` r

base <- list(
  study = df(key = c("id", "output"),
             value = c("USUBJID", "output/ard/ard.rds")),
  datasets = df(dataset = c("ADSL", "ADAE"),
                path = c("data/adsl.rds", "data/adae.rds")),
  populations = df(population_id = "SAF", dataset = "ADSL",
                   where = 'SAFFL == "Y"'))
```

A population is an analysis set: the subjects of `dataset` for which
`where` holds. In the program it is `pop_<population_id>`. An analysis
row may name a `dataset` and a `population_id`: its data are then that
dataset’s records of the analysis set’s subjects.

``` r

spec <- tfl_ard_spec(c(base, list(analyses = df(
  output_id = "T-14-1-1", analysis_id = "AGE", method = "continuous",
  population_id = "SAF", by = "TRT01A", variables = "AGE"))))
cat(tfl_ard_code(spec, part = "body"), sep = "\n")
report_id <- "T-14-1-1"

# ---- data ----
adsl <- readRDS("data/adsl.rds")

# ---- populations ----
# adsl_sub: SAF (SAFFL == "Y")
adsl_sub <- filter(adsl, SAFFL == "Y")

# ---- analyses ----
# AGE
ard_age <- adsl_sub |>
  ard_summary(
    by = TRT01A,
    variables = AGE,
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "AGE", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
ard_age |>
  save_ard(report_id, definition = "3a67c8569ba5a342d3c3f784a15c74a1")
```

## Analysis data

When several analyses read the same data – treatment-emergent events of
the safety set, say – the sheet `analysis_data` names it once.

**An analysis data is a report’s**: `output_id` says whose, and it is
required. A row’s `from` and `subjects` name only its own report’s rows
above it, and an analysis reads only its own report’s data. So the same
`data_id` may mean something else in another report – the safety set of
a Phase I table and of a Phase II one – and a data two reports use is
copied into each (tflplanner’s “copy from another report”).

Each row makes one data, in this order:

1.  `from` a dataset or an analysis data above it;
2.  kept to the subjects of a population (`population_id`) **or** of an
    analysis data above (`subjects`: the numerator kept to the
    denominator’s subjects);
3.  the records `where` keeps;
4.  columns of the population’s data added by the subject key (`add`);
5.  columns derived (`derive`), and the columns kept (`keep`; with
    `derive`, a `transmute()`);
6.  one row per set of values (`distinct`).

`data_id` is the object’s name in the program. An analysis names it in
its `data` column instead of `dataset` / `population_id`.

``` r

adata <- df(
  output_id = "T-14-3-1",
  data_id = c("adsl_saf", "adae_teae", "adae_ser"),
  label = c("Safety set", "TEAEs of the safety set", "Serious TEAEs"),
  from = c("ADSL", "ADAE", "adae_teae"),
  population_id = c("SAF", NA, NA),
  subjects = c(NA, "adsl_saf", NA),
  where = c(NA, 'TRTEMFL == "Y"', 'AESER == "Y"'),
  keep = c(NA, "AEBODSYS | AEDECOD | AESER | TRT01A", NA))

analyses <- df(
  output_id = "T-14-3-1",
  analysis_id = c("TEAE", "SER"),
  method = "hierarchical",
  data = c("adae_teae", "adae_ser"),
  by = "TRT01A",
  variables = c("AEBODSYS | AEDECOD", "AEDECOD"),
  denominator = "adsl_saf")

spec <- tfl_ard_spec(c(base, list(analysis_data = adata, analyses = analyses)))
```

``` r

cat(tfl_ard_code(spec, part = "body"), sep = "\n")
report_id <- "T-14-3-1"

# ---- data ----
adsl <- readRDS("data/adsl.rds")
adae <- readRDS("data/adae.rds")

# ---- analysis data ----
# adsl_saf: Safety set
adsl_saf <- filter(adsl, SAFFL == "Y")
# adae_teae: TEAEs of the safety set
adae_teae <- adae |>
  select(-any_of("TRT01A")) |>
  inner_join(select(adsl_saf, USUBJID, TRT01A), by = "USUBJID") |>
  filter(TRTEMFL == "Y") |>
  select(USUBJID, AEBODSYS, AEDECOD, AESER, TRT01A)
# adae_ser: Serious TEAEs
adae_ser <- filter(adae_teae, AESER == "Y")

# ---- analyses ----
# TEAE
ard_teae <- adae_teae |>
  ard_hierarchical_or_none(
    by = TRT01A,
    variables = c(AEBODSYS, AEDECOD),
    denominator = adsl_saf,
    id = USUBJID
  ) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "TEAE", population = "SAF")

# SER
ard_ser <- adae_ser |>
  ard_hierarchical_or_none(
    by = TRT01A,
    variables = AEDECOD,
    denominator = adsl_saf,
    id = USUBJID
  ) |>
  fmt_ard(fmt_default) |>
  tag_ard(report_id, "SER", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
bind_rows(ard_teae, ard_ser) |>
  save_ard(report_id, definition = "cebab636000d2d2289c3e1238e45aec9")
```

### What the columns cannot say: `code`

When the columns cannot say how a data is made, `code` is R whose value
is the data. The datasets, the `pop_*` analysis sets and the analysis
data above it are in reach. `from` stays (it says what the data is made
from), and the other columns that make a data are left blank (this is
checked):

``` r

adata2 <- adata[1:2, ]
adata2$output_id <- "T-14-3-2"     # its own copy of the two data
adata2 <- rbind(adata2, df(
  output_id = "T-14-3-2",
  data_id = "adae_first", label = "First TEAE of each subject and term",
  from = "adae_teae", population_id = NA, subjects = NA, where = NA,
  keep = NA))
adata2$code <- c(NA, NA, paste(
  "adae_teae <- adae_teae[order(adae_teae$USUBJID, adae_teae$AEDECOD), ]",
  "adae_teae[!duplicated(adae_teae[c(\"USUBJID\", \"AEDECOD\")]), ]",
  sep = "\n"))
spec2 <- tfl_ard_spec(c(base, list(
  analysis_data = adata2,
  analyses = df(output_id = "T-14-3-2", analysis_id = "FIRST",
                method = "categorical", data = "adae_first",
                by = "TRT01A", variables = "AEDECOD"))))
cat(tfl_ard_code(spec2, part = "body"), sep = "\n")
report_id <- "T-14-3-2"

# ---- data ----
adsl <- readRDS("data/adsl.rds")
adae <- readRDS("data/adae.rds")

# ---- analysis data ----
# adsl_saf: Safety set
adsl_saf <- filter(adsl, SAFFL == "Y")
# adae_teae: TEAEs of the safety set
adae_teae <- adae |>
  semi_join(select(adsl_saf, USUBJID), by = "USUBJID") |>
  filter(TRTEMFL == "Y") |>
  select(USUBJID, AEBODSYS, AEDECOD, AESER, TRT01A)
# adae_first: First TEAE of each subject and term
adae_first <- local({
  adae_teae <- adae_teae[order(adae_teae$USUBJID, adae_teae$AEDECOD), ]
  adae_teae[!duplicated(adae_teae[c("USUBJID", "AEDECOD")]), ]
})

# ---- analyses ----
# FIRST
ard_first <- adae_first |>
  ard_tabulate(
    by = TRT01A,
    variables = AEDECOD,
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "FIRST", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
ard_first |>
  save_ard(report_id, definition = "5a928109371010b18d526b388d9cdf56")
```

The program carries it as `<data_id> <- local({ ... })`, in the sheet’s
order. The spec keeps the code: the program is written from the spec and
is never edited afterwards.

## Analyses

One row is one cards / cardx call. Both ids become ARD columns, so a
table takes its rows with `tfl_ard_for(ard, output_id)`.

- **`method`**: a keyword –
  [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md)
  lists them, each with a label a person reads – or any `pkg::function`
  of cards / cardx, or a function of the study’s own (study key
  `source`).

``` r

knitr::kable(tfl_ard_methods()[, c("method", "label", "call")])
```

| method        | label                         | call                          |
|:--------------|:------------------------------|:------------------------------|
| continuous    | Summary statistics            | cards::ard_summary            |
| categorical   | Counts and percents           | cards::ard_tabulate           |
| dichotomous   | Count of one level            | cards::ard_tabulate_value     |
| missing       | Missing counts                | cards::ard_missing            |
| hierarchical  | Nested counts (e.g. SOC / PT) | cards::ard_stack_hierarchical |
| max           | Worst level per subject       | cardx::ard_tabulate_max       |
| subjects      | Subjects with a record        | (subjects)                    |
| total_n       | Number of subjects            | cards::ard_total_n            |
| proportion_ci | Proportion with CI            | cardx::ard_categorical_ci     |
| mean_ci       | Mean with CI                  | cardx::ard_continuous_ci      |
| ttest         | t test                        | cardx::ard_stats_t_test       |
| wilcox        | Wilcoxon rank-sum test        | cardx::ard_stats_wilcox_test  |
| chisq         | Chi-square test               | cardx::ard_stats_chisq_test   |
| fisher        | Fisher’s exact test           | cardx::ard_stats_fisher_test  |
| custom        | Custom R code                 | (code)                        |

- **`by`, `strata`, `variables`, `statistics`**: `|` between several
  values. `strata` repeats the analysis within each combination present
  in the data (a parameter by visit).
- **`denominator`**: what percentages are of – `population`, `row`,
  `column`, `cell`, another population, a dataset or an analysis data.
- **`formats`**: `mean=xx.x | p=xx.x%` (the `xx` part says the
  decimals).
- **`args`**: more arguments, as R (`over_variables = TRUE`). A function
  that takes a formula gets it here:
  `formula = survival::Surv(AVAL, 1 - CNSR) ~ TRT01P`.
- **`parent`** and **`post`**: rows run inside
  [`cards::ard_stack()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_stack.html)
  / `ard_strata()` / `ard_pairwise()`, and steps on the ARD after the
  call
  ([`cards::sort_ard_hierarchical()`](https://pharmaverse.github.io/cards/latest-tag/reference/sort_ard_hierarchical.html)).

[`tfl_ard_functions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_functions.md)
lists every installed cards / cardx `ard_*()` function by category, and
[`tfl_ard_args()`](https://ichirio.github.io/tflspec/reference/tfl_ard_args.md)
says in which column each argument of one goes:

``` r

a <- tfl_ard_args("cardx::ard_categorical_ci")
knitr::kable(a[, c("arg", "column", "choices")])
```

| arg | column | choices |
|:---|:---|:---|
| data | data |  |
| variables | variables |  |
| by | by |  |
| method | args | waldcc \| wald \| clopper-pearson \| wilson \| wilsoncc \| strat_wilson \| strat_wilsoncc \| agresti-coull \| jeffreys |
| denominator | denominator | column \| row \| cell |
| conf.level | args |  |
| value | args |  |
| strata | strata |  |
| weights | args |  |
| max.iterations | args |  |

### An analysis written as R: `custom`

What no column can say is written as R in `code`, with `method`
`custom`. In the code `data` is the analysis data and `population` the
analysis set’s subjects.
[`tfl_ard_as_custom()`](https://ichirio.github.io/tflspec/reference/tfl_ard_as_custom.md)
writes an existing row as that code (the same ARD), as a starting point:

``` r

cat(tfl_ard_as_custom(spec, "T-14-3-1", "SER")$code, sep = "\n")
cards::ard_stack_hierarchical(
  data,
  by = TRT01A,
  variables = AEDECOD,
  denominator = adsl_saf,
  id = USUBJID
)
```

## Code lists

A table spec’s `codelists` sheet (one row a value: `output_id`,
`variable`, `value`, `label`, `order`) gives the order of a variable’s
values and what each value is in the data the report’s program makes
(`label`; blank: the value itself). **A code list is a report’s**: every
row names its report, and a blank `output_id` is an error (a code list
two reports use is copied into each; the same arm may print one way in
one table and another way in the next).

Handed to the ARD program, a report’s code lists of the variables its
analyses read (`by`, `strata`, `variables`, and the names in `args`,
`code` and `post`) are written at the program’s head
(`cl_sex <- c(F = "Female", M = "Male")`) and put on those columns
before any analysis (`set_levels(SEX = cl_sex)`): each becomes a factor
in the list’s order with the labels as its values, so **the ARD holds
“Female”**, keeps the order and **counts a value no record has**
(`n = 0`). A value the list does not have stops the program, naming the
column and the value; a condition on such a column is written in its
labels (`SEX == "Female"`). A variable the table only prints is not made
a factor. A data made from another – an analysis set, an analysis data,
an analysis’s own subset – is made factors again as its last step
(`|> .levels()`): a value the list does not have counts only where the
data analysed have it (a table of the safety set has no column for the
screen failures’ arm).

``` r

codelists <- df(output_id = "T-14-1-1", variable = "SEX", value = c("M", "F"),
                label = c("Male", "Female"), order = 1:2)
spec3 <- tfl_ard_spec(c(base, list(analyses = df(
  output_id = "T-14-1-1", analysis_id = "SEX", method = "categorical",
  population_id = "SAF", by = "TRT01A", variables = "SEX"))))
cat(tfl_ard_code(spec3, part = "body", codelists = codelists), sep = "\n")
report_id <- "T-14-1-1"

# ---- code lists: each value in its order, as it is in the ARD ----
# (a value a list does not have stops the program: add it to the list)
cl_sex <- c(M = "Male", F = "Female")

# ---- data ----
adsl <- readRDS("data/adsl.rds")

# ---- populations ----
# adsl_sub: SAF (SAFFL == "Y")
adsl_sub <- adsl |>
  filter(SAFFL == "Y") |>
  set_levels(SEX = cl_sex)

# ---- analyses ----
# SEX
ard_sex <- adsl_sub |>
  ard_tabulate(
    by = TRT01A,
    variables = SEX,
    fmt_fun = everything() ~ fmt_default
  ) |>
  apply_fmt_fun() |>
  tag_ard(report_id, "SEX", population = "SAF")

# the report's rows of the study ARD, with the fingerprint of its
# definition (tfl_ard_spec_hash())
ard_sex |>
  save_ard(report_id, definition = "551d1e645d45f180865cffeac5fce902")
```

`set_levels()` and the other functions the program calls are not
tflspec’s exports:
[`tfl_helpers_code()`](https://ichirio.github.io/tflspec/reference/tfl_helpers_code.md)
gives their code, which a study keeps in a file of its own (tflplanner:
`programs/study_helpers.R`), so the programs run without tflspec. A
variable’s heading in a table is the `variables` sheet’s `label`; see
[The table
spec](https://ichirio.github.io/tflspec/articles/table-spec.md).

## Building the ARD

[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
runs the program from the study folder. The ARD carries `output_id`,
`analysis_id` and `population_id`;
[`tfl_ard_conditions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_conditions.md)
lists what went wrong while it was made (cards carries on when one
analysis fails, leaving the message in the ARD).

``` r

ard <- tfl_build_ard(spec, dir = study, save = FALSE)
table(ard$analysis_id)
#> 
#>  SER TEAE 
#>  126  264
tfl_ard_conditions(ard)
#> [1] output_id   analysis_id variable    groups      level       message    
#> [7] statistics 
#> <0 rows> (or 0-length row.names)
```

`tfl_check_ard(ard, spec)` checks an ARD made elsewhere (its shape, and
what the tables read from it);
[`tfl_write_ard()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard.md)
writes a copy as JSON, YAML or XPT.
[`tfl_ard_spec_hash()`](https://ichirio.github.io/tflspec/reference/tfl_ard_spec_hash.md)
is the fingerprint of an output’s definition, saved with the ARD so a
screen can tell which outputs are out of date.

## The whole program

`tfl_ard_code(spec)` is the whole file: the packages and helpers, the
data, the analyses, [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).
Write it to the study’s `programs/` and run it there; tflspec is not
needed to run it.

`tfl_ard_code(spec, output_id = )` is one report’s program, the form a
study runs (tflplanner writes one per report). The study’s program, with
every report, is one part a report after the datasets and analysis sets:
each part makes its own analysis data before its analyses, and, when a
report has code lists, reads its data again with them – so one report’s
analysis data and factors are never the next report’s.

``` r

writeLines(tfl_ard_code(spec), file.path(study, "programs", "ard.R"))
```
