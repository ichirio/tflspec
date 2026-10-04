# tflspec — AI user manual

**This manual documents tflspec 0.0.24.9027** (the development version,
after release 0.0.24; with rtfreporter 0.8.2).
Check it matches what you have — `packageVersion("tflspec")`. If they
differ, trust the package, not this file, and fetch the matching copy with
`tflspec_ai_manual()`.

**Source:** <https://github.com/ichirio/tflspec>

> **How to use this file.** Attach it at the start of a chat session and say:
> *"Use this manual when writing tflspec specifications and programs."* The
> assistant then has the specification formats, the functions that turn them
> into R code, and the common traps in context.
> 日本語で質問しても構いません（本文は英語ですが、回答は質問の言語で返ります）。

> **Scope: *using* tflspec** — writing specifications (Excel workbooks, YAML)
> and the report programs made from them. How a table becomes RTF pages is
> rtfreporter's; attach rtfreporter's own manual
> (`rtfreporter::rtfreporter_ai_manual()`) when the task is its code.

---

## 0. Ground rules for the assistant

1. **Only call functions listed in §13**, and rtfreporter's functions from
   rtfreporter's manual. tflspec is young and almost certainly *not* in your
   training data. If a requested feature has no function, say so plainly
   instead of inventing a name or an argument.
2. **tflspec writes specifications and code; it does not draw.** The ARD
   functions (`normalize_ard()`, `widen_ard()` ...), the plan
   (`table_plan()`, the `plan_*()` verbs, `plan_apply()`) and the RTF
   rendering are **rtfreporter's**. A program that runs a table spec starts
   with `library(rtfreporter)`.
3. **Every tflspec function is `tfl_*`** (and `tflspec_ai_manual()`).
   `table_plan()`, the `plan_*()` verbs, `plan_apply()`, `normalize_ard()`
   are **rtfreporter's**, never `tfl_`-prefixed; there are no aliases for
   former names (§12).
4. **A specification is data.** Workbooks are read with `col_types = "text"`:
   write every cell as text; `TRUE` / `FALSE`, numbers and `|`-lists are
   parsed and checked where they are written, with an error naming the
   sheet and column.
5. **Unknown columns are errors**, never ignored — a typo in a header would
   otherwise be a setting that silently never applies. The one exception is
   a `note` column, allowed on every sheet and never read.
6. **Sheets whose name starts with `_`** (`_methods`, `_statistics`,
   `_tflplanner` ...) and `about` are notes: never read as part of the
   definition.  **Each writer writes only its spec's sheets** (a table
   spec: `study`, the seven table sheets, `about`; a report spec: `study`,
   the six report sheets, `about`; an ARD spec: its four sheets); what a
   column means is a comment on its header cell (`tfl_spec_columns()`).
   Several specs may share one workbook (`tfl_write_specs()`): each reader
   takes its own sheets and passes over the others' sheets and `study`
   keys.
7. **`output_id` is the key.** A row with a blank `output_id` is the study
   default; a report's own row replaces the default row **per sheet** (and,
   on the keyed sheets, per key).
8. Indices are 1-based (ordinary R).

---

## 1. The one workflow

```text
ADaM ──(ARD spec)──> ARD ──(table spec)──> plan ──(report spec)──> RTF
          tfl_ard_code()      tfl_table_code()       tfl_report_code()
          tfl_build_ard()     tfl_table_plan()       tfl_report()
```

Each spec can be **run** (`tfl_build_ard()`, `tfl_table_plan()`,
`tfl_report()`) or **written out as a program** (`tfl_ard_code()`,
`tfl_table_code()`, `tfl_report_code()`) from the **same** list of steps, so
the object and the program cannot disagree. The written program is what a
study keeps: it needs rtfreporter (and cards), not tflspec.

Listings (`tfl_listing_spec()` → `tfl_listing_code()` / `tfl_listing()`)
and figures (figure design YAML → `tfl_fig_design_code()`) are the same idea.

---

## 2. Copy-paste starter (complete, runnable)

```r
library(rtfreporter)
library(tflspec)

d  <- system.file("extdata", "ard-spec", package = "tflspec")
sp <- tfl_read_table_spec(c(file.path(d, "study.xlsx"),
                            file.path(d, "report.xlsx")), output_id = "DM")

# the program a study keeps: the table plan + the document
cat(tfl_table_code(sp, pipe = "|>"), sep = "\n")
cat(tfl_report_code(sp, content = "plan"), sep = "\n")

# or run it: `data` is a normalized ARD (rtfreporter::normalize_ard())
# plan <- tfl_table_plan(data, sp)
# generate_rtfreport(tfl_report(sp, content = plan), tfl_report_path(sp), overwrite = TRUE)
```

`tfl_table_code()` writes `plan <- table_plan(data, ...) |> plan_*(...)`;
`tfl_report_code()` writes `doc <- rtf_document(...)`,
`rtf_section(...)`, `rtf_tables(doc, plan)`.

---

## 3. Program structure

A table program written from the specs has three parts:

```r
library(rtfreporter)
# 1 DATA    make `data`: an ARD, normalized
data <- normalize_ard(ard)
# 2 TABLE   the table spec as plan verbs      (tfl_table_code())
plan <- table_plan(data, cols = "TRT01P", rows = c(group = "variable")) |>
  plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
  plan_stub(name = "row_label", before = TRUE)
# 3 REPORT  the report spec as the document  (tfl_report_code())
doc <- rtf_document()
doc <- rtf_tables(doc, plan)
generate_rtfreport(doc, "output/DM.rtf", overwrite = TRUE)
```

The ARD comes from the ARD spec (§5): `tfl_ard_code(spec, output_id)`
writes the cards code; `tfl_build_ard()` runs it and saves one study ARD;
`tfl_ard_for(ard, output_id)` takes one output's rows.
`tfl_write_ard(ard, "ard.json")` writes a copy as JSON / YAML (one record
per statistic, each variable's levels in order; `tfl_read_ard()` reads it
back) or XPT (flat; factors, formats and warnings are lost, with a
warning); the rds stays the record.

---

## 4. Function map — what to call for what

| I want to … | Call |
|---|---|
| start an ARD spec / read / write it | `tfl_ard_spec_template()` / `tfl_read_ard_spec()` / `tfl_write_ard_spec()` |
| write the cards code of one output | `tfl_ard_code(spec, output_id)` |
| run the ARD spec, one study ARD | `tfl_build_ard(spec)`; one output's rows: `tfl_ard_for(ard, output_id)` |
| scaffold a table spec from an ARD | `tfl_table_spec_template(ard)` |
| read / write a table (+ report) spec | `tfl_read_table_spec()` / `tfl_read_report_spec()` / `tfl_write_table_spec()` / `tfl_write_report_spec()` |
| several specs in one workbook; what a column means | `tfl_write_specs(path, ard, table, report, listing)`; `tfl_spec_columns(sheet)` |
| the specs as CDISC ARS (JSON), and its check | `tfl_ars(ard_spec, table_spec, report_spec)` → `tfl_write_ars_json(ars, path)`; `tfl_check_ars(ars)`; what ARS cannot say: `tfl_ars_unmapped(ars)`; CDISC's Excel template to read: `tfl_write_ars_xlsx(ars, path)` (§5.1) |
| ARS siera can run, and its ARD (round trip) | `tfl_ars(ard_spec, profile = "siera")` → `tfl_ars_ard(ars, adam)` (siera writes and runs one programme per output) |
| an ARS JSON (anyone's) back as specs | `tfl_read_ars_json(path)` → `tfl_ars_to_specs(ars, table = FALSE)`: `$ard`, `$report` (`$table`: the groupings' levels); fill `datasets$path`; what has no place: `attr(, "unmapped")` |
| the plan a table spec stands for | `tfl_table_plan(data, spec)` (then any rtfreporter verb: last wins) |
| the plan as code | `tfl_table_code(spec)` |
| a plan written in code, back to a workbook | `tfl_as_table_spec(plan)` |
| a listing written in code (`listing_spec()` / `plan_listing()`), back to a listing spec | `tfl_as_listing_spec(x, output_id, dataset = )` |
| the document / its code / its file | `tfl_report(spec, output_id, content = plan)` / `tfl_report_code(spec)` / `tfl_report_path(spec)` |
| a listing's program / its pages | `tfl_listing_code(spec)` / `tfl_listing(data, spec)` |
| a figure: start / check / write the script | `tfl_fig_template()` / `tfl_check_fig_design()` / `tfl_fig_design_code()` |
| attach this manual to a chat session | `tflspec_ai_manual(file = )` |

### 4.1 How the columns are named

The column names follow two rules. **Table, listing and report columns
are rtfreporter's argument names** (a `layout` column is the verb's prefix
and its argument: `pages_max_rows` is `plan_paginate_rows(max_rows = )`).
**ARD columns are cards' argument names** (`by`, `variables`, `strata`,
`denominator`), with two exceptions: `statistics` (cards' `statistic`) and
`formats` (tflspec's own). A column whose value has a unit says it
(`_twips`, `_in`, `_half_points`; `rel_width` is a relative width). One
argument is written in one place: a column, or `args`, never both.

---

## 5. ARD spec — four sheets

`tfl_ard_spec()`, `tfl_read_ard_spec()`, `tfl_write_ard_spec()`,
`tfl_ard_spec_template()`.

| Sheet | Columns | Meaning |
|---|---|---|
| `study` | `key`, `value` | `id`: the subject key (`USUBJID`); `output`: where the study ARD goes (`output/ard/ard.rds`); `source`: R files of the study's own analysis functions, relative to the study folder (`|` between them) |
| `datasets` | `dataset`, `level`, `path`, `derive` | a name for the data, its level (`SDTM` / `ADaM`), its file relative to the study folder, new columns (`NAME = R expression`, `|` between them) |
| `populations` | `population_id`, `dataset`, `where`, `derive` | an analysis set: the subjects of `dataset` for which `where` (R) holds; `derive` adds columns (`TRTA = TRT01A`) |
| `analyses` | `output_id`, `analysis_id`, `label`, `method`, `dataset`, `population_id`, `where`, `by`, `strata`, `variables`, `statistics`, `denominator`, `formats`, `args`, `code`, `purpose`, `reason` | one analysis a row; both ids become ARD columns; `purpose` / `reason` (CDISC terms) only for `tfl_ars()` |

- `method`: a keyword — `continuous`, `categorical`, `dichotomous`,
  `missing`, `hierarchical`, `max`, `subjects`, `total_n`, `proportion_ci`,
  `mean_ci`, `ttest`, `wilcox`, `chisq`, `fisher`, `custom` — or any
  `pkg::function` (`cards::`, `cardx::`), or a function of the study's own
  that the study key `source` loads (`ard_riskdiff_mn`): it takes the
  analysis data first, `by` and `variables` as bare column names, and gives
  a cards ARD. `tfl_ard_methods()` lists the
  keywords, each with a `label` (the name a person reads: "Summary
  statistics", "Counts and percents", "Nested counts (e.g. SOC / PT)" …)
  and a one-line `note`; `tfl_ard_statistics()` the statistics and their
  formats. `tfl_ard_functions()` lists every installed cards / cardx
  `ard_*()` by category with a heading and a sentence, and
  `tfl_ard_args("cardx::ard_categorical_ci")` one function's arguments
  (default, required, what fills each, the analysis row's column it goes
  in: `by` / `variables` / `strata` / `denominator` / `statistics`, else
  `args`). Not offered, by design: the old names (`replaced_by` gives the
  new one), the survey-design functions (their input is not a data frame)
  and `ard_formals()` (not an analysis). cards >= 0.8.0, cardx >= 0.3.1.
- `by`, `strata`, `variables`, `statistics`: `|` between several. `strata`:
  the analysis repeated within them (cards' `strata`: a subgroup, a
  parameter by visit).
- `parent`: the analysis this one runs inside, when that one's method is
  `cards::ard_stack` (several analyses on the same data and `by`, plus the
  by counts and the total N with `args` `.total_n = TRUE`),
  `cards::ard_strata` (within the subgroups of its `by` / `strata`) or
  `cards::ard_pairwise` (each pair of the levels of its one `variables`).
  The rows inside leave `dataset` / `population_id` / `where` (and in a
  stack `by`) blank: they are the parent's. The program writes one call,
  `cards::ard_stack(pop_saf, .by = ARM, cards::ard_continuous(variables =
  c(AGE, BMIBL)), cards::ard_categorical(variables = SEX))`, and tags each
  variable's rows with its own analysis (the stack's own rows with the
  parent's id). ARS gets each row inside as an analysis of its own.
- `post`: steps on the ARD after the call, the ARD left out, `|` between
  them — `cards::add_calculated_row(expr = sd / sqrt(N), stat_name = "se")`,
  `cards::filter_ard_hierarchical(p > 0.05)`, `cards::sort_ard_hierarchical()`.
  Written as `ard <- ard |> step1 |> step2`. Not on a row inside a parent
  (put it on the parent).
- `denominator`: what percentages are of — `population` (the analysis
  set; `hierarchical` and `max` take it anyway), `row` / `column` / `cell`
  (cards), another population, or a dataset (its records of the analysis
  set's subjects). Not in `args` as well.
- `args`: more arguments as R (`over_variables = TRUE, overall = TRUE`),
  read as the arguments of a call — any order; refused if not R.
- `formats`: `mean=xx.x | p=xx.x% | AGE:sd=xx.xx` — the `xx` part says the
  decimals only.
- `custom` takes R in `code`; `args` passes arguments to the method.
- In `args` and `code`, `data` is the analysis data and `population` the
  analysis set's subjects.
- A function that takes a **formula** (`cardx::ard_survival_survdiff`,
  `cardx::ard_regression`, `cardx::ard_stats_aov`) leaves `by` and
  `variables` blank and gets `formula = ...` in `args`.
  `cardx::ard_survival_survfit` takes `y` as **text**
  (`y = "survival::Surv(AVAL, 1 - CNSR)"`) and the groups in `variables`.
- For CIs, tests and models, `statistics` chooses which of the results to
  **keep** (`estimate | conf.low | conf.high`); for `continuous`,
  `categorical` and `missing` it says what to compute.
- `tfl_read_ard_spec(check = FALSE)` (and `tfl_read_listing_spec(check =
  FALSE)`) read a definition still being written; a table or report spec
  is always checked when read.
- A keyword has arguments of its own unless `args` gives them:
  `hierarchical` and `max` get `denominator = population, id = <id>`
  (`tfl_ard_methods()$defaults`).

Rows of `analyses` for a Kaplan-Meier estimate and a Cox model (the
columns not given are blank):

```r
# manual example: analyses rows for survival and a model
km  <- list(output_id = "T-14-2-2", analysis_id = "KM", population_id = "SAF",
            dataset = "ADTTE", method = "cardx::ard_survival_survfit",
            variables = "TRTA",
            args = 'y = "survival::Surv(AVAL, 1 - CNSR)", times = c(30, 90)')
cox <- list(output_id = "T-14-2-2", analysis_id = "HR", population_id = "SAF",
            dataset = "ADTTE", method = "cardx::ard_regression",
            args = 'formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, method = "coxph", package = "survival"')
```

### 5.1 CDISC ARS — writing and reading the analyses as the standard

The specs stay the source; ARS (CDISC Analysis Results Standard v1.0) is
written from them, and read back into them.  The JSON is the form to
exchange.

```r
ars <- tfl_ars(ard_spec, table_spec, report_spec)   # profile = "cdisc"
tfl_check_ars(ars)                 # 0 rows = valid (jsonvalidate: CDISC's schema too)
tfl_ars_unmapped(ars)              # what ARS cannot say, with the reason
tfl_write_ars_json(ars, "ars/study_ars.json")
tfl_write_ars_xlsx(ars, "ars/study_ars.xlsx")       # CDISC's Excel template, to read

# siera runs it: the same analyses, made runnable, and their ARD
ars_s <- tfl_ars(ard_spec, profile = "siera")
ard   <- tfl_ars_ard(ars_s, adam)  # adam: named list or folder

# anyone's ARS back into specs
sp <- tfl_ars_to_specs(tfl_read_ars_json("their_ars.json"), table = TRUE)
sp$ard; sp$report; sp$table       # fill sp$ard$datasets$path before running
```

- `purpose` (PRIMARY / SECONDARY / EXPLORATORY OUTCOME MEASURE) is the
  SAP's decision: fill the analyses' `purpose` column (or
  `tfl_ars(purpose = )`); it is never guessed, and `tfl_check_ars()` names
  a blank one.  `reason` defaults to SPECIFIED IN SAP.
- The layout is CDISC's own example's: a categorical variable is a count
  of the subject key grouped by the variable; its percentage names the
  output's subject count (added when the output has none).
- `profile = "siera"` leaves out what siera has no template for (t test,
  Wilcoxon, Fisher, mean CI, missing, custom ...) and lists it; a hierarchy
  keeps its nesting (two levels below the arm at most).
- Reading: an analysis whose method tflspec has no keyword for (an ANOVA,
  SAS code only) is not made a row and is listed in
  `attr(sp, "unmapped")`.  ARS has no ADaM path.

---

## 6. Table spec — sheets and columns

`tfl_table_spec()`, `tfl_read_table_spec()`, `tfl_write_table_spec()`,
`tfl_table_spec_template(ard)` (scaffolds `tables` / `variables` / `cells`
from an ARD). A workbook may be split over several files; a sheet is said
once. The `study` sheet has `key` / `value` (`rounding`: `sas` / `iec` /
`r`; `output_path`; `program_dir`).

**tables** (one row a table) — the roles and the table-wide layers:

| Column | Goes to | Notes |
|---|---|---|
| `cols` | `table_plan(cols = )` | column keys, outermost first: `TR01AG1 \| SEROSTAT` |
| `rows` | `table_plan(rows = )` | `name = column`; a quoted value is a constant heading |
| `label` | `table_plan(label = )` | blank keeps `.label`; `NA` builds then drops; `NULL` leaves out |
| `stats` | `plan_cells(stats = )` | `cells` (default: a cell from a template) or `rows` (one statistic a row, the raw values) |
| `value` | `plan_cells(value = )` | which column a cell's values come from: `stat` (the number; default) or `stat_fmt` (cards' formatted text). **Not** a template: templates are `cells$template` |
| `na` | `plan_cells(na = )` | what a cell no template could fill prints |
| `sep` | `plan_columns(sep = )` | what joins several column keys into a column name (default `____`) |
| `sort` | `plan_sort()` | `TRUE`, `FALSE`, or keys in order, `-` for descending |
| `sort_stat` | `plan_sort(stat = )` | the statistic totalled for a frequency order |
| `header_n` | `plan_col_header(values = list(n = ))` | `page`, `table`, or `n = page \| N = table` |

**variables**: `variable`, `label`, `order`, `levels` (`Grade 0 | Grade 1`)
→ `plan_labels()`, `plan_levels()`.

**codelists** (the study's code list, one row a value): `variable`, `value`, `label`, `order` → `plan_labels(SEX = c(SEX = "Sex", F = "Female"))` and `plan_levels(SEX = c("M", "F"))`. A report's own rows replace the defaults of the same variable / value; a variable's `levels` on the `variables` sheet, when given, is the order instead.

**cells**: `variable`, `context`, `row`, `when`, `template`, `digits`,
`signif` → `plan_cells()`. A row with `variable` blank is the table's
default template. Rows with the same variable / context / row are
one chain tried in order; `when` is an R guard (`n == 0`). A row with **no
template** in a `stats = rows` table is one statistic's digits →
`plan_digits(.rows = c(Mean = 2, SD = "3s"))` (`"3s"` = 3 significant).

A table of subjects by SOC and PT, `n (%)` in every cell:

```r
# manual example: a table's tables and cells rows
tables <- data.frame(output_id = "T-14-3-1", cols = "TRTA",
                     rows = "group1 = AESOC", label = "label = AEDECOD")
cells  <- data.frame(output_id = "T-14-3-1", template = "{n} ({p:.1f%})")
```

**layout** (one row a table; prefix = verb, suffix = argument):

| Columns | Verb |
|---|---|
| `pages_max_rows`, `pages_split`, `pages_break_before`, `pages_min_group_rows`, `pages_cont_label`, `pages_page_by` | `plan_paginate_rows()` (`page_by`: BY pages, the row budget inside each) |
| `group_mode`, `group_collapse` | `plan_row_group()` |
| `group_page`, `group_col`, `group_keep` | `plan_paginate_group(col, keep)` — one page per value |
| `blank_where`, `blank_first`, `blank_last`, `blank_counted` | `plan_blanks()` |
| `stub_vars`, `stub_name`, `stub_indent`, `stub_summary`, `stub_before` | `plan_stub()` |
| `colpages_every`, `colpages_at`, `colpages_cut_by`, `colpages_keep`, `colpages_fit`, `colpages_allow_span_break`, `colpages_order` | `plan_paginate_cols()` |

**columns** (one row a printed column, by name; `.values` = every value
column): `column`, `rel_width`, `row_title`, `decimal_split`, `hide` →
`plan_columns(widths, row_title, decimal)`, `plan_hide()`.

**style** (one row a table): `plan_style()`'s arguments — `border`,
`align_count_pct`, `font`, `font_size_half_points`, `row_height_twips`,
`row_height_exact`, `header_row_height_twips`, `blank_row_height_twips`,
`cell_padding_left_twips`, `cell_padding_right_twips`, `cell_valign`,
`table_align`, `markup`, `blank_row_normalize`, and one kind of row's rules
`border_header` … `border_last_row` written as sides (`top | bottom`) or
`none`; the default look `header_align`, `header_bold`, `header_italic`,
`align`, `bold`, `italic`, `underline` (one `rtf_table_style()` with the
`border_*` columns; a blank `align` keeps each column's default) and
the width `table_width_twips`, `table_width_pct`,
`table_width_pct_of_writable`; plus `auto_width` (→ `plan_columns()`) and
`col_header_align` (→ `plan_col_header()`). Not in a sheet (they stay in
code): `plan_columns(cell_format =)` (a function), `column_widths_twips`
(`rel_width` with `table_width_twips` sets the same widths), and
`plan_col_header(header_sep =)` (the column names are joined by
`tables$sep`).

**cell_styles** (one row a `plan_cell_style()`, in order; a report's own
rows replace the defaults whole): `cols` (`.values` = every value column),
`header` (`TRUE`: the column header), `where` (an R condition over the
table's columns: `label == "Any TEAE"`), `bold`, `italic`, `align`,
`color`, `background`, `underline`, `indent_twips` (not on the header).
Two rows with a `where` may not set the same look.

**col_header** (one row a header cell): `line`, `cols` (a name, `.values`,
a position or range `3:last`, or `KEY = value`), `span` (blank: one cell;
`each`: one per column; a key: one per value), `text` (tokens `{col}`,
`{col1}`, `{n}`, `{n1}`, `{n:sum}`), `align`, `bold`, `border_top`,
`border_bottom` → `plan_col_header(header = )`.

`tfl_as_table_spec(plan)` writes a plan back as a workbook (compared by its
RTF, byte by byte; `compare = FALSE` skips that) and lists what a
sheet cannot say (`attr(, "not_converted")`) — a look computed row by
row (`bold = ~ ...`), `plan_after()` steps, guarded labels stay in code.

---

## 7. Report spec — the document

`tfl_read_report_spec()` reads the table sheets **and** these; `tfl_report()`
runs them, `tfl_report_code()` writes them, `tfl_report_path()` gives the
file.

| Sheet | Columns |
|---|---|
| `report` | `type` (`table` / `listing` / `figure`), `file` (`{output_id}.rtf`), `program`, `auto_section`, `section_align`, `auto_title`, `title_align`, `table_font_size_half_points`, `title_font_size_half_points`, `footnote_font_size_half_points`, `page_header`, `page_footer`, `watermark` (`DRAFT`), `figure_width_in`, `figure_height_in` |
| `page` | `paper_size`, `orientation`, `width_in`, `height_in`, margins `margin_*_in`, `header_dist_in`, `footer_dist_in`, `font_size_half_points`, `title_format`, `footnote_format`, `title_width`, `footnote_width`, `markup` |
| `header`, `footer`, `titles`, `footnotes` | `line`, `left`, `center`, `right` — a report's line replaces the default line of the same number; a report's line that says `(none)` takes it out |

Page tokens in the running header / footer: `{PAGE}`, `{TOTAL_PAGES}`,
`{PROGRAM}`, `{DATETIME}`.

### 7.1 A company's TOC as report specs

A study's list of outputs in the company's own workbook (or `.csv`) is
read through a map of its columns into the `report`, `titles` and
`footnotes` sheets -- the reports exist, titled, before any table:

```r
sp <- tfl_read_toc("TOC.xlsx", sheet = "TOC", skip = 0,
                   map = list(output_id = "Output No.", type = "Type",
                              title = c("Title 1", "Title 2"),
                              population = "Analysis Set",
                              footnote = "Footnotes", program = "Program"))
attr(sp, "guessed")   # reports whose kind came from the id / title
tfl_write_specs("spec/report.xlsx", report = sp)
```

- `map` names are the fields (`output_id` required; `type`, `title`,
  `population`, `footnote`, `program`, `file`, `note`); values are the
  TOC's column names, matched ignoring case. A missing column names the
  closest ones.
- `title` / `footnote`: one column or several; a cell's line breaks or
  `" | "` make lines. `population` is the last title line.
- The kind: `Table` / `tbl` / `Figure` / `Fig` / `Listing` ... read
  loosely; else from the id or first title (`T-14-1-1`, `Table 14.1.1`,
  `F14.2`); else `table`.
- A row without an output id that says one thing at most (a section
  heading) is skipped (`attr(, "skipped")`); one that says more, and an id
  given twice, are errors.

---

## 8. Listing spec

`tfl_listing_spec()`, `tfl_read_listing_spec()`, `tfl_write_listing_spec()`;
`tfl_listing_code()` writes the program, `tfl_listing()` makes the pages.

| Sheet | Columns |
|---|---|
| `listings` (one row a listing) | `output_id`, `type` (`multiline`), `dataset`, `where` (R), `sort` (`-` for descending), `max_rows`, `blank_row` (`TRUE` / `FALSE`), `wrap` (the name of an R function) |
| `listing_cols` (one row a column) | `output_id`, `vars` (`|` stacks variables in one column), `label`, `width` (characters), `sep` (quoted to keep spaces: `" / "`), `align`, `collapse_repeats` |

Layout on the page is rtfreporter's `listing_spec()` / `as_rtftables()`.
`tfl_as_listing_spec(x, output_id, dataset = )` writes a `listing_spec()`
or a `plan_listing()` plan back as a listing spec, listing what the sheets
cannot carry (`attr(, "not_converted")`); from a plan it compares the RTF.
`tfl_read_data_code(datasets, dataset)` writes the line that reads a
dataset of the data catalog (`.rds`, `.xpt`, `.sas7bdat`, `.csv`,
`.parquet`).

---

## 9. Figure design (YAML)

`tfl_fig_design()`, `tfl_read_fig_design()` / `tfl_write_fig_design()`
(one `.yml` a figure), `tfl_fig_design_code()` writes the ggplot2 script,
`tfl_check_fig_design()` / `tfl_fig_advice()` check it, `tfl_fig_template()`
starts one (`tfl_fig_templates()` lists 38, each with its `category` --
the clinical category of `tfl_fig_catalog()` -- and the `data` it reads:
`"ADTR + ADRS"` both, `"ADLB / ADVS + ADSL"` one of ADLB / ADVS and ADSL),
`tfl_fig_parts()` lists every piece and field.

```yaml
template: km_risk_table
data:                                   # ADaM -> df
- {step: read, dataset: ADTTE}
- {step: param, value: OS}
- {step: flag, variable: FASFL}
stats:                                  # computed from df
- {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01A}
plot: {x_label: Time (Months), colour_by: TRT01A, legend: inside}
layers:                                 # what is drawn, in order
- {layer: km_curve}
- {layer: risk_table}
```

- `data` steps: `read`, `join`, `param`, `flag`, `filter`, `derive`,
  `time_unit`, `levels`, `rank`, `data_code`.
- `layers`: catalog layers (`line`, `point`, `errorbar`, `boxplot` … see
  `tfl_fig_add_layer()`), `geom` (any geom by name), `call` (any function:
  `fn`, `package`, `data`, `aes`, `pos`, `args`), `layer_code`, `figure`.
- `plot.add`: `call`s written after the figure's settings (`theme()`,
  `scale_*()`, `facet_*()`, `labs()` …).
- Raw R inside a value is the `!r` tag: `labels: !r scales::label_number()`
  (`tfl_fig_r()` in R).
- Composed figures: `plots:` (name → a whole design) and `compose:`
  (`layout: km | box`, `add:` patchwork calls).
- `ggplot2_version: "3.5"` or `"4.0"` writes the script for that version
  (`tfl_fig_compat()` lists the differences); `tfl_fig_calls()` lists
  extension functions offered by name.

The older sheet-based figure spec (`tfl_fig_spec()`, `tfl_fig_code()`,
`tfl_fig_km()`, `tfl_fig_waterfall()` …) and the quick figure functions
remain; new work should use the design.

---

## 10. What the written code looks like — the plan

The plan is rtfreporter's (its manual §17). In its words: `table_plan()`
declares the **roles** once; each `plan_*()` **verb** adds a layer; a later
layer **wins** (last wins), which is why a verb piped after
`tfl_table_plan(data, spec)` changes one report without touching the
workbook; nothing runs until `plan_apply()` (or `rtf_tables(doc, plan)`),
and `plan_apply(plan, stage = "args")` shows the calls it amounts to.

- Tables: `table_plan()` takes **only the roles** — `cols`, `rows`,
  `label`, `stat`. Everything else is a verb: `plan_cells(stats =, value =,
  na =, notes =)`, `plan_sort(stat =)`, `plan_columns(widths =, sep =,
  row_title =, decimal =, auto_width =)`, `plan_col_header(header =,
  values =)`, `plan_digits(.rows =)`, `plan_stub(name =)`,
  `plan_paginate_group(col =, keep =)`, `plan_paginate_cols(keep =)`,
  `plan_cell_style(cols =, header =, where =, bold = …)`.
- `rtf_tables(doc, plan)` takes a plan directly; `plan_apply(plan, "args")`
  shows the calls it amounts to.
- Programs are written unqualified (`table_plan()`, not
  `rtfreporter::table_plan()`), so they need `library(rtfreporter)`.

---

## 11. Composing with tflplanner

tflplanner is the GUI over these specs (one study folder: `spec/`,
`programs/`, `output/`). What it saves is these workbooks and YAML files;
anything written here can be opened there.

---

## 12. Names that do NOT exist

| Wrong | Right |
|---|---|
| `tfl_table_plan_*()`, `tfl_plan_digits()` … | rtfreporter's `plan_*()` verbs, piped after `tfl_table_plan()` |
| `tflspec::table_plan()`, `tflspec::plan_apply()` | `rtfreporter::table_plan()`, `rtfreporter::plan_apply()` (or bare, after `library(rtfreporter)`) |
| `tfl_read_spec()`, `tfl_spec()` | `tfl_read_table_spec()` / `tfl_read_report_spec()` / `tfl_read_ard_spec()` / `tfl_read_listing_spec()`, by kind |
| `tfl_fig_design_write()`, `tfl_write_fig_yaml()` | `tfl_write_fig_design()` |
| `tfl_ard_normalize()`, `tfl_plan()`, `tfl_plan_*()`, `tfl_apply_plan()` | rtfreporter's `normalize_ard()`, `table_plan()`, `plan_*()`, `plan_apply()` |
| `spread_ard()`, `plan_fmt()`, `plan_header_style()` / `plan_col_style()` / `plan_zone_style()` | `widen_ard()`, `plan_digits(.rows = )`, `plan_cell_style()` |
| `table_plan(stats =, value =, na =, notes =, sort_stat =, sep =)` | `plan_cells()`, `plan_sort(stat =)`, `plan_columns(sep =)` |
| `plan_stub(into =)`, `show = FALSE`, `plan_col_header(n =)`, `plan_paginate_cols(carry =)` | `name =`, `keep = FALSE`, `values = list(n = )`, `keep =` |
| layout `stub_into`, `group_show`, `colpages_carry`, `pages_by` | `stub_name`, `group_keep`, `colpages_keep`, `group_page = TRUE` + `group_col` |
| an ARD back to an ARD spec (`tfl_as_ard_spec()`) | none: an ARD has no data paths, populations, `where` or `derive`, so a spec made from it could not make it again. An existing analysis comes in through CDISC ARS: `tfl_ars_to_specs()` |

A workbook with a former column is refused with the column to write instead.
With `sep = "_"`, a key value may not contain `_` (it could not be split
back): rename the value or choose another separator.

---

### 12.1 How the functions are named and take their arguments

- Every function starts with `tfl_`, except `tflspec_ai_manual()` (the
  same form as rtfreporter's `rtfreporter_ai_manual()`).
- A spec is read with `tfl_read_<kind>_spec(path)` and written with
  `tfl_write_<kind>_spec(spec, path)`; what a spec makes is named after
  what it makes: `tfl_build_ard()` (builds and saves the ARD),
  `tfl_table_plan()`, `tfl_listing()`, `tfl_report()`; its program is
  `tfl_<kind>_code()`.
- The spec comes first, then `output_id`, then the rest:
  `tfl_ard_code(spec, output_id)`, `tfl_table_code(spec, output_id)`,
  `tfl_report(spec, output_id, content)`. A function that makes pages from
  data takes the data first: `tfl_table_plan(data, spec)`,
  `tfl_listing(data, spec)`. A template takes
  its material first and the path second: `tfl_table_spec_template(ard,
  path)`, `tfl_fig_list_template(adam, path)`.
- `tfl_plot_*()` draw one part (`tfl_plot_sankey()`); `tfl_fig_*()` make a
  figure from ADaM (`tfl_fig_km()`).
- A function given something other than what it takes says what it
  wants and what it got; a file that is not there is
  `<function>(): no file '<path>'`.

## 13. Complete public API (nothing outside this list exists)

**Manual:** `tflspec_ai_manual`

**ARD spec:** `tfl_ard_spec` `tfl_read_ard_spec` `tfl_write_ard_spec`
`tfl_ard_spec_template` `tfl_ard_code` `tfl_build_ard` `tfl_ard_for`
`tfl_ard_spec_hash` `tfl_ard_methods` `tfl_ard_statistics`
`tfl_ard_functions` `tfl_ard_args` `tfl_write_ard` `tfl_read_ard`

**CDISC ARS:** `tfl_ars` `tfl_write_ars_json` `tfl_check_ars`
`tfl_ars_unmapped` `tfl_ars_ard` `tfl_read_ars_json` `tfl_ars_to_specs`
`tfl_write_ars_xlsx`

**Table spec:** `tfl_table_spec` `tfl_read_table_spec` `tfl_write_table_spec`
`tfl_write_report_spec` `tfl_write_specs` `tfl_spec_columns`
`tfl_table_spec_template` `tfl_table_plan` `tfl_table_code`
`tfl_as_table_spec`

**Report spec:** `tfl_read_report_spec` `tfl_read_toc` `tfl_report` `tfl_report_code`
`tfl_report_path`

**Listing spec:** `tfl_listing_spec` `tfl_read_listing_spec`
`tfl_write_listing_spec` `tfl_listing` `tfl_listing_code`
`tfl_as_listing_spec` `tfl_read_data_code`

**Figure design:** `tfl_fig_design` `tfl_read_fig_design`
`tfl_write_fig_design` `tfl_fig_design_code` `tfl_check_fig_design`
`tfl_fig_advice` `tfl_fig_apply_fix` `tfl_fig_template` `tfl_fig_templates`
`tfl_fig_parts` `tfl_fig_add_layer` `tfl_fig_calls` `tfl_fig_compat`
`tfl_fig_r`

**Figure style:** `tfl_fig_style` `tfl_fig_style_template`
`tfl_read_fig_style` `tfl_fig_setup_code` `tfl_fig_palettes`
`tfl_check_fig`

**Figure spec (sheets) and quick figures:** `tfl_fig_spec`
`tfl_fig_spec_template` `tfl_read_fig_spec` `tfl_check_fig_spec`
`tfl_example_fig_spec` `tfl_fig_code` `tfl_write_fig_code` `tfl_fig_schema`
`tfl_fig_types` `tfl_fig_catalog` `tfl_fig_list_template`
`tfl_fig_list_code` `tfl_fig_km` `tfl_fig_waterfall` `tfl_fig_swimmer`
`tfl_fig_forest` `tfl_fig_mean` `tfl_fig_box` `tfl_fig_bar`
`tfl_fig_scatter` `tfl_fig_individual` `tfl_fig_pk` `tfl_fig_ae_dot`
`tfl_fig_butterfly` `tfl_fig_edish` `tfl_fig_sankey` `tfl_fig_sunburst`

**Sankey / sunburst:** `tfl_plot_sankey` `tfl_plot_sankey_batch`
`tfl_sankey_data` `tfl_plot_sunburst` `tfl_sunburst_data`

**Data:** `tfl_read_adam` `tfl_example_adam`

---

## 14. Troubleshooting

| Message | Cause | Fix |
|---|---|---|
| `The \`layout\` sheet has a column it does not read: 'stub_into' (renamed \`stub_name\`)` | a workbook from before 0.0.24 | rename the column as the message says (§12) |
| `tfl_table_plan(): 'notes' is not a role` | table-wide options passed to `tfl_table_plan()` | pipe into the verb: `plan_cells(notes = FALSE)` |
| `could not find function "table_plan"` | the program did not attach rtfreporter | `library(rtfreporter)` first |
| `... has a value containing the key separator "_"` | `sep = "_"` and a key value with `_` | rename the value, or `plan_columns(sep = )` / `tables$sep` |
| `The sheet X has rows in both a.xlsx and b.xlsx` | a sheet said in two workbooks | keep its rows in one |
| a header prints `N=NA` with a warning | the ARD does not state that population | add it to the ARD (`cards::ard_total_n()` …) or give `plan_col_header(values = list(n = ...))` |
