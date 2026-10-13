# tflspec: from a TOC and shells to specs -- a guide for the assistant

> **Scope.** Read this after `tflspec-ai-user-manual.md` (the manual).  The
> manual says what each spec is: its sheets, columns and values.  This
> guide says how to get them out of what a company actually hands over:
> a table of contents (TOC) and mock tables (shells), each in its own
> layout.  Nothing here adds a column or a function the manual does not
> have; when the two seem to differ, the manual is right.

A study's outputs come as two documents.  The **TOC** lists them: an id, a
title, a population, footnotes.  A **shell** is one output drawn empty:
the column header, the rows, `xx (xx.x%)` where numbers will go.  The job
is to write, from those, the files tflspec reads -- the ARD spec, the
table spec, the report spec, the listing spec and a figure design -- so
that the programs tflspec writes make exactly the tables the shells show.

Two rules above all:

1. **Copy, do not compose.**  A title is the TOC's title, character for
   character.  A row label is the shell's.  A footnote is the shell's or
   the TOC's.  Never improve the wording.
2. **Everything you guessed goes in `Assumptions and questions`** (§9).
   A shell never says which dataset a row comes from, what a percent is
   of, or which test a p-value is from.  You will choose; say that you
   did, and ask.

---

## 1. What the kit holds, and what to return

The folder the user attached (`tflspec_ai_manual(dir)`) is flat:

| File | What it is |
|---|---|
| `00_START_HERE.md` | the request, and this table |
| `01_tflspec-ai-user-manual.md` | the manual: sheets, columns, values, functions |
| `02_tflspec-ai-toc-shell-guide.md` | this guide |
| `03_blank_specs.xlsx` | every sheet of every spec, empty; a `_columns` sheet describes each column (sheet, column, description, example) |
| `04_blank_figure.yml` | a figure design with every field, commented |
| `05_example_toc_shell.xlsx` | a made-up TOC and four shells (§10) |
| `06_example_specs.xlsx` | the specs written from them, in one workbook |
| `07_example_figure.yml` | the figure design written from the figure shell |

Return files, not prose: one workbook with the ARD, table, report and
listing specs (the sheets of `03_blank_specs.xlsx`, filled), and one
`.yml` per figure.  A chat that cannot write `.xlsx` returns one CSV block
per sheet, titled with the sheet's name.  Keep the blank workbook's
sheet names and column headers exactly; leave a column you do not use
empty rather than deleting it; add nothing but a `note` column or a sheet
whose name starts with `_` (§9: `_assumptions`).

---

## 2. Reading the TOC

### 2.1 Find the table

A TOC is a sheet in a workbook, or a `.csv`.  Expect:

- **Sheet names vary**: `TOC`, `Table of Contents`, `TFL list`, `Outputs`,
  `Shells`, a study id.  Take the sheet whose rows have an output id
  column (§2.2).  A workbook may hold the TOC and every shell as sheets
  (`05_example_toc_shell.xlsx` does), or the shells may be a second
  workbook or a Word document.
- **The header is not always row 1.**  A title line ("XYZ-001 Table of
  Contents", a version, a date) often sits above it; the header is the
  first row whose cells read like column names.  `tfl_read_toc(skip = )`
  counts the rows above it.
- **Blocks and headings.**  Rows with no id and one text ("14.1
  Demographics") are section headings; they are not outputs.  Blank
  rows separate blocks.  Both are skipped by `tfl_read_toc()`.
- **Merged cells** repeat a value downwards: a section or a population
  written once for several rows.  Fill it down.

### 2.2 Which column is what

`tfl_read_toc()` takes a `map` from its fields to the TOC's column names.
The names below are common; match them ignoring case and blanks, and
say in the assumptions which column you took for each field when the
name was not obvious.

| Field | Column names seen | Notes |
|---|---|---|
| `output_id` | Shell Number, Output No., TFL ID, Table No., Number, ID | required; must be unique; letters, digits, `-` `_` `.` only (a file is named after it) |
| `type` | Type, Kind, Output Type, Category | Table / Listing / Figure in any spelling; missing: from the id (`T-`, `L-`, `F-`) or the first title ("Table 14.1.1 ...") |
| `title` | Title, Title1, Title2, Title3, Title Line 1 ... | one column or several, in order; a cell with line breaks or `" \| "` is several lines |
| `population` | Population, Analysis Set, Analysis Population | becomes the last title line; also the analysis set of the ARD spec (§4.1) |
| `footnote` | Footnote, Footnotes, Footnote1 ..., Notes | one or several; blank is fine |
| `program` | Program, Program Name, Macro | optional |
| `section` | Section, Domain, Category | optional; else the heading row above |
| `datasets` | Dataset(s), Source, Input Data | optional; "ADSL, ADAE" |
| `label` | Shell, Reference, Display No. | the id as printed ("Table 14.1.1"); optional |

What the map does **not** have: a phase column, a topline / priority
flag, a SAP section, a shell reference, a status.  They are not part of
the spec.  Do not invent fields for them; mention them in the
assumptions only when they change what you write (§2.3).

### 2.3 The columns that need a decision

- **Phase / period columns** ("Phase", "Ph1 / Ph2", "Part A / B", a column
  per phase with Y / N).  One row may mean one output per phase (two
  reports, two ids) or one output with both.  Write one report per id
  the TOC gives; if the TOC gives one id for two phases, write one
  report and ask.
- **Title1 / Title2 / Title3.**  Title1 is usually the id as printed
  ("Table 14.1.1"), Title2 the title, Title3 a qualifier.  Keep them as
  separate title lines in that order.  Do not move the population into
  a title column you were not given; `population` is its own field and
  prints last.
- **Population text** ("Safety Analysis Set", "Safety Population", "ITT",
  "All Randomized Subjects").  It names the analysis set.  Map each
  distinct text to one `population_id` of the ARD spec (§4.1), the same
  id for the same text everywhere.
- **Footnotes in the TOC and in the shell.**  The shell's footnotes are
  the output's; the TOC's are usually the same or a subset.  When they
  differ, take the shell's and list the difference as a question.
- **"Shell number" vs "output number".**  Some TOCs give a shell
  reference ("DM-01") beside the output id: several outputs may share one
  shell (the same layout for two populations).  The spec's `output_id`
  is the output's id; the shell reference is a `note`.
- **TOC and shells in one workbook.**  A shell sheet is often named by the
  id; a shell with no TOC row, or a TOC row with no shell, is a question,
  not an error you fix silently.

### 2.4 What to write from the TOC

The report spec's `report`, `titles` and `footnotes` sheets, one row per
line, keyed by `output_id`:

- `report`: `output_id`, `type` (`table` / `listing` / `figure`).  `file`
  and `program` stay blank (`{output_id}.rtf`, the program's own name)
  unless the TOC names them.
- `titles`: `line` 1, 2, 3 ... with the TOC's title lines in `center`
  (the usual placement; `left` when the shell shows them left-aligned);
  the population last.
- `footnotes`: `line` 1, 2 ... in `left`.
- Study-wide rows (`output_id` blank) for what every output shares: the
  running `header` and `footer` lines the shells show at the top and
  bottom of the page (`{PAGE}`, `{TOTAL_PAGES}`, `{PROGRAM}`,
  `{DATETIME}` where the shell shows "Page x of y", a program name, a run
  date), `page` (orientation, font) when the shells say it.

`tfl_read_toc()` does this for a TOC in a known layout; what you do by
hand must give the same rows it would.

---

## 3. Reading a shell

### 3.1 The anatomy

A table shell has, top to bottom:

1. The **running header** (study, "Page x of y", "Confidential") -- a
   report spec `header` row, study-wide.
2. The **titles** -- already in the TOC; the shell confirms them.
3. The **column header**: the arms ("Placebo", "Drug X 10 mg"), often a
   `Total`, often `(N=xx)` under each, sometimes a spanner over several
   columns ("Drug X" over "10 mg" and "20 mg"), sometimes a label over
   the stub ("Parameter", "Characteristic").
4. The **stub** (the first column): variable headings ("Age (years)",
   "Sex, n (%)"), statistic rows under a continuous variable ("n", "Mean
   (SD)", "Median", "Min, Max"), levels under a categorical one ("Male",
   "Female"), indented one step; groups of rows (a SOC with its PTs).
5. The **cells**: `xx`, `xx.x`, `xx (xx.x%)`, `xx.x (xx.xx)`, `xx.x, xx.x`,
   `x.xxx` -- the pattern says the template and the decimals.
6. The **footnotes**, and sometimes a note on paging ("one page per
   parameter", "continued").

A listing shell has a column header and one row per record.  A figure
shell is a sketch: axes, groups, a legend, sometimes a number-at-risk
table below.

### 3.2 Shell → the `tables` row

One row, `output_id` the table's.

- `cols`: the variable whose values are the columns -- the arm.  Its
  name is not in the shell; take the dataset's treatment variable
  (`TRT01A` on ADSL; `TRTA` on most event datasets, but check: the
  pilot data's ADAE has only `TRT01A`; §4.1) and say so.  The arms'
  order is the shell's: a `variables` row for the arm with `levels`
  (`Placebo | Drug X 10 mg | Drug X 20 mg`), else they come out
  alphabetically.  Two header rows
  of keys ("Drug X" over doses, or arms over `SEX`) are two keys,
  outermost first: `TRTA | SEX`.
- `rows`: the keys down the stub.  A demographics table has one row key
  (the variable): write `group = variable` (the ARD's variable column),
  as the examples do, and leave `label` blank (the ARD's own `.label`).
  A hierarchy (SOC, then PT) is `rows` `group1 = AEBODSYS` with `label`
  blank: after `normalize_ard()` the PT is the `.label`, not an
  `AEDECOD` column.
- `total`: the heading the shell gives the Total column ("Total", "All
  Subjects"); `total_position` `first` when it is the first column.  Its
  numbers come from the analysis's `overall` (§4.5); no `total` when the
  shell has no such column.
- `sort`: the shell's row order when it is not the data's.  "SOCs
  alphabetically, PTs by decreasing frequency in the total of the arms"
  is `group1 | .depth | -.sort_stat | label` with `sort_stat` `n`: the
  total goes into a column `.sort_stat`, which the `columns` sheet hides
  (`column` `.sort_stat`, `hide` `TRUE`).  A fixed order of levels is not
  a sort; it is `variables$levels` / the code lists (§3.7).
- `stats`, `value`, `sep`, `na`, `header_n`: leave blank unless the shell
  demands it (`na`: what an empty cell shows, "-" or "0 (0.0%)" -- the
  latter is a cells row with `when: n == 0`, not `na`).

### 3.3 Stub rows → `variables` and `cells`

Each variable heading in the stub is a `variables` row: `variable` (the
ADaM name, §4.2), `label` (the heading, character for character:
"Age (years)", "Sex, n (%)"), `order` (its place from the top: 1, 2, 3).

The rows under a heading are templates on the `cells` sheet.  Write
them **once per kind**, not once per variable, when every continuous
variable shows the same rows -- `variable` `continuous`:

| Shell row | `row` | `template` | `digits` |
|---|---|---|---|
| n | `n` | `{N}` | `0` |
| Mean (SD) | `Mean (SD)` | `{mean} ({sd})` | `1,2` (mean one decimal, SD two) |
| Median | `Median` | `{median}` | `1` |
| Min, Max | `Min, Max` | `{min}, {max}` | `0` |
| Q1, Q3 | `Q1, Q3` | `{p25}, {p75}` | `1` |

and for the categorical ones, one row with `variable` `categorical`,
`row` blank (one row per level, labelled by the level) and `template`
`{n} ({p:.1f%})` or `{n:.0f} ({p:.1f%})`.

- `row` is the shell's statistic label, exactly ("Mean (SD)", not "Mean
  (SD) " or "Mean(SD)").
- **Decimals** come from the `xx.x` pattern: `xx` is 0, `xx.x` is 1,
  `xx.xx` is 2.  Put them in the template (`{mean:.1f}`) or in `digits`
  (one number per token, a comma between).  A variable shown with other
  decimals than the rest ("Weight (kg)" to two) is a `digits` sheet row
  for that variable and statistic, or its own `cells` row with
  `variable` the name.
- A percent `xx.x%` is `{p:.1f%}`; with no `%` sign in the shell, `{p:.1f}`.
- The statistic names are the ARD's: `N`, `mean`, `sd`, `median`, `min`,
  `max`, `p25`, `p75`, `n`, `p`, `N_miss` ... (`tfl_ard_statistics()`).
  "n" in a continuous block is `{N}` (the non-missing count); "n (%)" in
  a categorical block is `{n}` and `{p}`.
- A shell row the ARD cannot fill from the method's statistics ("Geo.
  mean", "CV%") needs that statistic in `analyses$statistics` (§4.6) --
  and a question if the shell is unclear.
- `when`: a row shown only when something holds ("0" instead of "0
  (0.0%)" when n is 0): two rows of the same `row`, the guarded one first
  (`when: n == 0`, template `{n}`), then the general one.

### 3.4 Column header → `col_header`

`col_header` rows draw the header cells; `line` 1 is the top.  The usual
two-line header -- the arm, then `(N=xx)` -- is:

| line | cols | span | text |
|---|---|---|---|
| 1 | `row_label` | | (blank, or the stub's heading) |
| 1 | `.values` | `each` | `{col}` |
| 2 | `row_label` | | `Parameter` |
| 2 | `.values` | `each` | `(N={n})` |

- `{col}` is the column's key value (the arm's name as the data have
  it); `{n}` the number of subjects in the analysis set for that column,
  read from the ARD.  `(N=xx)` in the shell is `(N={n})`; the shell's
  `xx` is never written.
- A **spanner** ("Drug X" over two doses): a row with `span` the key's
  name (`TRT01P`) and `text` `{col1}`, the doses on the next line as
  `{col2}` -- two column keys in `tables$cols`.
- The stub's heading ("Parameter", "Characteristic", blank) is the
  `row_label` cell's text.  `row_label` is the stub column's default
  name (`layout$stub_name`).
- The Total column takes the same `each` cells; its `{n}` is the overall
  N.
- Leave `align`, `bold`, `border_*` blank unless the shell shows them
  (bold header, a rule under the spanner only).

### 3.5 Indentation, groups, pages → `layout` and `variables$under`

- The stub: `layout` `stub_name` `row_label` and `stub_before` `TRUE` (as
  in the example) fold the row keys into one first column, `row_label`,
  which the `col_header` and `columns` sheets name.  A long PT wraps in a
  narrow stub: give `columns` `rel_width` (`row_label` 3, `.values` 1).
- Levels indented under a heading, PTs under a SOC: the default.  A
  deeper nesting the shell shows ("Asian" with sub-categories under it)
  is `variables$under` (`RACE: Asian`).
- **One page per parameter / visit** ("Table continues for each
  parameter"): `layout` `group_page` `TRUE`, `group_col` the parameter
  variable; `pages_page_by` when the page key is a column of the ARD
  (a period) rather than a row group.
- A blank row between groups: `blank_where` `between_groups`; the
  examples' AE table also sets `blank_first`, `blank_last`.
- Rows per page: `pages_max_rows` when the shell says it (rare); else
  blank.
- A group label continued on the next page ("(Cont.)"):
  `pages_cont_label`.
- Columns that do not fit one page (many visits): `colpages_*`.

### 3.6 Footnotes

Footnotes are the **report spec's** `footnotes` rows (one per line,
`left`), never a table spec column.  Keep the shell's order and wording;
a footnote that holds a token ("Generated on: ...", a program name) is a
`footer` line with `{DATETIME}` / `{PROGRAM}`.  "N = number of subjects
in the analysis set" is a footnote too, not an instruction.

### 3.7 Levels in a fixed order → code lists

A categorical variable whose levels the shell lists in an order that is
not alphabetical ("18-64", ">64"; "Male", "Female") needs that
order written:

- `variables$levels` (`18-64 | >64`) when the shell's words are the
  data's values; or
- `codelists` rows (`variable`, `value`, `label`, `order`) when the shell
  shows labels ("Male") for coded values ("M").  One row per value, per
  report; the program then makes the column a factor in that order with
  the labels as its values, so the ARD holds "Male".

You rarely know the data's values.  Write the shell's labels as
`levels`, say in the assumptions that the data's values are assumed to
be these labels, and ask for the code list.  A level the shell shows
that the data may never have ("Not reported"): `empty_levels` `show`
prints a 0 row; `hide` drops it.

---

## 4. Shells → the ARD spec

The table spec says how to print; the ARD spec says what to compute.
Every number a shell shows must have an analysis that computes it.

### 4.1 `datasets` and `populations`

- `datasets`: one row per ADaM dataset the shells need: `dataset`
  (`ADSL`, `ADAE`, `ADTTE` ...), `level` `ADaM`, `path` the file as the
  study keeps it (`data/adam/adsl.rds` when you are not told; say so).
  Which dataset: demographics and disposition from ADSL; adverse events
  from ADAE; labs / vitals from ADLB / ADVS; time-to-event from ADTTE;
  response from ADRS; exposure from ADEX.  When a TOC gives a `datasets`
  column, use it.
- `populations`: one row per distinct population text of the TOC:
  `population_id` (`SAF`, `ITT`, `FAS`, `PP`, `ENR`), `dataset` `ADSL`,
  `where` the flag (`SAFFL == "Y"`, `ITTFL == "Y"`).  The flag's name is
  a convention, not a fact: list each `population_id → flag` in the
  assumptions.  "All subjects" / "All enrolled" has no flag: `where`
  blank.
- The arm variable: `TRT01A` (actual) or `TRT01P` (planned) on ADSL;
  `TRTA` / `TRTP` on event datasets.  A safety table is usually actual
  treatment, an efficacy table planned; ask when the shell does not say.

### 4.2 One analysis per block of rows

Each `variables` row of the table spec needs an analysis that produces
its statistics for the right `by`.  Group the shell's blocks by method:

| Shell block | `method` | `variables` | `statistics` (when not the default) |
|---|---|---|---|
| n / Mean (SD) / Median / Min, Max | `continuous` | the variables (`AGE \| WEIGHTBL`) | `N \| mean \| sd \| median \| min \| max` (also `p25 \| p75` when shown) |
| levels with n (%) | `categorical` | `SEX \| AGEGR1 \| RACE` | |
| one yes/no row with n (%) ("Any TEAE") | `dichotomous`, or `subjects` on an event dataset | the flag | |
| SOC / PT counted by subject | `hierarchical` | `AEBODSYS \| AEDECOD` | |
| worst grade / severity by subject | `max` | `AESEV` (ordered) | |
| missing counts | `missing` | | |
| number of subjects (N= in the header) | the header's `{n}` reads the arms' N from the ARD; a Total column's N needs a `total_n` analysis (no `by`) | | |
| a p-value row | `ttest` / `wilcox` / `chisq` / `fisher` | the variable; `by` the arm | |
| a mean with a CI | `mean_ci` | | |
| a response rate with a CI | `proportion_ci` | | |
| anything else | `pkg::function` (cards / cardx) or `custom` with `code` | | |

Every row: `output_id`, `analysis_id` (short, unique within the report,
upper case: `AGE`, `SEX`, `AE`, `ANYTEAE`), `method`, `dataset` (or
`data`), `population_id`, `by` (the arm; the same for every analysis
of one table), `variables`.  One analysis may cover several variables of
the same method (`AGE | WEIGHTBL`); it may not mix methods.

### 4.3 Denominators

`n (%)`: the percent is of what?  The shell never says; the footnote
sometimes does ("Percentages are based on the number of subjects in the
analysis set").

- Of the analysis set in each column (the usual case for subject
  counts): `denominator` blank for `hierarchical` / `max` (they take the
  population), `population` for `categorical` / `dichotomous` on an
  event dataset.
- Of the row's subjects: `row`; of the column's records: `column`.
- Of another set (treatment-emergent events over the treated set; a
  parameter's subjects at a visit): an `analysis_data` row that makes
  the denominator (`distinct` `USUBJID`, or `USUBJID | AVISIT`) and
  `denominator` its `data_id`.

Write the denominator you chose in the assumptions, every time.

### 4.4 Adverse events by subject (SOC / PT)

The standard AE table -- each SOC with its PTs, subjects counted once
per SOC and once per PT, `n (%)` of the analysis set:

| column | value |
|---|---|
| `analysis_id` | `AE` |
| `method` | `hierarchical` |
| `dataset` | `ADAE` |
| `population_id` | `SAF` |
| `where` | `TRTEMFL == "Y"` |
| `by` | the arm on ADAE (`TRTA`, or `TRT01A` when ADAE has no `TRTA`) |
| `overall` | `TRUE` (when the shell has a Total column) |
| `variables` | `AEBODSYS \| AEDECOD` |

The table spec's side (the example's T-2): `rows` `group1 = AEBODSYS`,
`label` blank, `sort` `group1 | .depth | -.sort_stat | label` with
`sort_stat` `n` and `.sort_stat` hidden on the `columns` sheet, one
default `cells` row `{n} ({p:.1f%})`, `layout` `blank_where`
`between_groups`, `stub_name` `row_label`, `stub_before` `TRUE`.

**"Subjects with at least one TEAE"**, the row many shells put first:
the analysis can compute it (`args` `over_variables = TRUE`), but the
table prints it only when the report's program names it
(`normalize_ard(overall = "Subjects with at least one TEAE")`), and no
sheet column says that yet.  Leave `over_variables` out, and put the
row in the assumptions ("the any-TEAE row needs a line in the program").

Variants: serious AEs (`where` `TRTEMFL == "Y" & AESER == "Y"`), related
AEs (`AREL`), AEs leading to discontinuation (`AEACN`), by severity
(`max` on `AESEV`).  Event counts instead of subject counts
(`cards::ard_stack_hierarchical_count`) when the shell says "events".

### 4.5 `by`, Total, and repeats

- `by` is the arm.  A table with no columns but one ("All subjects"): no
  `by`, `cols` blank.
- The Total column: `overall` `TRUE` on each analysis whose rows show it
  -- the analysis run again without its `by`.  Never a `Total` value in
  the data.
- A table repeated within a parameter, a visit, a subgroup (one block
  per `PARAM`, "by visit"): `strata` (`PARAMCD | AVISIT`), and the table
  spec's `rows` / `layout` show the strata as groups or pages.
- A column key of two levels (arm × sex): `by` `TRTA | SEX`.

### 4.6 Statistics and formats

- `statistics`: only when the shell shows other statistics than the
  method's default, or fewer; in the shell's order.  `continuous`
  defaults to the usual set; `categorical` to `n`, `N`, `p`.
- `formats`: the ARD's own text (`mean=xx.x`) is for `tables$value =
  stat_fmt`; with the default `stat`, the decimals are the table spec's
  (§3.3).  Do not write both.
- `label`: the analysis's name for a person ("Age (years)"), optional.
- `purpose` / `reason`: CDISC ARS only; blank unless asked.

---

## 5. A listing shell → the listing spec

`listings`, one row: `output_id`, `type` `multiline`, `dataset` (ADAE for
an AE listing, ADSL for a disposition listing, ADCM ...), `where` (the
records: `AESER == "Y"` for a serious AE listing), `sort` (the shell's
order, usually subject then date: `USUBJID | ASTDY`; `-` before a
variable for descending), `max_rows` only when the shell says it.

`listing_cols`, one row per printed column, in the shell's order:
`vars` (the variable; two stacked in one column -- "Subject / Age" on
two lines -- are `USUBJID | AGE`), `label` (the column header, exactly;
a line break in the header is written as the two characters backslash
and `n`), `width` (characters; take it from the shell's relative column
widths when they are drawn, else leave blank), `collapse_repeats` `TRUE`
on the subject column when the shell prints the subject once for several
rows.

The variable names: `USUBJID`, the arm (`TRTA`, or `TRT01A`), `AEBODSYS`, `AEDECOD`,
`AESTDTC` / `ASTDY`, `AESEV`, `AESER`, `AEREL`, `AEOUT`, `AEACN` for an AE
listing.  Say which you assumed for each shell column.  The population
of a listing is the `where` on the dataset's flag (`SAFFL == "Y"`) or the
TOC's population text in the title; write the flag in `where` and ask.

---

## 6. A figure shell → a figure design (YAML)

Start from a template (`tfl_fig_templates()`; the manual §9) that matches
the shell, and fill its fields from the shell.  The usual shells:

| Shell | `template` | What to read off the shell |
|---|---|---|
| Kaplan-Meier curves, number at risk below | `km_risk_table` (no risk table: `km_simple`) | the time axis's unit and label; the groups (the legend); `PARAMCD` from the title ("Time to First Dermatologic Event" → ask for the code); censor marks; a median line |
| mean over visits with error bars | `mean_se` / `mean_sd` / `mean_ci` | the parameter; the visits (the axis); SE vs SD vs CI from the title or the legend |
| boxplots by arm | `box` | the parameter, the visit |
| waterfall of best change | `waterfall` | the parameter; reference lines (20 %, -30 %) |
| swimmer | `swimmer` | the events in the legend |
| forest plot of subgroups | `forest_hr` / `forest_or` | the subgroups listed; the comparison arm |

The design's fields: `data` steps (`read` the dataset, `param` the
`PARAMCD`, `flag` the population flag, `survfit` for KM), `plot`
(`x_label`, `y_label`, `colour_by` the arm, `legend`), `layers`.  Axis
labels are the shell's, exactly.  The time unit: a KM shell in months
over an `AVAL` in days needs a `time_unit` step; say which you assumed.
A figure that prints a table's numbers (a median, a hazard ratio) reads
that table's ARD (`ard_number`); name the table in the assumptions.

The figure's titles and footnotes are the report spec's, as for a
table; `report$type` is `figure`.

---

## 7. The report spec

From the TOC (§2.4): `report`, `titles`, `footnotes` per output.  From
the shells' page: the study-wide `header` and `footer` (`output_id`
blank), `page` (landscape / portrait, the font when the shell names it,
`A4` / `letter` -- ask when not shown).  `tokens` only for a text the
shells repeat as a placeholder (`{STUDY}`).  Everything else blank: the
defaults are rtfreporter's clinical layout.

---

## 8. The checklist, before answering

Run through it; the review will.

1. Every output of the TOC has a `report` row, every `report` row an
   output of the TOC; the ids match character for character.
2. Every title and footnote is the TOC's / shell's text, in its order;
   the population is the last title line.
3. Every table's `tables` row names its `cols`; every variable heading
   of the shell is a `variables` row; every statistic row of the shell
   has a template that an analysis can fill.
4. Every `variables` row has an analysis of the same report that
   analyses it (or groups by it); every analysis is used by a table.
5. Every `n (%)` has a denominator you can name.
6. Every `population_id` an analysis names is in `populations`; every
   `dataset` in `datasets`.
7. The decimals of every template are the shell's.
8. Levels in a fixed order are written (`levels` or `codelists`).
9. Sheet names and column headers are the blank workbook's; nothing
   added but `note` and `_` sheets; every cell is text; `|` between
   several values; `TRUE` / `FALSE` in capitals.
10. Everything you decided without being told is in the assumptions.

---

## 9. The answer

- **The files**: one `.xlsx` holding the ARD, table, report and listing
  sheets (`06_example_specs.xlsx` is the model), one `.yml` per figure.
  Name them after the study (`XYZ-001_specs.xlsx`, `XYZ-001_F-1.yml`).
- **`_assumptions`**: a sheet of the workbook (its name starts with `_`,
  so no reader takes it as part of the spec) with the columns `output_id`
  (blank for the study), `sheet`, `item`, `assumption`, `question`,
  one row per decision:

  | output_id | sheet | item | assumption | question |
  |---|---|---|---|---|
  | | populations | SAF | SAFFL == "Y" | Is the safety set flagged by SAFFL? |
  | T-2 | analyses | AE denominator | the analysis set per column (SAF) | Percentages of the analysis set, or of subjects with any AE? |
  | T-1 | codelists | SEX | the data code sex as M / F | Please give the code list of SEX. |

  A figure's `.yml` carries the same list under a top-level
  `assumptions:` key (a list of strings), as the manual's drafting
  answers do.
- **In the chat**, after the files: the same list as text, so the person
  can answer without opening the workbook.  Nothing else: no summary of
  the sheets, no code.
- **Do not**: invent a column or a sheet; write a value you were not
  given ("N=86"); translate a title; fill a `program` or `file` the TOC
  does not give; drop a footnote; put the population in a title column;
  write a `Total` level into the data; leave a guess out of the
  assumptions.

---

## 10. Worked example: `05_example_toc_shell.xlsx` → `06` / `07`

A made-up study, XYZ-001, four outputs, on the CDISC pilot data
(pharmaverseadam: arms Placebo, Xanomeline Low Dose, Xanomeline High
Dose).  Open the files beside this guide; this section says what was
read where.  The specs run as they are: the ARD, the two tables, the
listing and the figure are made from them in tflspec's own tests.

### The TOC sheet

Row 1 is a title ("XYZ-001 -- Table of contents, v0.3"); the header is
row 2: `Shell Number`, `Phase`, `Title1`, `Title2`, `Population`,
`Footnote1`, `Footnote2`.  Headings ("14.1 Demographic and baseline
characteristics") sit on rows with no shell number.

| Shell Number | Phase | Title1 | Title2 | Population | Footnote1 | Footnote2 |
|---|---|---|---|---|---|---|
| | | 14.1 Demographic and baseline characteristics | | | | |
| T-1 | | Table 14.1.1 | Demographic Characteristics | Safety Analysis Set | | |
| | | 14.2 Vital signs | | | | |
| F-1 | | Figure 14.2.1 | Mean (SE) Systolic Blood Pressure by Visit | Safety Analysis Set | | |
| | | 14.3 Adverse events | | | | |
| T-2 | | Table 14.3.1 | Treatment-Emergent Adverse Events by System Organ Class and Preferred Term | Safety Analysis Set | A subject is counted once in a system organ class and once in a preferred term. | Coded with MedDRA. |
| | | 16.2 Subject data listings | | | | |
| L-1 | | Listing 16.2.7 | Serious Adverse Events | Safety Analysis Set | | |

Read as `tfl_read_toc(path, sheet = "TOC", skip = 1, map = list(output_id =
"Shell Number", title = c("Title1", "Title2"), population = "Population",
footnote = c("Footnote1", "Footnote2")))`; the kind from the id.  The
`Phase` column is empty: nothing to do.  "Safety Analysis Set" → `SAF`,
`SAFFL == "Y"` (an assumption).

→ report spec: `report` four rows (`table`, `table`, `listing`,
`figure`); `titles` three lines each, `center` (Title1, Title2, the
population); `footnotes` T-2's two lines, `left`; study-wide `header`
line 1 (`left` "XYZ-001", `right` "Page {PAGE} of {TOTAL_PAGES}") and
`footer` line 1 (`left` "{PROGRAM}", `right` "Generated on:
{DATETIME}"), as the shells' page shows.

### Shell T-1

```
                  Placebo       Xanomeline    Xanomeline    Total
                                Low Dose      High Dose
Parameter         (N=xx)        (N=xx)        (N=xx)        (N=xx)
Age (years)
  n               xx            xx            xx            xx
  Mean (SD)       xx.x (xx.xx)  ...
  Median          xx.x
  Min, Max        xx, xx
Sex, n (%)
  Male            xx (xx.x%)
  Female          xx (xx.x%)
Age group, n (%)
  18-64           xx (xx.x%)
  >64             xx (xx.x%)
```

→ `tables`: `cols` `TRT01A`, `rows` `group = variable`, `total` `Total`.
→ `variables`: `TRT01A` with `levels` `Placebo | Xanomeline Low Dose |
Xanomeline High Dose` (the arms in the shell's order); `AGE` "Age
(years)" 1; `SEX` "Sex, n (%)" 2; `AGEGR1` "Age group, n (%)" 3 with
`levels` `18-64 | >64`.
→ `codelists`: `SEX` `M` → Male (order 1), `F` → Female (2): the shell
shows words, the data codes; the codes are an assumption.
→ `cells`: the four `continuous` rows of §3.3 (`{N}` 0; `{mean} ({sd})`
`1,2`; `{median}` 1; `{min}, {max}` 0) and one `categorical` row
`{n} ({p:.1f%})`.
→ `layout`: `blank_where` `between_groups`, `stub_name` `row_label`,
`stub_before` `TRUE`.
→ `col_header`: the four rows of §3.4, `Parameter` on line 2.
→ `analyses`: `N` `total_n` ADSL SAF (the Total column's N); `AGE`
`continuous` ADSL SAF `by` TRT01A `overall` TRUE `variables` AGE; `CAT`
`categorical` ADSL SAF `by` TRT01A `overall` TRUE `variables`
`SEX | AGEGR1`.

### Shell T-2

```
                                Placebo      Xanomeline   Xanomeline
System Organ Class                           Low Dose     High Dose
  Preferred Term                (N=xx)       (N=xx)       (N=xx)
SYSTEM ORGAN CLASS 1
  Preferred term 1              xx (xx.x%)
  Preferred term 2              xx (xx.x%)

SYSTEM ORGAN CLASS 2
  Preferred term 1              xx (xx.x%)
```

No Total column; SOCs alphabetically, PTs by decreasing frequency in
the total of the arms within a SOC; the SOC rows carry no numbers.
→ `tables`: `cols` `TRT01A`, `rows` `group1 = AEBODSYS`, `label` blank,
`sort` `group1 | .depth | -.sort_stat | label`, `sort_stat` `n`.
→ `variables`: `TRT01A` `levels` as T-1.
→ `cells`: one row, `{n} ({p:.1f%})`.
→ `layout`: as T-1.  `columns`: `row_label` `rel_width` 3, `.values` 1,
`.sort_stat` `hide` TRUE.
→ `col_header`: line 1 `row_label` "System Organ Class", `.values` `each`
`{col}`; line 2 `row_label` "  Preferred Term", `.values` `each`
`(N={n})`.
→ `analyses`: `AE` `hierarchical` ADAE SAF `where` `TRTEMFL == "Y"` `by`
TRT01A `variables` `AEBODSYS | AEDECOD`.
Assumptions: the denominator is the analysis set per column; TEAE is
`TRTEMFL`; the arm on ADAE is `TRT01A` (the pilot ADAE has no `TRTA`).

### Shell L-1

```
Subject       Treatment   System Organ Class /   Start   Severity   Outcome
                          Preferred Term         Day
xxxx-xx-xxxx  xxxxxxxx    XXXXXXXX /             xx      xxxxxx     xxxxxxxx
                          xxxxxxxx
```

"Sorted by subject and start day; the subject printed once."
→ `listings`: `type` `multiline`, `dataset` ADAE, `where` `SAFFL == "Y" &
AESER == "Y"`, `sort` `USUBJID | ASTDY`.
→ `listing_cols`, in order: `USUBJID` "Subject" `collapse_repeats` TRUE;
`TRT01A` "Treatment"; `AEBODSYS | AEDECOD` "System Organ Class /" +
backslash-n + "Preferred Term", `sep` `" / "`; `ASTDY` "Start" +
backslash-n + "Day"; `AESEV` "Severity"; `AEOUT` "Outcome".
Assumptions: the variables behind each column; serious is `AESER`;
`SAFFL` is on ADAE.

### Shell F-1

A description, not a picture: a line per arm through the mean at each
visit, an error bar of one standard error, the arms side by side; x:
Visit in visit order; y: "Mean (SE) Systolic Blood Pressure (mmHg)";
the legend below.
→ `07_example_figure.yml`: `template` `mean_se`; `data`: `read` ADVS,
`param` `SYSBP` (assumed: a question), `flag` `SAFFL`, `filter` the
non-missing values, `levels` `AVISIT` by `AVISITN`, `summary` `sm` (`AVAL`
by `TRTA`, `AVISITN`, `AVISIT`, interval `se`); `plot`: `x_label`
"Visit", `y_label` "Mean (SE) Systolic Blood Pressure (mmHg)",
`colour_by` TRTA, `legend` `bottom`; `layers`: `line`, `point`,
`errorbar`; then `assumptions:` (the parameter code; AVAL rather than
CHG).  The template's `join` step is gone: ADVS has `TRTA` and `SAFFL`
itself.

### The `_assumptions` sheet of `06`

Every "assumed" above, one row each, with its question -- the model for
yours.
