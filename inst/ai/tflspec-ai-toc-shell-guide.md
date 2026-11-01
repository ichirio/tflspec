# tflspec: from a TOC and shells to a study's spec files -- a guide for the assistant

> **Scope.** Read this after `tflspec-ai-user-manual.md` (the manual).  The
> manual says what each file of a study's spec holds (its §5) and what
> each function's arguments mean.  This guide says how to get them out of
> what a company actually hands over: a table of contents (TOC) and mock
> tables (shells), each in its own layout.  Nothing here adds a key or a
> function the manual and `03_spec_schema.yml` do not have; when they seem
> to differ, they are right.

A study's outputs come as two documents.  The **TOC** lists them: an id, a
title, a population, footnotes.  A **shell** is one output drawn empty:
the column header, the rows, `xx (xx.x%)` where numbers will go.  The job
is to write, from those, the YAML files tflspec reads -- the study's data
catalog, its list of reports, and one file a report holding its ARD
analyses, its content and its page -- so that the programs tflspec writes
make exactly the tables the shells show.

Two rules above all:

1. **Copy, do not compose.**  A title is the TOC's title, character for
   character.  A row label is the shell's.  A footnote is the shell's or
   the TOC's.  Never improve the wording.
2. **Everything you guessed goes in `spec/_assumptions.yml`** (§9).  A
   shell never says which dataset a row comes from, what a percent is of,
   or which test a p-value is from.  You will choose; say that you did,
   and ask.

---

## 1. What the kit holds, and what to return

The folder the user attached (`tflspec_ai_manual(dir)`) is flat:

| File | What it is |
|---|---|
| `00_START_HERE.md` | the request, and this table |
| `01_tflspec-ai-user-manual.md` | the manual: the files, their keys, the functions |
| `02_tflspec-ai-toc-shell-guide.md` | this guide |
| `03_spec_schema.yml` | every function a `content:` item may call: its arguments and the type of each value |
| `04_blank_spec.md` | every file of a study, blank, its keys said in comments -- in the form of the answer |
| `05_example_toc_shell.xlsx` | a made-up TOC and four shells (§10) |
| `06_example_spec.md` | the answer written from them: the model for yours |
| `07_adam_metadata.csv` | when given: the study's ADaM metadata, one row a variable (`dataset`, `dataset_label`, `variable`, `label`, `kind`, `levels` with ` \| ` between them, `codelist`, `source`) |

**When `07_adam_metadata.csv` is there, every dataset, variable and level
you write must be one of its rows.**  A shell row with no variable there
is an item of `_assumptions.yml` (its `item` the shell row, its
`question` which variable it is), not a guess.  Without it, name datasets
and variables after the ADaM IG (`ADSL`, `TRT01A`, `SAFFL`, ...), and make
each of them an item of `_assumptions.yml`.  The user gives you the TOC
and the shells (one file or several), and may give the SAP's sections on
analysis sets and methods, company standards and a previous study's
specs; use them, and never ask for patient-level data.

**Return one Markdown text holding every file** (§9): a heading with the
file's path, then a `yaml` block -- `study.yml`, `spec/data.yml`,
`spec/toc.yml`, `spec/defaults.yml`, one `spec/outputs/<id>.yml` a
report, `spec/_assumptions.yml`.  The user saves it and
`tfl_ai_unpack()` writes the files.

How to write a value: YAML for what YAML can say -- a list `[TRT01A,
SEX]`, a map `{AGE: Age (years)}`, a number, `true` / `false`; `!r` only
for R (a condition `!r SAFFL == "Y"`, arguments `!r over_variables =
TRUE`).  A content item is `- <function>: {<argument>: <value>, ...}`;
its keys are the function's arguments as `03_spec_schema.yml` lists them,
and nothing else.

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
  first row whose cells read like column names.
- **Blocks and headings.**  Rows with no id and one text ("14.1
  Demographics") are section headings; they are not outputs, but they
  give the outputs below them their `section`.  Blank rows separate
  blocks.
- **Merged cells** repeat a value downwards: a section or a population
  written once for several rows.  Fill it down.

### 2.2 Which column is what

The names below are common; match them ignoring case and blanks, and say
in the assumptions which column you took for each field when the name
was not obvious.

| Field | Column names seen | Notes |
|---|---|---|
| output id | Shell Number, Output No., TFL ID, Table No., Number, ID | required; must be unique; letters, digits, `-` `_` `.` only (the report's file is named after it) |
| kind | Type, Kind, Output Type, Category | Table / Listing / Figure in any spelling; missing: from the id (`T-`, `L-`, `F-`) or the first title ("Table 14.1.1 ...") |
| title | Title, Title1, Title2, Title3, Title Line 1 ... | one column or several, in order; a cell with line breaks or `" \| "` is several lines |
| population | Population, Analysis Set, Analysis Population | the last title line; also the report's analysis set (§4.1) |
| footnote | Footnote, Footnotes, Footnote1 ..., Notes | one or several; blank is fine |
| program | Program, Program Name, Macro | optional |
| section | Section, Domain, Category | optional; else the heading row above |
| datasets | Dataset(s), Source, Input Data | optional; "ADSL, ADAE" |
| label | Shell, Reference, Display No. | the id as printed ("Table 14.1.1"); optional |

What there is no place for: a phase column, a topline / priority flag, a
SAP section, a shell reference, a status.  Do not invent keys for them;
mention them in the assumptions only when they change what you write
(§2.3).

### 2.3 The columns that need a decision

- **Phase / period columns** ("Phase", "Ph1 / Ph2", "Part A / B", a column
  per phase with Y / N).  One row may mean one output per phase (two
  reports, two ids) or one output with both.  Write one report per id
  the TOC gives; if the TOC gives one id for two phases, write one
  report and ask.
- **Title1 / Title2 / Title3.**  Title1 is usually the id as printed
  ("Table 14.1.1"), Title2 the title, Title3 a qualifier.  Keep them as
  separate title lines in that order, the population after them.
- **Population text** ("Safety Analysis Set", "Safety Population", "ITT",
  "All Randomized Subjects").  It names the analysis set.  Map each
  distinct text to one population `id` of `spec/data.yml` (§4.1), the
  same id for the same text everywhere.
- **Footnotes in the TOC and in the shell.**  The shell's footnotes are
  the output's; the TOC's are usually the same or a subset.  When they
  differ, take the shell's and list the difference as a question.
- **"Shell number" vs "output number".**  Some TOCs give a shell
  reference ("DM-01") beside the output id: several outputs may share one
  shell (the same layout for two populations).  The report's `id` is the
  output's id; the shell reference is an assumption's `item`, if anything.
- **TOC and shells in one workbook.**  A shell sheet is often named by the
  id; a shell with no TOC row, or a TOC row with no shell, is a question,
  not an error you fix silently.

### 2.4 What to write from the TOC

- **`spec/toc.yml`**: `outputs:`, one entry a report in the TOC's order:
  `id`, `kind` (`table` / `listing` / `figure`), `label` (the title, the
  line that names the output -- Title2 in the usual layout), `section`
  (the heading's number, `'14.1'`, quoted), `population` (the population
  `id`).
- **Each report file's `page:`**: `titles:` by line number, the TOC's
  title lines then the population, each `{center: ...}` (`left` when the
  shell shows them left-aligned); `footnotes:` by line number, each
  `{left: ...}`.  Line numbers are quoted keys: `'1':`, `'2':`.
- **`spec/defaults.yml`'s `page:`** for what every output shares: the
  running `header:` and `footer:` lines the shells show at the top and
  bottom of the page (`{PAGE}`, `{TOTAL_PAGES}`, `{PROGRAM}`,
  `{DATETIME}` where the shell shows "Page x of y", a program name, a run
  date), and the page itself when the shells say it (`rtf_page:
  {orientation: landscape}`).
- `study.yml`'s `settings:` `program` / `file` only when the TOC names the
  programs or the files by a rule (`{output_id}.R`); else leave them out.

---

## 3. Reading a shell

### 3.1 The anatomy

A table shell has, top to bottom:

1. The **running header** (study, "Page x of y", "Confidential") --
   `defaults.yml`'s `page.header`.
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

A table's report file is then: `data: {source: ard, object: ard}` (it
reads the study ARD), its `ard:` (§4), its `content:` -- the items below,
in this order -- and its `page:`.

### 3.2 The roles → `table_plan`

- `cols`: the variable whose values are the columns -- the arm.  Its
  name is not in the shell; take the dataset's treatment variable
  (`TRT01A` on ADSL; `TRTA` on most event datasets, but check: the
  sample data's ADAE has only `TRT01A`; §4.1) and say so.  Two header
  rows of keys ("Drug X" over doses, or arms over sex) are two keys,
  outermost first: `cols: [TRTA, SEX]`.
- `rows`: the keys down the stub.  A demographics table has one row key,
  the variable: `rows: {group: variable}`.  A hierarchy (SOC, then PT) is
  `rows: {group1: AEBODSYS}`: after `normalize_ard` the PT is the row's
  label, not an `AEDECOD` column.
- The arms' order is the shell's: `plan_levels: {TRT01A: [Placebo, Drug X
  10 mg, Drug X 20 mg]}`, else they come out alphabetically.
- The Total column: `plan_total: {label: Total}` with the heading the
  shell gives ("Total", "All Subjects"); `position: first` when it is the
  first column.  Its numbers come from the analysis's `overall` (§4.5);
  no `plan_total` when the shell has no such column.
- The row order, when it is not the data's: `plan_sort`.  "SOCs
  alphabetically, PTs by decreasing frequency in the total of the arms"
  is `plan_sort: {keys: [.overall, group1, .depth, -.sort_stat, label],
  stat: n}` (`.overall` first keeps an any-event row on top): the total
  goes into a column `.sort_stat`, which `plan_hide: {cols: [.sort_stat]}`
  keeps from print.  A fixed order of levels is not a sort; it is
  `plan_levels` / the code lists (§3.7).
- `plan_cells`' `stats`, `value`, `na`: leave out unless the shell demands
  it (`na`: what an empty cell shows, "-"; "0" instead of "0 (0.0%)" is a
  guard, §3.3).

### 3.3 Stub rows → `plan_labels` and `plan_cells`

Each variable heading in the stub is an entry of `plan_labels`, **in the
shell's order from the top**: `plan_labels: {AGE: Age (years), SEX: Sex, n
(%), AGEGR1: Age group, n (%)}` -- the heading character for character
(quote it when it holds `:` or starts with a symbol).

The rows under a heading are templates of `plan_cells`.  Write them
**once per kind**, not once per variable, when every continuous variable
shows the same rows -- the key `continuous`, one entry a printed row,
named by the shell's label:

```yaml
- plan_cells:
    continuous:
      n: '{N:.0f}'
      Mean (SD): '{mean:.1f} ({sd:.2f})'
      Median: '{median:.1f}'
      Min, Max: '{min:.0f}, {max:.0f}'
      Q1, Q3: '{p25:.1f}, {p75:.1f}'
    categorical: '{n} ({p:.1f%})'
```

`categorical` is one template: one row per level, labelled by the level.

- A row's name is the shell's statistic label, exactly ("Mean (SD)", not
  "Mean (SD) " or "Mean(SD)").
- **Decimals** come from the `xx.x` pattern: `xx` is 0, `xx.x` is 1,
  `xx.xx` is 2.  Put them in the template (`{mean:.1f}`), or say them once
  with `plan_digits: {continuous: {mean: 1, sd: 2}}` and write the tokens
  bare (`{mean} ({sd})`).  A variable shown with other decimals than the
  rest ("Weight (kg)" to two) is its own `plan_digits` entry
  (`WEIGHTBL: {mean: 2}`) or its own `plan_cells` entry by its name.
- A percent `xx.x%` is `{p:.1f%}`; with no `%` sign in the shell, `{p:.1f}`.
- The statistic names are the ARD's: `N`, `mean`, `sd`, `median`, `min`,
  `max`, `p25`, `p75`, `n`, `p`, `N_miss` ... (`tfl_ard_statistics()`).
  "n" in a continuous block is `{N}` (the non-missing count); "n (%)" in
  a categorical block is `{n}` and `{p}`.
- A shell row the ARD cannot fill from the method's statistics ("Geo.
  mean", "CV%") needs that statistic in the analysis's `statistics` (§4.6)
  -- and a question if the shell is unclear.
- **Guards**: a row shown differently when something holds ("0" instead
  of "0 (0.0%)" when n is 0) is a list, the guards first, the template
  last: `categorical: [!r "n == 0 ~ '0'", '{n} ({p:.1f%})']`.

### 3.4 Column header → `plan_col_header`

`plan_col_header: {header: [...]}` draws the header, one entry a cell;
`line` 1 is the top.  The usual two-line header -- the arm, then
`(N=xx)` -- is:

```yaml
- plan_col_header:
    header:
      - {line: '1', cols: row_label}
      - {line: '1', cols: .values, span: each, text: '{col}'}
      - {line: '2', cols: row_label, text: Parameter}
      - {line: '2', cols: .values, span: each, text: '(N={n})'}
```

- `{col}` is the column's key value (the arm's name as the data have
  it); `{n}` the number of subjects in the analysis set for that column,
  read from the ARD.  `(N=xx)` in the shell is `(N={n})`; the shell's
  `xx` is never written, nor a number you were told.
- A **spanner** ("Drug X" over two doses): a cell with `span` the key's
  name (`TRT01P`) and `text` `{col1}`, the doses on the next line as
  `{col2}` -- two column keys in `table_plan`'s `cols`.
- The stub's heading ("Parameter", "Characteristic", blank) is the
  `row_label` cell's text.  `row_label` is the stub column's name
  (`plan_stub`, §3.5).
- The Total column takes the same `each` cells; its `{n}` is the overall
  N.
- Leave `align`, `bold`, `border_top`, `border_bottom` out unless the
  shell shows them (bold header, a rule under the spanner only).

### 3.5 Indentation, groups, pages

- The stub: `plan_stub: {name: row_label, before: true}` (as in the
  example) folds the row keys into one first column, `row_label`, which
  `plan_col_header` and `plan_columns` name.  A long PT wraps in a narrow
  stub: `plan_columns: {widths: {row_label: 3, .values: 1}}`.
- Levels indented under a heading, PTs under a SOC: the default.  A
  deeper nesting the shell shows (sub-categories under "Asian") is
  `plan_nest: {RACEGR: {RACE: Asian}}`.
- **One page per parameter / visit** ("Table continues for each
  parameter"): `plan_paginate_group: {col: PARAM}`;
  `plan_paginate_rows: {page_by: [APERIODC]}` when the page key is a
  column of the ARD (a period) rather than a row group.
- A blank row between groups: `plan_blanks: {where: between_groups}`
  (`first: true`, `last: true` add one at the top and bottom of every page).
- Rows per page: `plan_paginate_rows: {max_rows: 40}` when the shell says
  it (rare).
- A group label continued on the next page ("(Cont.)"):
  `plan_paginate_rows: {cont_label: (Cont.)}`.
- Columns that do not fit one page (many visits): `plan_paginate_cols`.

What every table of the study shares (the stub, the blank rows, the
rounding, `normalize_ard`) can go once in `spec/defaults.yml`'s
`content:`; a report's own item of the same function replaces it.

### 3.6 Footnotes

Footnotes are the report's `page.footnotes` (one per line, `left`),
never a content item.  Keep the shell's order and wording; a footnote
that holds a token ("Generated on: ...", a program name) is a `footer`
line with `{DATETIME}` / `{PROGRAM}`.  "N = number of subjects in the
analysis set" is a footnote too, not an instruction.

### 3.7 Levels in a fixed order → code lists

A categorical variable whose levels the shell lists in an order that is
not alphabetical ("18-64", ">64"; "Male", "Female") needs that order
written:

- `plan_levels: {AGEGR1: ['18-64', '>64']}` when the shell's words are the
  data's values; or
- the report's `codelists:` when the shell shows labels ("Male") for
  coded values ("M"): `codelists: {SEX: [{value: M, label: Male}, {value:
  F, label: Female}]}`, in the order printed.  The program then makes the
  column a factor in that order with the labels as its values, so the ARD
  holds "Male" -- and `plan_levels` names the labels.

You rarely know the data's values.  Write the shell's labels, say in the
assumptions which values you assumed, and ask for the code list.  A
level the shell shows that the data may never have ("Not reported")
prints a 0 row by default; `plan_levels: {.drop_empty: [RACE]}` drops it.

---

## 4. Shells → the ARD: `spec/data.yml` and each report's `ard:`

The content says how to print; the ARD says what to compute.  Every
number a shell shows must have an analysis that computes it.

### 4.1 `datasets` and `populations` (`spec/data.yml`)

- `datasets:` one entry per ADaM dataset the shells need: `dataset`
  (`ADSL`, `ADAE`, `ADTTE` ...), `level: ADaM`, `path` the file as the
  study keeps it (`data/adam/adsl.rds` when you are not told; say so).
  Which dataset: demographics and disposition from ADSL; adverse events
  from ADAE; labs / vitals from ADLB / ADVS; time-to-event from ADTTE;
  response from ADRS; exposure from ADEX.  When a TOC gives a datasets
  column, use it.
- `populations:` one entry per distinct population text of the TOC: `id`
  (`SAF`, `ITT`, `FAS`, `PP`, `ENR`), `dataset: ADSL`, `where` the flag
  (`!r SAFFL == "Y"`), `label` the TOC's text.  The flag's name is a
  convention, not a fact: list each `id → flag` in the assumptions.  "All
  subjects" / "All enrolled" has no flag: no `where`.
- The arm variable: `TRT01A` (actual) or `TRT01P` (planned) on ADSL;
  `TRTA` / `TRTP` on event datasets.  The default follows the analysis
  set: the safety set (`SAFFL`; adverse events, exposure, safety labs and
  vitals) takes the actual treatment, `TRT01A`; every other set (ITT, FAS,
  PP; demographics, disposition, efficacy) the planned treatment,
  `TRT01P`.  The shell or the TOC saying otherwise wins.  Write the choice
  as an assumption (`item: arm`): the set it follows from in
  `assumption`, the variable in `value`.

### 4.2 One analysis per block of rows

A report's analyses are its file's `ard: {analyses: [...]}`.  Each
variable of `plan_labels` needs an analysis that produces its statistics
for the right `by`.  Group the shell's blocks by method:

| Shell block | `method` | `variables` | `statistics` (when not the default) |
|---|---|---|---|
| n / Mean (SD) / Median / Min, Max | `continuous` | the variables (`[AGE, WEIGHTBL]`) | `[N, mean, sd, median, min, max]` (also `p25`, `p75` when shown) |
| levels with n (%) | `categorical` | `[SEX, AGEGR1, RACE]` | |
| one yes/no row with n (%) ("Any TEAE") | `dichotomous`, or `subjects` on an event dataset | the flag | |
| SOC / PT counted by subject | `hierarchical` | `[AEBODSYS, AEDECOD]` | |
| worst grade / severity by subject | `max` | `[AESEV]` (ordered) | |
| missing counts | `missing` | | |
| number of subjects (N= in the header) | the header's `{n}` reads the arms' N from the ARD; a Total column's N needs a `total_n` analysis (no `by`) | | |
| a p-value row | `ttest` / `wilcox` / `chisq` / `fisher` | the variable; `by` the arm | |
| a mean with a CI | `mean_ci` | | |
| a response rate with a CI | `proportion_ci` | | |
| anything else | `pkg::function` (cards / cardx) or `custom` with `code` | | |

Every analysis: `id` (short, unique within the report, upper case: `AGE`,
`SEX`, `AE`, `ANYTEAE`), `label` (its name for a person), `method`,
`dataset` (or `data`, an analysis data of the report's `ard:
{analysis_data: [...]}`), `population` (a population `id`), `by` (the
arm; the same for every analysis of one table), `variables`.  One
analysis may cover several variables of the same method (`[AGE,
WEIGHTBL]`); it may not mix methods.

```yaml
ard:
  analyses:
    - id: AGE
      label: Age (years)
      method: continuous
      dataset: ADSL
      population: SAF
      by: [TRT01A]
      overall: true
      variables: [AGE]
```

### 4.3 Denominators

`n (%)`: the percent is of what?  The shell never says; the footnote
sometimes does ("Percentages are based on the number of subjects in the
analysis set").

- Of the analysis set in each column (the usual case for subject
  counts): no `denominator` for `hierarchical` / `max` (they take the
  population), `denominator: population` for `categorical` /
  `dichotomous` on an event dataset.
- Of the row's subjects: `row`; of the column's records: `column`.
- Of another set (treatment-emergent events over the treated set; a
  parameter's subjects at a visit): an entry of `ard.analysis_data` that
  makes the denominator (`distinct: [USUBJID]`, or `[USUBJID, AVISIT]`)
  and `denominator` its `id`.

Write the denominator you chose in the assumptions, every time.

### 4.4 Adverse events by subject (SOC / PT)

The standard AE table -- each SOC with its PTs, subjects counted once
per SOC and once per PT, `n (%)` of the analysis set:

```yaml
ard:
  analyses:
    - id: AE
      method: hierarchical
      dataset: ADAE
      population: SAF
      where: !r TRTEMFL == "Y"
      by: [TRT01A]           # TRTA, or TRT01A when ADAE has no TRTA
      overall: true          # only when the shell has a Total column
      variables: [AEBODSYS, AEDECOD]
      args: !r over_variables = TRUE   # the "Subjects with at least one TEAE" row
```

The content's side (the example's T-2): a `normalize_ard` item first,
with the hierarchy and that row's text exactly as the shell has it --
`normalize_ard: {hierarchy: [AEBODSYS, AEDECOD], overall: Subjects with
at least one TEAE}` -- then `table_plan: {cols: TRT01A, rows: {group1:
AEBODSYS}}`, the `plan_sort` and `plan_hide` of §3.2, one default
template `plan_cells: {default: '{n} ({p:.1f%})'}`, `plan_blanks: {where:
between_groups}`, `plan_stub: {name: row_label, before: true}`.  The SOC
rows carry their own counts.  No any-event row in the shell: leave
`over_variables` and `overall` out.

Variants: serious AEs (`where: !r TRTEMFL == "Y" & AESER == "Y"`), related
AEs (`AREL`), AEs leading to discontinuation (`AEACN`), by severity
(`max` on `AESEV`).  Event counts instead of subject counts
(`cards::ard_stack_hierarchical_count`) when the shell says "events".

### 4.5 `by`, Total, and repeats

- `by` is the arm.  A table with no columns but one ("All subjects"): no
  `by`, no `cols`.
- The Total column: `overall: true` on each analysis whose rows show it
  -- the analysis run again without its `by`.  Never a `Total` value in
  the data.
- A table repeated within a parameter, a visit, a subgroup (one block per
  `PARAM`, "by visit"): `strata: [PARAMCD, AVISIT]`, and the content's
  `table_plan` rows / `plan_paginate_group` show the strata as groups or
  pages.
- A column key of two levels (arm × sex): `by: [TRTA, SEX]`.

### 4.6 Statistics and formats

- `statistics`: only when the shell shows other statistics than the
  method's default, or fewer; in the shell's order.  `continuous`
  defaults to the usual set; `categorical` to `n`, `N`, `p`.
- `formats`: the ARD's own text (`mean=xx.x`) is for `plan_cells: {value:
  stat_fmt}`; with the default, the decimals are the content's (§3.3).
  Do not write both.
- `purpose` / `reason`: CDISC ARS only; leave out unless asked.

---

## 5. A listing shell → its report file

`data:` says the records: `source: dataset`, `dataset` (ADAE for an AE
listing, ADSL for a disposition listing, ADCM ...), `where` (`!r AESER ==
"Y"` for a serious AE listing), `sort` (the shell's order, usually
subject then date: `[USUBJID, ASTDY]`; `-` before a variable for
descending).

`content:` is one `plan_listing` item, its `cols` one `listing_col` call
per printed column, in the shell's order:

```yaml
content:
  - plan_listing:
      type: multiline
      cols:
        - {fn: listing_col, args: {vars: [USUBJID], label: Subject, collapse_repeats: true}}
        - {fn: listing_col, args: {vars: [AEBODSYS, AEDECOD], label: 'System Organ Class /\nPreferred Term', sep: '" / "'}}
```

`vars` is the variable; two stacked in one column ("Subject / Age" on two
lines) are `[USUBJID, AGE]`.  `label` is the column header, exactly; a
line break in it is the two characters backslash and `n`.  `width`
(characters) only when the shell's column widths are drawn;
`collapse_repeats: true` on the subject column when the shell prints the
subject once for several rows.  Rows per page:
`plan_paginate_rows: {max_rows: ...}` only when the shell says it.

The variable names: `USUBJID`, the arm (`TRTA`, or `TRT01A`), `AEBODSYS`,
`AEDECOD`, `AESTDTC` / `ASTDY`, `AESEV`, `AESER`, `AEREL`, `AEOUT`,
`AEACN` for an AE listing.  Say which you assumed for each shell column.
The population of a listing is the `where` on the dataset's flag
(`SAFFL == "Y"`) or the TOC's population text in the title; write the
flag in `where` and ask.

---

## 6. A figure shell → its report file

A figure's file holds its data steps and its plot as function calls (the
manual §10.1): `template` (the one it started from), `data: {source:
code, steps: [...]}`, and `content:` -- the function that makes the plot
first, then its layers, then its settings, `tfl_image` last.  Start from
the template (`tfl_fig_templates()`) that matches the shell; the usual
shells:

| Shell | `template` | The plot's items | What to read off the shell |
|---|---|---|---|
| Kaplan-Meier curves, number at risk below | `km_risk_table` (no risk table: `km_simple`) | `ggsurvfit`, `add_censor_mark`, `geom_hline` (the median line), the axes, `labs`; the number at risk a plot of its own, `tfl_risk_panel`, under it (`wrap_plots`) -- or `add_risktable` on the curves | the time axis's unit and label; the groups (the legend); `PARAMCD` from the title ("Time to First Dermatologic Event" → ask for the code); censor marks; a median line |
| mean over visits with error bars | `mean_se` / `mean_sd` / `mean_ci` | `ggplot`, `geom_line`, `geom_point`, `geom_errorbar` (dodged) | the parameter; the visits (the axis); SE vs SD vs CI from the title or the legend |
| boxplots by arm | `box_by_visit` / `box_by_group` / `box_change` | `ggplot`, `geom_boxplot` | the parameter, the visit |
| waterfall of best change | `waterfall_response` / `waterfall_plain` | `ggplot`, `geom_col`, `geom_hline` (20 %, -30 %) | the parameter; the reference lines |
| swimmer | `swimmer_*` | `ggplot`, `geom_segment`, `geom_point` | the events in the legend |
| forest plot of subgroups | `forest_hr` / `forest_or` | `ggplot`, `geom_vline`, `geom_errorbar`, `geom_point`, `scale_x_log10`; the text at its right a plot of its own, `tfl_text_column` (`wrap_plots`) | the subgroups listed; the comparison arm |

- A request that answers with templates (`tfl_templates()`, not the
  designer's presets above) has two for a Kaplan-Meier shell:
  `km_survival` (the survival, a median line) and `km_cumulative` (the
  cumulative percentage, CI bars -- FDA STF Figures 1, 4 and 13, whose
  report definitions are in `extdata/report-definitions/`).
- The data steps: `read` the dataset, `param` the `PARAMCD`, `flag` the
  population flag, `filter`, `levels`, `summary` / `survfit` (an object
  of its own by `name`: `sm`, `fit`).  The time unit: a KM shell in
  months over an `AVAL` in days needs a `time_unit` step; say which you
  assumed.
- The axes' titles are `labs: {x: ..., 'y': ...}`, the shell's text
  exactly; the legend `theme: {legend.position: bottom}`; the arms'
  colours `scale_colour_tfl: {palette: treatment}`, the theme `theme_tfl:
  {}` (the company standard).  The axes' ranges and breaks are
  `scale_x_continuous` / `scale_y_continuous` and `coord_cartesian`.
- `mapping:` holds columns only (`colour: TRTA`); a fixed colour or size is
  the function's own argument (`colour: grey50`), never in `mapping:`.
- A figure that prints a table's numbers (a median, a hazard ratio) reads
  that table's ARD: `tfl_ard_number` (`analysis_id`, `variable`, `level`,
  `stat`, `group`, `label: "Median: {value}"`); name the table in the
  assumptions.  A figure's own analyses (a forest plot's models) are its
  `ard:`, as a table's.
- Write `y` as `'y'` (quoted): YAML reads a bare y as true.

The figure's titles and footnotes are its `page:`, as a table's.

---

## 7. The page

From the TOC (§2.4): each report's `page.titles` and `page.footnotes`.
From the shells' page, once in `spec/defaults.yml`'s `page:`: the
`header` and `footer` lines, the page (`rtf_page: {orientation:
landscape, paper_size: letter}` -- ask when not shown), the font when the
shell names it.  `tokens:` only for a text the shells repeat as a
placeholder (`{STUDY}`).  Everything else left out: the defaults are
rtfreporter's clinical layout.

---

## 8. The checklist, before answering

Run through it; the review will.

1. Every output of the TOC is an entry of `spec/toc.yml` and has its file
   `spec/outputs/<id>.yml`; every file is an output of the TOC; the ids
   match character for character, in the file's name and its `id`.
2. Every title and footnote is the TOC's / shell's text, in its order;
   the population is the last title line.
3. Every table's `table_plan` names its `cols`; every variable heading of
   the shell is an entry of `plan_labels`, in the shell's order; every
   statistic row of the shell has a template that an analysis can fill.
4. Every variable the content prints has an analysis in the same report's
   `ard:` that analyses it (or groups by it); every analysis is printed by
   its report.
5. Every `n (%)` has a denominator you can name.
6. Every `population` an analysis names is a population `id` of
   `spec/data.yml`; every `dataset` one of its `datasets`.
7. The decimals of every template are the shell's.
8. Levels in a fixed order are written (`plan_levels` or `codelists`).
   With `07_adam_metadata.csv`, every dataset, variable and level is one
   of its rows (in its `levels`), or an item of `_assumptions.yml`.
9. Every content item is a function of `03_spec_schema.yml`, its keys
   that function's arguments; lists and maps written as YAML, `!r` only
   for R; nothing outside `study.yml` and `spec/`.
10. Everything you decided without being told is in the assumptions.

---

## 9. The answer

- **One Markdown text, every file in it**: for each file a heading with
  its path, then a fenced `yaml` block holding the whole file --

  ````text
  ### spec/outputs/T-1.yml

  ```yaml
  id: T-1
  kind: table
  ...
  ```
  ````

  `06_example_spec.md` is the model: `study.yml`, `spec/data.yml`,
  `spec/defaults.yml`, `spec/toc.yml`, then the reports, then
  `spec/_assumptions.yml`.  A path is `study.yml` or under `spec/`, ends
  in `.yml`, and comes once.  A chat that can attach a file returns this
  text as one `.md` file.
- **`spec/_assumptions.yml`** (its name starts with `_`, so no reader
  takes it as part of the spec): a list, one item a decision --
  `output_id` (left out for the study), `where` (the file or key it is
  about), `item`, `assumption`, `value`, `question`.  `assumption` is
  **one sentence a reader understands on its own: what you decided, and
  how** -- not a label (`arm: TRT01A` says nothing; `Treatment variable
  for the columns: TRT01A (actual), because the analysis set is the safety
  set (SAF)` does).  `value` is **the value you used** (the variable, the
  flag, the list, the text), so a screen can show it beside the sentence.

  **What goes in, and what does not.**  An assumption is a choice that
  **changes the numbers or how they are read**: the analysis set and its
  flag, the treatment variable (`TRT01A` / `TRT01P`), a denominator, how
  missing values are handled, the unit counted (subjects or events), a
  stratum's or subgroup's condition, a code list and its order, the
  parameter behind a shell row, the visit a column stands for.  **Not an
  assumption** is how the output looks: the title's lines, the columns'
  order and widths, indentation, the default decimals, blank rows, where a
  footnote sits, the order of the statistics -- decide those by the
  company or study standard and rtfreporter's defaults, write them, and
  say nothing.  Three that look like one and are not: the statistics'
  order (the standard's); a title split in two lines (the shell's look);
  the width of the label column (rtfreporter's default).  Three that are:
  the denominator of a percentage; which flag marks treatment-emergent;
  whether a subject with two events counts once.  A condition that
  **defines** the report (the analysis set, the parameter of a PARAMCD
  dataset) and is missing is neither: ask it as a question and leave the
  report without it (§4.1).

  The example:

  ```yaml
  - where: spec/data.yml populations
    item: SAF
    assumption: "Safety set: the flag SAFFL == \"Y\" on ADSL, because the TOC names the set SAF and no flag"
    value: SAFFL == "Y"
    question: Is the safety set flagged by SAFFL?
  - output_id: T-1
    where: ard.analyses
    item: arm
    assumption: "Treatment variable for the columns: TRT01A (actual), because the analysis set is the safety set (SAF)"
    value: TRT01A
    question: Actual (TRT01A) or planned (TRT01P) treatment?
  - output_id: T-2
    where: ard.analyses
    item: AE denominator
    assumption: "AE denominator: the analysis set of each column (SAF), because the shell does not say"
    value: SAF
    question: Percentages of the analysis set, or of subjects with any AE?
  ```
- **In the chat**, after the files: the same list as text, so the person
  can answer without opening the files.  Nothing else: no summary of the
  files, no code.
- **Do not**: invent a key or a function; write a value you were not
  given ("N=80"); translate a title; set a `program` or `file` the TOC
  does not give; drop a footnote; write a `Total` level into the data;
  write a list or a map as `!r` R code; leave a guess out of the
  assumptions.

---

## 10. Worked example: `05_example_toc_shell.xlsx` → `06_example_spec.md`

A made-up study, XYZ-001, four outputs, on tflspec's oncology sample
data (`adsl`, `adae`, `advs`: arms Drug X and Control).  Open the files beside this guide; this section says what was
read where.  The answer runs as it is: the ARD, the two tables, the
listing and the figure are made from it in tflspec's own tests.

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

The kind from the id.  The `Phase` column is empty: nothing to do.
"Safety Analysis Set" → the population `SAF`, `!r SAFFL == "Y"` (an
assumption).

→ `spec/toc.yml`: four entries in the TOC's order, each `id`, `kind`,
`label` (Title2), `section` (`'14.1'` ...), `population: SAF`.
→ each report's `page.titles`: `'1'` Title1, `'2'` Title2, `'3'` the
population, all `center`; T-2's `page.footnotes` its two lines, `left`.
→ `spec/defaults.yml`'s `page`: `header` line 1 (`left` "XYZ-001",
`right` "Page {PAGE} of {TOTAL_PAGES}") and `footer` line 1 (`left`
"{PROGRAM}", `right` "Generated on: {DATETIME}"), as the shells' page
shows.

### Shell T-1

```
                  Drug X        Control       Total
Parameter         (N=xx)        (N=xx)        (N=xx)
Age (years)
  n               xx            xx            xx
  Mean (SD)       xx.x (xx.xx)  ...
  Median          xx.x
  Min, Max        xx, xx
Sex, n (%)
  Male            xx (xx.x%)
  Female          xx (xx.x%)
Age group, n (%)
  <65             xx (xx.x%)
  >=65            xx (xx.x%)
```

→ `table_plan: {cols: TRT01A, rows: {group: variable}}`;
`plan_total: {label: Total}`.
→ `plan_levels`: `TRT01A` the arms in the shell's order; `AGEGR1`
`['<65', '>=65']`; `SEX` `[Male, Female]`.
→ `plan_labels`: `AGE` "Age (years)", `SEX` "Sex, n (%)", `AGEGR1` "Age
group, n (%)", in that order.
→ `codelists`: `SEX` `M` → Male, `F` → Female: the shell shows words, the
data codes; the codes are an assumption.
→ `plan_cells`: the four `continuous` rows of §3.3 and `categorical`
`'{n} ({p:.1f%})'`.
→ `plan_stub: {name: row_label, before: true}`, `plan_blanks: {where:
between_groups}`.
→ `plan_col_header`: the four cells of §3.4, `Parameter` on line 2.
→ `ard.analyses`: `N` `total_n` ADSL SAF (the Total column's N); `AGE`
`continuous` ADSL SAF `by: [TRT01A]` `overall: true` `variables: [AGE]`;
`CAT` `categorical` ADSL SAF `by: [TRT01A]` `overall: true` `variables:
[SEX, AGEGR1]`.

### Shell T-2

```
System Organ Class              Drug X       Control
  Preferred Term                (N=xx)       (N=xx)
Subjects with at least one TEAE xx (xx.x%)
SYSTEM ORGAN CLASS 1            xx (xx.x%)
  Preferred term 1              xx (xx.x%)
  Preferred term 2              xx (xx.x%)

SYSTEM ORGAN CLASS 2            xx (xx.x%)
  Preferred term 1              xx (xx.x%)
```

No Total column; the any-TEAE row first, SOCs alphabetically, PTs by
decreasing frequency in the total of the arms within a SOC.
→ `normalize_ard: {hierarchy: [AEBODSYS, AEDECOD], overall: Subjects
with at least one TEAE}`.
→ `table_plan: {cols: TRT01A, rows: {group1: AEBODSYS}}`; `plan_sort:
{keys: [.overall, group1, .depth, -.sort_stat, label], stat: n}`;
`plan_hide: {cols: .sort_stat}`.
→ `plan_levels`: `TRT01A` as T-1.  `plan_cells: {default: '{n}
({p:.1f%})'}`.
→ `plan_stub`, `plan_blanks` as T-1; `plan_columns: {widths: {row_label:
3, .values: 1}}`.
→ `plan_col_header`: line 1 `row_label` "System Organ Class", `.values`
`each` `{col}`; line 2 `row_label` "Preferred Term", `.values` `each`
`(N={n})`.
→ `ard.analyses`: `AE` `hierarchical` ADAE SAF `where: !r TRTEMFL == "Y"`
`by: [TRT01A]` `variables: [AEBODSYS, AEDECOD]` `args: !r over_variables
= TRUE`.
Assumptions: the denominator is the analysis set per column; TEAE is
`TRTEMFL`; the arm on ADAE is `TRT01A` (the sample ADAE has no `TRTA`).

### Shell L-1

```
Subject       Treatment   System Organ Class /   Start   Severity   Outcome
                          Preferred Term         Day
xxxx-xx-xxxx  xxxxxxxx    XXXXXXXX /             xx      xxxxxx     xxxxxxxx
                          xxxxxxxx
```

"Sorted by subject and start day; the subject printed once."
→ `data: {source: dataset, dataset: ADAE, where: !r SAFFL == "Y" & AESER
== "Y", sort: [USUBJID, ASTDY]}`.
→ `plan_listing`, `type: multiline`, its columns in order: `USUBJID`
"Subject" `collapse_repeats: true`; `TRT01A` "Treatment"; `[AEBODSYS,
AEDECOD]` "System Organ Class /" + backslash-n + "Preferred Term", `sep`
`'" / "'`; `ASTDY` "Start" + backslash-n + "Day"; `AESEV` "Severity";
`AEOUT` "Outcome".
Assumptions: the variables behind each column; serious is `AESER`;
`SAFFL` is on ADAE.

### Shell F-1

A description, not a picture: a line per arm through the mean at each
visit, an error bar of one standard error, the arms side by side; x:
Visit in visit order, baseline to cycle 9; y: "Mean (SE) Systolic Blood Pressure (mmHg)";
the legend below.
→ `template: mean_se`; `data: {source: code, steps: [...]}`: `read`
ADVS, `param` `SYSBP` (assumed: a question), `flag` `SAFFL`, `filter`
the non-missing values of the shell's visits (`AVISITN <= 8`), `levels`
`AVISIT` by `AVISITN`, `summary` `sm` (`AVAL` by `TRT01A, AVISITN,
AVISIT`, interval `se`).  The template's `join` step is gone: ADVS has
`TRT01A` and `SAFFL` itself.
→ `content`: `ggplot: {}`, then `geom_line`, `geom_point` and
`geom_errorbar` on `sm` (`data: sm`, their `mapping`, dodged by
`position_dodge(width = 0.3)`), `scale_colour_tfl` (the treatment
palette), `labs: {x: Visit, 'y': Mean (SE) Systolic Blood Pressure
(mmHg)}`, `theme_tfl`, `theme: {legend.position: bottom}`, and
`tfl_image: {dpi: 300}`.

### `spec/_assumptions.yml` of `06`

Every "assumed" above, one item each, with its question -- the model for
yours.
