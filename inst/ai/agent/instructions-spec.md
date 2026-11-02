## The skeleton (what a draft holds)

A draft is a report file, `ai/spec/outputs/<id>.yml`, in the form above
(one report; no `tflspec_ai` header, no `reports` list).  It holds the
report's skeleton and nothing of its look:

- `id`, `kind` (`table`, `listing`, `figure`), `label` (the TOC's title;
  when the shell's differs, the TOC's, and an assumption that says so),
  `population` (the analysis set's id from `spec/data.yml`), `extends:
  [defaults]`;
- `data` (`{source: ard}` for a table of statistics; a listing's records as
  the listing section says);
- `ard`: `analysis_data` (the analysis set's subjects, a condition) and
  `analyses` (`method`, `by`, `variables`, `denominator`, `where`);
- the content: `template: {name: <template>, args: {...}}` when one fits
  (only the arguments that differ from the default; the content then holds
  only what the template does not do), else `content:` with `table_plan`
  (`cols`, `rows`, `label`) and `plan_cells` -- and nothing else;
- the digits, only when the shell shows them (`xx.x`): in the cell
  templates (`{mean:.1f} ({sd:.2f})`) or a `plan_digits` item; a shell
  without them takes the study's default -- do not ask, and do not invent
  a rule of your own (one decimal for Min and Max, ...): a rule the shell
  does not show is the study's, not yours.  Digits that differ by
  parameter (a lab table's rows) are a question when the shell does not
  show them per row -- unless `ai/adam_summary.md` lists them from the
  define (`[define.xml: digits 1]` on a variable, or its "Value level"
  section by parameter): those are the study's digits, use them and say
  so in an assumption (`from: define`);
- `page: titles` (the shell's title lines), and the shell's footnotes:
  {{footnotes_at}}
  A footnote is the shell's text as it is: never reworded, none added.  A
  footnote of a statistic the table does not show (`SD = Standard
  deviation.` under an AE table) is dropped, with an assumption that says
  so;
- a cell template is Python's `format()` of a number with tflspec's `%`:
  `.1f%` is the proportion times 100 with one decimal -- **no `%` sign**;
  a sign the shell shows is written after the brace.  The shell's form,
  as the cell template:
  - `xx (xx.x%)`: `"{n} ({p:.1f%}%)"`;
  - `xx (xx.x)`: `"{n} ({p:.1f%})"`;
  - `xx.x` (a percent alone): `"{p:.1f%}"`.
  `{p:.1%}` is not one -- the build stops (`Unknown format spec '.1'`);
- **the shell's cell form against the study's**: `study_facts.md` says
  the study's count cell and how it prints.  When the shell's differs
  (`xx (xx.x%)` where the study prints `xx (xx.x)`), write the shell's
  for this report: a template's `count_cell` argument, or a code SPEC's
  `plan_cells: {categorical: ...}`.  When they agree, write none;
- **the N under the arm**: when the shell's column headers show
  `(N=xx)` under each arm and `study_facts.md` says the study's header
  has no N (and the template's own header does not have it -- `param_change`
  and `shift` have it -- and each arm heads one value column, not a Value
  and a Change column under it), write this
  one item, nothing more of the header: `plan_col_header: {header: [{line:
  "1", cols: .values, span: each, text: "{col}\n(N={n})"}]}`;
- an `ard.analysis_data` that `code` makes may name its `population`: the
  set's subjects are made first and are in reach as `<dataset>_sub`
  (`adsl_sub`); its other fields (`where`, `add`, `derive` ...) stay blank.
- a text holding a comma, a colon or a `#` is quoted: `{left: "MedDRA
  version 27.0, fictional dictionary."}` -- unquoted, the text after the
  comma is lost and the check refuses the draft.

Not in a draft, ever: header rows (`plan_col_header`, but the N under the
arm above), pages (`plan_paginate_*`), blank rows, borders, fonts,
`plan_style`.  The study's defaults and the person's builder do those.
The column widths are in every draft: `plan_columns: {widths: {...}}`
for a table, `width:` for a listing column (ai/AGENTS.md, step 5).
A zero count the SAP or the standard prints as `0`: a guarded list (a
guard sees the cell's statistics), a template's `count_cell: [!r 'n == 0
~ "0"', "{n} ({p:.1f%}%)"]` or a code SPEC's `plan_cells: {categorical:
[!r 'n == 0 ~ "0"', "{n} ({p:.1f%}%)"]}`.
What the study's defaults (`spec/defaults.yml`, the company standard)
already give -- the cell rules, the digits -- is not written again; a
cell form or an N the shell shows otherwise is (above).
Nor what a template does by default: a template argument is written only
when its value differs (no list of every visit, no `plan_levels` of a
variable the table does not show, `plan_empty: {text: ...}` with no other
key).

Under a top-level `ai:` key: `assumptions` (what you took from a document,
`from: shell` / `SAP` / `define`; from a decision, `from: D<n>`; from the
usual convention for this kind of table, `from: "programmer convention"`;
or on your own after three rounds) and `confirm` (what the person decides
in the builder), as ai/AGENTS.md shows.  The order is always: the
documents, else the convention, else a question (ai/AGENTS.md, *Every
open point*).

## A draft, whole: the forms to copy

A table of statistics, as a code SPEC (an AE table by SOC and PT):

```yaml
id: T-14-3-1
kind: table
label: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term
population: SAF
extends: [defaults]
data: {source: ard}
ard:
  analyses:
    - id: AE
      method: hierarchical
      dataset: ADAE
      population: SAF
      where: !r TRTEMFL == "Y"
      by: [TRT01A]
      variables: [AEBODSYS, AEDECOD]
      args: !r over_variables = TRUE
content:
  - normalize_ard: {hierarchy: [AEBODSYS, AEDECOD], overall: Subjects with at least one TEAE}
  - table_plan: {cols: TRT01A, rows: {group1: AEBODSYS}}
  - plan_cells: {categorical: "{n} ({p:.1f%})"}
  - plan_sort: {keys: [.overall, group1, .depth, -n, label]}
page:
  titles:
    "1": {center: Table 14.3.1}
    "2": {center: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term}
    "3": {center: Safety Analysis Set}
  footer:
    "1": {left: "MedDRA version 27.0, fictional dictionary."}
ai:
  assumptions:
    - {where: ard.analyses, item: where, assumption: "treatment-emergent is TRTEMFL == \"Y\"", value: "TRTEMFL == \"Y\"", from: D3}
  confirm:
    - {where: ard.analyses, item: denominator, why: "the shell shows n (%) but not whether the % is of the arm or of all"}
```

The same table with a template (only the arguments that differ from the
template's default; nothing the template does on its own):

```yaml
id: T-14-3-1
kind: table
label: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term
population: SAF
extends: [defaults]
template:
  name: ae_soc_pt
  args: {population: SAF, by: TRT01A}
page:
  titles:
    "1": {center: Table 14.3.1}
    "2": {center: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term}
```

An analysis data made by code (a per-subject worst category), its
population named so that `adsl_sub` is in reach; the analysis then reads
it by `data`, with no `dataset` and no `population` of its own:

```yaml
ard:
  analysis_data:
    - id: lb_worst
      label: one row a subject and parameter, baseline and worst post-baseline category
      from: ADLB
      population: SAF
      code: !r 'post <- dplyr::filter(adlb, USUBJID %in% adsl_sub$USUBJID, ONTRTFL == "Y"); dplyr::summarise(dplyr::group_by(post, USUBJID, TRT01A, PARAMCD), WORST = max(ANRIND), .groups = "drop")'
  analyses:
    - {id: SHIFT, method: categorical, data: lb_worst, by: [TRT01A, PARAMCD], variables: [WORST], denominator: column}
```

A figure from a template with its own data (`km_survival`,
`km_cumulative`): the template by `name` and `args`, and its data as
`steps` under `data` (never a bare list) -- the data stage only: `read`,
the analysis set (`flag`, or a semi `join` on ADSL), `param`.  **Not the
template's own steps**: no `survfit`, no `time_unit` -- the template
makes them, and the check refuses a draft that writes them.  Never drop
`data.steps` either: without them the figure would be of every record.

```yaml
id: F-14-2-2
kind: figure
label: "Kaplan-Meier Plot of Overall Survival"
population: ITT
extends: [defaults]
template:
  name: km_survival
  args: {by: TRT01P, time_unit: months}
data:
  source: code
  steps:
    - {step: read, dataset: ADTTE}
    - {step: flag, variable: ITTFL}
    - {step: param, value: OS}
plot: {x_label: "Time (Months)", y_label: "Survival Probability"}
page:
  titles:
    "1": {center: "Figure 14.2.2"}
    "2": {center: "Kaplan-Meier Plot of Overall Survival"}
```

## The fields, exactly

`ard.analysis_data`, one item each: `id`, `label`, `from` (a dataset of
`spec/data.yml`, or an analysis data above it), `population`, `subjects`,
`where`, `add`, `derive`, `keep`, `distinct`, `code` -- no other key.
`population` and `subjects` are one or the other; with `code`, only `id`,
`label`, `from` and `population`.

`ard.analyses`, one item each: `id`, `method`, then **either** `data`
(an analysis data's id of this report) **or** `dataset` with
`population` -- never both --, `where`, `by`, `variables`, `strata`,
`overall`, `denominator`, `args`, `statistics`, `label`, `parent`,
`records`, `groups`, `optional`, `formats`, `post`, `code`, `purpose`,
`reason` -- no other key.  `method` is one of: continuous, categorical,
dichotomous, missing, hierarchical, max, subjects, total_n,
proportion_ci, mean_ci, ttest, wilcox, chisq, fisher, cmh, prop_diff,
lsmeans, lsmeans_diff, km, logrank, abnormal, incidence_rate, custom (or a
`pkg::function` the instructions list).  `denominator` is `population`,
`row`, `column`, `cell`, or an analysis data's id.

`table_plan`: `cols` names a `by` variable of the analyses (the arms),
`rows` the variables or the groups (`{group1: AEBODSYS}`); the levels of
the `variables` are rows, never columns -- a column by a variable's level
needs that variable in `by`.

## What shells often ask for, and the item that does it

- **The rows ordered by one column's count** (the SOCs and PTs by the
  Drug A 20 mg column, most first, ties alphabetical): `plan_sort: {keys:
  [.overall, "-n@group1[Drug A 20 mg]", group1, .depth, "-n[Drug A 20 mg]",
  label]}` -- `-n@group1[<column>]` orders the SOCs by their own count in
  that column, `group1` right after it keeps each SOC's rows together when
  two SOCs have the same count (without it their rows mix), `-n[<column>]`
  the PTs by theirs; `-n` alone is the count over every column.  `group1`
  alone in place of the `@` key keeps the SOCs alphabetical, whatever the
  PTs are sorted by.
  With the template `ae_soc_pt` / `ae_ser_soc_pt`, the same list as its
  `sort` argument.
- **Digits that differ by parameter** (a lab table: ALT one decimal, BILI
  three): on the `param_change` template, `plan_digits` by the parameter
  as the table holds it -- `PARAM`, the label `ai/adam_summary.md` lists:
  `plan_digits: {PARAM: {"Alanine Aminotransferase (U/L)": {mean: 1, sd:
  2, median: 1, min: 0, max: 0}, "Bilirubin (umol/L)": {mean: 2, sd: 3,
  median: 2, min: 1, max: 1}}}` -- a parameter it leaves out keeps the
  template's (one decimal, the SD two); its cells are written without
  digits, so leave them as they are.  A code SPEC's own cells: guarded
  cell templates, one a parameter, the last one for the
  rest -- `plan_cells: {continuous: [!r 'PARAMCD == "ALT" ~ "{mean:.1f}
  ({sd:.2f})"', !r 'PARAMCD == "BILI" ~ "{mean:.3f} ({sd:.4f})"', "{mean:.2f}
  ({sd:.3f})"]}` (a guard is `<row key> == "<value>" ~ "<template>"`, as
  `!r`).  The same list under one row's label when only that row differs:
  `continuous: {"Mean (SD)": [!r '...', "..."], Median: "{median:.1f}"}`
  (not the label as an entry of its own: `plan_cells: {"Mean (SD)":
  [...]}` is refused).  A guard compares the value as the table has it: a
  key with a code list holds the label (`PARAMCD == "Bilirubin (umol/L)"`,
  not `"BILI"`), and a table by parameter (`param_change`, a shift) holds
  `PARAM`, the label `ai/adam_summary.md` lists -- `PARAM == "Alanine
  Aminotransferase (U/L)"`; a guard on the code (`PARAMCD == "ALT"`)
  matches nothing and every row takes the last template.  `plan_digits`
  by a value gives digits only to a cell written without them (`{mean}
  ({sd})`), as `param_change`'s are; a cell with its own (`{mean:.1f}`)
  keeps them.
- **Every row the shell shows** (Race's Multiple, ECOG 0 to 3): each
  value in `codelists` (value and label, in the shell's order) -- a value
  the data does not have too: it prints as a 0 row.  A value left out of
  the code list has no row.  A variable's heading as the shell's (`Baseline
  ECOG performance status, n (%)`), in the template's `labels` -- without
  it the heading is the variable's name.
- **A title line that changes with the page** ("Parameter: <PARAM>", one
  parameter a page): the templates `param_change` and `shift` do it
  themselves (`param_page`, `param_heading`); else `plan_paginate_rows: {page_by: [PARAMCD], heading:
  "Parameter: {PARAM}", heading_at: title}` -- the heading goes under the
  report's titles on each page; `{PARAM}` is that page's value of the
  column.  `page_by` names a column the table holds, as it holds it: a
  `table_plan` whose `rows: {group1: PARAM}` makes the column `group1`,
  so `page_by: [group1]`, `heading: "Parameter: {value}"` and
  `plan_hide: {cols: [group1]}` (a `page_by: [PARAMCD]` there is not
  found and the build fails).  Group by the label (`PARAM`) when the
  heading shows the label.  Several keys: `page_by: [ANALYTE, RENALGR]` and a heading of two
  lines, `"Analyte: {ANALYTE}
Group: {RENALGR}"`.
- **Every subject** (a report of all subjects, no flag): `population:
  ALL` -- the study's `spec/data.yml` lists it beside the flags' sets.
- **A p-value column** (a test of each variable between the arms, beside
  the counts): one analysis a test, the same `by` and `variables` as the
  counts', and a comparison column that reads its record --
  `analyses: [..., {id: P_SEX, method: fisher, dataset: ADSL, population:
  SAF, by: [TRT01A], variables: [SEX]}, {id: P_AGE, method: kruskal,
  dataset: ADSL, population: SAF, by: [TRT01A], variables: [AGE]}]` and
  `plan_compare: {p: "{p.value:.3f}"}` (the column's name is yours: `p`,
  `pvalue`; `"<0.001"` for small values: `p: [!r 'p.value < 0.001 ~
  "<0.001"', "{p.value:.3f}"]`).  The methods: `fisher`, `chisq`
  (categorical), `ttest` (two arms), `wilcox`, `kruskal`, `oneway`
  (continuous, any number of arms).  The p-value goes on the variable's
  first row; a p-value the shell puts on a row of its own is a question.
- **The arms in their order**: the data's order (TRT01AN beside TRT01A)
  is the program's default; write a code list (`codelists: {TRT01A:
  [...]}`) only when the shell's order or labels differ from the data's.
- **A table with a second block of rows under it** (a listing of the
  values and their summary): `plan_append` with its own `data` and
  `content` (the instructions' item list), after the first table's items.
- **A shift table of counts** (baseline category down, worst post-baseline
  category across): the template `shift` (or `lab_shift` at one visit)
  keeps a subject once a parameter at its worst category, with a Missing
  and a Total row and a page a parameter; write only `where` (the
  parameters, the records after baseline the study counts) and what
  differs.
- **A shift table with percents of the Total row** (the % of the arm's
  subjects with a post-baseline value for the parameter, the Missing row
  among them): the template `shift` with `percent_of: total` and a cell
  with `{p}`; a worst category that differs by parameter (the SAP: low
  first for HGB, NEUT, PLAT) in `worst_by`, by the parameter's value:

  ```yaml
  template:
    name: shift
    args:
      population: SAF
      where: 'PARAMCD %in% c("ALT", "AST", "BILI", "HGB", "NEUT", "PLAT") & ONTRTFL == "Y"'
      percent_of: total
      cell: "{n} ({p:.1f%}%)"
      worst: [HIGH, LOW, NORMAL]
      worst_by: {HGB: [LOW, HIGH, NORMAL], NEUT: [LOW, HIGH, NORMAL], PLAT: [LOW, HIGH, NORMAL]}
      param_labels: {ALT: "Alanine Aminotransferase (U/L)", BILI: "Bilirubin (umol/L)"}
  ```

  A zero count the SAP prints as `0`: the cell a guarded list, `cell:
  [!r 'n == 0 ~ "0"', "{n} ({p:.1f%}%)"]`.

- **A figure's time axis**: its label carries the unit, `Time (Months)`
  (the data's `AVALU`), never `Time` alone.  A template's own argument
  (`censor_marks`, `risk_table`) says what the figure shows -- the look
  the study standard sets (`censor_shape`, the colours, the legend's
  place) is not a `template: args:` item (the build refuses it): leave it
  to the standard, under `ai: confirm` when the shell differs; a `layers:`
  item is for what no argument does -- never both for one thing.
- **A listing**: a listing column's `width:` in the shell's proportions,
  wide enough for its longest value (a subject id on one line) and its
  heading's longest word -- a short column too (`Age/Sex`, `64/M`: about
  10).  The widths are scaled to the page, so their sum fits it (a
  landscape page about 120 characters): the room comes from the long
  text column, not the short ones; a column
  heading's line breaks as the shell's, `"Start Date\n(Study
  Day)"`; a mark after a term with its space, `paste0(AEDECOD, " #")`; a
  missing date blank, `ifelse(is.na(ASTDT), "", format(ASTDT, ...))`.

## Easy mistakes (the check refuses these)

| wrong | right |
|---|---|
| `{left: MedDRA version 27.0, fictional dictionary.}` | `{left: "MedDRA version 27.0, fictional dictionary."}` -- a comma, a colon or `#` in a text: quote it |
| `label: Shift Table of Laboratory Values: Baseline versus Worst` | `label: "Shift Table of Laboratory Values: Baseline versus Worst"` -- `: ` inside a value is a YAML error; quote every free text |
| `template: {name: km_survival, ...}` with `data.steps` holding `survfit`, or with no `data.steps` | the data stage only: `read`, the analysis set, `param` -- the template makes `time_unit` and `survfit` |
| `where: TRTEMFL == "Y"` | `where: !r TRTEMFL == "Y"` -- R code carries `!r` |
| `data: [{step: read, dataset: ADTTE}, ...]` | `data: {source: code, steps: [{step: read, dataset: ADTTE}, ...]}` |
| an analysis with `data: lb_worst` **and** `population: SAF` | one of the two: `data` alone, or `dataset` with `population` |
| an analysis data with `code` **and** `where` / `add` / `derive` | `code` with `id`, `label`, `from`, `population` only |
| `table_plan: {cols: [TRT01A, WORST]}` with `variables: [WORST]` | `cols: TRT01A` -- a variable's levels are rows; to have them across, put the variable in `by` |
| `plan_cells: {categorical: {n} ({p:.1f%})}` | `plan_cells: {categorical: "{n} ({p:.1f%})"}` -- braces in a text: quote it |
| `"{n} ({p:.1%})"` | `"{n} ({p:.1f%}%)"` -- `.1%` is not a tflspec format; `.1f%` prints no sign, so the shell's `%` goes after the brace |
| a key of your own (`notes:`, `comment:`, `denominator_note:`) | none: a key the schema does not have is refused; a note is an `ai: assumptions` or `ai: confirm` item |
| `extends: defaults` | `extends: [defaults]` |
| `population: Safety Analysis Set` | `population: SAF` -- the id from `spec/data.yml` |
| `kind: Table` | `kind: table` |

## Before you save a draft: read it once more

1. Every key is one the schema or this file names; none of your own.
2. Indentation is two spaces a level; a list is `- ` items or `[a, b]`.
3. Every free-text value (`label`, titles, footnotes, assumption text)
   is in double quotes -- always one with `: ` or `#`, one starting with
   `[ { * & ! | > ' " % @`, or yes / no / on / off or a number meant as
   text; R code has `!r`.
4. `id` is the report's id as the request gives it; the file is `<id>.yml`.
5. Each analysis reads `data` or `dataset` + `population`, not both; each
   `data` names an analysis data of this report; each variable is in
   `ai/adam_summary.md` or made by a `code` / `derive` of this report.
6. `table_plan` `cols` is a `by` variable; `rows` are variables or groups.
7. Nothing of the look; the digits only where the shell shows them.
8. `ai:` is the last top-level key: `assumptions` (each with `from` when
   from a decision) and `confirm`.
9. A template with its own data (`km_survival`, `km_cumulative`) has
   `data.steps` of `read`, the analysis set and `param` -- and none of the
   template's own steps (`survfit`, `time_unit`).
