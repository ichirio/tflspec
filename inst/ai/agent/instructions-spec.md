## The skeleton (what a draft holds)

A draft is a report file, `ai/spec/outputs/<id>.yml`, in the form above
(one report; no `tflspec_ai` header, no `reports` list).  It holds the
report's skeleton and nothing of its look:

- `id`, `kind` (`table`, `listing`, `figure`), `label` (the shell's title),
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
  show them per row;
- `page: titles` (the shell's title lines), and the shell's footnotes:
  {{footnotes_at}}
- an `ard.analysis_data` that `code` makes may name its `population`: the
  set's subjects are made first and are in reach as `<dataset>_sub`
  (`adsl_sub`); its other fields (`where`, `add`, `derive` ...) stay blank.
- a text holding a comma, a colon or a `#` is quoted: `{left: "MedDRA
  version 27.0, fictional dictionary."}` -- unquoted, the text after the
  comma is lost and the check refuses the draft.

Not in a draft, ever: column widths, header rows (`plan_col_header`),
pages (`plan_paginate_*`), blank rows, borders, fonts, `plan_style`,
`plan_columns`.  The study's defaults and the person's builder do those.

Under a top-level `ai:` key: `assumptions` (what you took from a decision,
`from: D<n>`, or on your own after three rounds) and `confirm` (what the
person decides in the builder), as ai/AGENTS.md shows.

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

A figure from a template: its data as `steps` under `data` (never a bare
list), the template by name:

```yaml
id: F-14-2-2
kind: figure
label: Kaplan-Meier Plot of Overall Survival
population: ITT
extends: [defaults]
template: km_simple
data:
  source: code
  steps:
    - {step: read, dataset: ADTTE}
    - {step: param, value: OS}
    - {step: flag, variable: ITTFL}
    - {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}
plot: {x_label: Time (Months), y_label: Survival Probability, colour_by: TRT01P}
page:
  titles:
    "1": {center: Figure 14.2.2}
    "2": {center: Kaplan-Meier Plot of Overall Survival}
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
  [.overall, group1, .depth, "-n[Drug A 20 mg]", label]}` -- `-n[<column>]`
  is that column's count; `-n` alone is the count over every column.  With
  the template `ae_soc_pt` / `ae_ser_soc_pt`, the same list as its `sort`
  argument.
- **Digits that differ by parameter** (a lab table: ALT one decimal, BILI
  three): guarded cell templates, one a parameter, the last one for the
  rest -- `plan_cells: {continuous: [!r 'PARAMCD == "ALT" ~ "{mean:.1f}
  ({sd:.2f})"', !r 'PARAMCD == "BILI" ~ "{mean:.3f} ({sd:.4f})"', "{mean:.2f}
  ({sd:.3f})"]}` (a guard is `<row key> == "<value>" ~ "<template>"`, as
  `!r`).  `plan_digits` is by variable, not by a key's value.
- **A title line that changes with the page** ("Parameter: <PARAM>", one
  parameter a page): `plan_paginate_rows: {page_by: [PARAMCD], heading:
  "Parameter: {PARAM}", heading_at: title}` -- the heading goes under the
  report's titles on each page; `{PARAM}` is that page's value of the
  column.  Several keys: `page_by: [ANALYTE, RENALGR]` and a heading of two
  lines, `"Analyte: {ANALYTE}
Group: {RENALGR}"`.
- **A table with a second block of rows under it** (a listing of the
  values and their summary): `plan_append` with its own `data` and
  `content` (the instructions' item list), after the first table's items.
- **A shift table** (baseline category down, worst post-baseline category
  across, a Total row, the % of the arm's subjects with a post-baseline
  value for the parameter): the worst category is a column key of the
  ARD (`strata`), the baseline category the rows' variable (`variables`,
  never `label`), the denominator the subjects a parameter each, and the
  Total row `plan_total` with `direction: rows` from a second analysis of
  the column key by the row keys:

  ```yaml
  ard:
    analysis_data:
      - id: lb_shift
        label: one row a subject and parameter: the baseline category, the worst post-baseline category
        from: ADLB
        population: SAF
        code: !r 'post <- dplyr::filter(adlb, USUBJID %in% adsl_sub$USUBJID, ONTRTFL == "Y", is.na(ABLFL) | ABLFL != "Y", !is.na(ANRIND)); base <- dplyr::distinct(dplyr::filter(adlb, ABLFL == "Y"), USUBJID, PARAMCD, .keep_all = TRUE)[, c("USUBJID", "PARAMCD", "BNRIND")]; worst <- dplyr::summarise(dplyr::group_by(post, USUBJID, TRT01A, PARAMCD), WORST = if (any(ANRIND == "HIGH")) "HIGH" else if (any(ANRIND == "LOW")) "LOW" else "NORMAL", .groups = "drop"); out <- dplyr::left_join(worst, base, by = c("USUBJID", "PARAMCD")); dplyr::mutate(out, BASECAT = dplyr::coalesce(BNRIND, "MISSING"))'
      - id: lb_den
        label: the subjects with a post-baseline value, a parameter each (the denominator)
        from: lb_shift
        keep: USUBJID | TRT01A | PARAMCD
        distinct: USUBJID | PARAMCD
    analyses:
      - {id: SHIFT, method: categorical, data: lb_shift, by: [TRT01A, PARAMCD], strata: [WORST], variables: [BASECAT], denominator: lb_den}
      - {id: TOTAL, method: categorical, data: lb_shift, by: [TRT01A, PARAMCD], variables: [WORST], denominator: lb_den}
  content:
    - normalize_ard: {}
    - table_plan: {cols: [TRT01A, WORST], rows: {group1: PARAMCD}}
    - plan_total: {label: Total, direction: rows, position: last}
    - plan_cells: {categorical: "{n} ({p:.1f%})"}
    - plan_levels: {WORST: [LOW, NORMAL, HIGH], BASECAT: [LOW, NORMAL, HIGH, MISSING]}
  ```

  A `by` or `strata` key that code makes (`WORST`, `BASECAT`) is a column
  of the ARD; a column that is neither a key nor the variable is not, and
  `table_plan` cannot name it.

## Easy mistakes (the check refuses these)

| wrong | right |
|---|---|
| `{left: MedDRA version 27.0, fictional dictionary.}` | `{left: "MedDRA version 27.0, fictional dictionary."}` -- a comma, a colon or `#` in a text: quote it |
| `where: TRTEMFL == "Y"` | `where: !r TRTEMFL == "Y"` -- R code carries `!r` |
| `data: [{step: read, dataset: ADTTE}, ...]` | `data: {source: code, steps: [{step: read, dataset: ADTTE}, ...]}` |
| an analysis with `data: lb_worst` **and** `population: SAF` | one of the two: `data` alone, or `dataset` with `population` |
| an analysis data with `code` **and** `where` / `add` / `derive` | `code` with `id`, `label`, `from`, `population` only |
| `table_plan: {cols: [TRT01A, WORST]}` with `variables: [WORST]` | `cols: TRT01A` -- a variable's levels are rows; to have them across, put the variable in `by` |
| `plan_cells: {categorical: {n} ({p:.1f%})}` | `plan_cells: {categorical: "{n} ({p:.1f%})"}` -- braces in a text: quote it |
| a key of your own (`notes:`, `comment:`, `denominator_note:`) | none: a key the schema does not have is refused; a note is an `ai: assumptions` or `ai: confirm` item |
| `extends: defaults` | `extends: [defaults]` |
| `population: Safety Analysis Set` | `population: SAF` -- the id from `spec/data.yml` |
| `kind: Table` | `kind: table` |

## Before you save a draft: read it once more

1. Every key is one the schema or this file names; none of your own.
2. Indentation is two spaces a level; a list is `- ` items or `[a, b]`.
3. Every text with `,` `:` `#` `{` `}` is in double quotes; R code has `!r`.
4. `id` is the report's id as the request gives it; the file is `<id>.yml`.
5. Each analysis reads `data` or `dataset` + `population`, not both; each
   `data` names an analysis data of this report; each variable is in
   `ai/adam_summary.md` or made by a `code` / `derive` of this report.
6. `table_plan` `cols` is a `by` variable; `rows` are variables or groups.
7. Nothing of the look; the digits only where the shell shows them.
8. `ai:` is the last top-level key: `assumptions` (each with `from` when
   from a decision) and `confirm`.
