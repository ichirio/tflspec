# tflspec — AI user manual

**This manual documents tflspec 0.0.24.9238** (the development version,
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

> **Scope: *using* tflspec** — writing a study's specification, the
> **code spec** (YAML files: one file a report, its `content:` one item a
> function call), and the report programs made from it. The older form of
> the same definition, Excel sheets, is Appendix A. How a table becomes RTF pages is
> rtfreporter's; attach rtfreporter's own manual
> (`rtfreporter::rtfreporter_ai_manual()`) when the task is its code.

---

## 0. Ground rules for the assistant

1. **Only call functions listed in §14**, and rtfreporter's functions from
   rtfreporter's manual. tflspec is young and almost certainly *not* in your
   training data. If a requested feature has no function, say so plainly
   instead of inventing a name or an argument.
2. **tflspec writes specifications and code; it does not draw.** The ARD
   functions (`normalize_ard()`, `widen_ard()` ...), the plan
   (`table_plan()`, the `plan_*()` verbs, `plan_apply()`) and the RTF
   rendering are **rtfreporter's**. A program that runs a table spec starts
   with `library(rtfreporter)`.
3. **Every tflspec function is `tfl_*`** (and `tflspec_ai_manual()`).
   The functions the generated programs call as they run (`set_levels()`,
   `tag_ard()`, `fmt_ard()`, `keep_stats()`, `fmt_pvalue()`, `save_ard()`)
   are not tflspec's exports: `tfl_helpers_code()` gives their code, which a
   study keeps in a file of its own (tflplanner: `programs/study_helpers.R`),
   so the programs run without tflspec.
   `table_plan()`, the `plan_*()` verbs, `plan_apply()`, `normalize_ard()`
   are **rtfreporter's**, never `tfl_`-prefixed; there are no aliases for
   former names (§13).
4. **A study keeps its spec as YAML files** (§5): one file a report
   (`spec/outputs/<id>.yml`), its `content:` one item a function call
   whose keys are that function's arguments (`tfl_schema()`).  An argument
   the function does not have is an error.  A table's items are
   rtfreporter's `normalize_ard`, `table_plan` and `plan_*`; its numbers
   come from its `ard:` analyses (cards / cardx).  This is the **code
   spec**, and the main path; §6 shows a table's file key by key.
5. **`id` (`output_id`) is the key.**  A report file says only what
   differs from what it extends (`extends: [defaults]`): the study's
   `spec/defaults.yml` holds the page and the items every table shares.
6. **Write values as YAML**: a list `[TRT01A, SEX]`, a map
   `{AGE: Age (years)}`, a number, `true` / `false`; a cell template is
   always quoted (`"{n} ({p:.1f%})"`); `!r` marks R code and only where the
   schema's type is `r`.
7. **The same definition as Excel workbooks** (Appendix A, the sheet
   spec) is the older form: workbooks are read with `col_types = "text"`,
   unknown columns are errors (but `note`), sheets whose name starts with
   `_` are notes, and a blank `output_id` row is the study default.  Use
   them when a study is still in workbooks; a study converts once
   (`tfl_convert_sheets_to_yaml()`).
8. **Drafting from a TOC and shells with an AI chat** (§9): tflspec makes
   the prompt and reads, checks and writes the answer; it sends nothing.
9. Indices are 1-based (ordinary R).

---

## 1. The one workflow

```text
TOC / SHELL ──(AI chat: the table task, §9.1)──> spec/outputs/<id>.yml
ADaM ──(each report's ard:)──> study ARD ──(content:)──> plan ──(page:)──> RTF
         tfl_build_ard(study)          tfl_content_run()        tfl_report()
                                       tfl_content_code()       tfl_report_path()
```

A study folder (§5) holds the code spec, a report a file:
`tfl_build_ard(study)` makes the study ARD from every report's `ard:`,
`tfl_content_code(report)` / `tfl_content_run(report, data)` write / run
its content (the table's plan), and `tfl_report(report, content, study = )`
its document.  Each step can be **run** or **written out as a program**
from the same definition, so the object and the program cannot disagree.
The written program is what a study keeps: it needs rtfreporter (and
cards), not tflspec.

Listings (`plan_listing` items, §11) and figures (`ggplot`, `geom_*` ...
items, §7) are report files too.  The sheet spec (Appendix A) runs the same
way: `tfl_ard_code()` / `tfl_build_ard()`, `tfl_table_code()` /
`tfl_table_plan()`, `tfl_report_code()` / `tfl_report()`.

---

## 2. Copy-paste starter (complete, runnable)

```r
library(rtfreporter)
library(tflspec)

# a study folder: the ADaM (the sample data, as XPT), study.yml,
# spec/data.yml (datasets, analysis sets) and spec/defaults.yml
d <- file.path(tempdir(), "XYZ-ONC-01")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
for (x in c("adsl", "adae", "adrs", "adlb")) {
  df <- getExportedValue("tflspec", x)
  for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
}
tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01", overwrite = TRUE)

# the report files: the AI kit's sample answer (four tables)
tfl_ai_unpack(system.file("ai", "example", "onco-answer.md", package = "tflspec"),
              d, overwrite = TRUE)

# the ARD, then each table: its plan and its RTF
st   <- tfl_read_study_spec(d)
outs <- tfl_read_outputs(d)
ard  <- tfl_build_ard(st, dir = d, save = FALSE)
for (id in names(outs)) {
  o    <- tfl_resolve_output(outs[[id]], st)
  plan <- tfl_content_run(o, subset(ard, output_id == id))
  generate_rtfreport(tfl_report(o, content = plan, study = st),
                     file.path(d, paste0(id, ".rtf")), overwrite = TRUE)
}
cat(tfl_content_code(tfl_resolve_output(outs[["T-14-1-1"]], st)), sep = "\n")
```

---

## 3. Program structure

A table program written from the specs has three parts:

```r
library(rtfreporter)
# 1 DATA    the ARD (this table's rows of the study ARD)
ard <- readRDS("output/ard/ard.rds")
# 2 TABLE   the table spec as plan verbs      (tfl_table_code())
plan <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01P", rows = c(group = "variable")) |>
  plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
  plan_stub(name = "row_label", before = TRUE)
# 3 REPORT  the report spec as the document  (tfl_report_code())
doc <- rtf_document()
doc <- rtf_tables(doc, plan)
generate_rtfreport(doc, "output/DM.rtf", overwrite = TRUE)
```

The ARD comes from the ARD spec (§A.1): `tfl_ard_code(spec, output_id)`
writes the cards code; `tfl_build_ard()` runs it and saves one study ARD;
`tfl_ard_for(ard, output_id)` takes one output's rows.
`tfl_write_ard(ard, "ard.json")` writes a copy as JSON / YAML (one record
per statistic, each variable's levels in order; `tfl_read_ard()` reads it
back) or XPT (flat; factors, formats and warnings are lost, with a
warning); the rds stays the record. An ARD made elsewhere: `report$ard_source`
`import:<file>` (the file in the study's `input/ard/`), checked with
`tfl_check_ard(ard, spec)` (its shape, and what the table reads).
`tfl_ard_conditions(ard)` lists what went wrong while an ARD was made (a
test that could not run, a statistic that stopped or warned): cards keeps
going and leaves the message in the `error` / `warning` column.

---
## 4. Function map — what to call for what

| I want to … | Call |
|---|---|
| read a study folder (YAML); a report over its layers | `tfl_read_study_spec(dir)`, `tfl_read_outputs(dir)`; `tfl_resolve_output(report, study)` (§5) |
| a report file's content as code / run it | `tfl_content_code(report)` / `tfl_content_run(report, data)`; an item's arguments: `tfl_schema(fn)`, `tfl_schema_check(item)`, `tfl_schema_help(fn, arg)` |
| the study folder as workbooks and back; workbooks to a folder once | `tfl_sheets_from_yaml(dir)` (`tfl_sheets_from_spec(study, outputs)` from a spec in memory), `tfl_write_spec_xlsx(dir, path)` / `tfl_import_spec_xlsx(dir, path)`; `tfl_convert_sheets_to_yaml(from, to)` |
| start an ARD spec / read / write it | `tfl_ard_spec_template()` / `tfl_read_ard_spec()` / `tfl_write_ard_spec()` |
| write the cards code of one output; each analysis's data by the program's name | `tfl_ard_code(spec, output_id)`; `tfl_ard_data_names(spec, output_id)` |
| run the ARD spec, one study ARD | `tfl_build_ard(spec)`; one output's rows: `tfl_ard_for(ard, output_id)` |
| scaffold a table spec from an ARD | `tfl_table_spec_template(ard)` |
| read / write a table (+ report) spec | `tfl_read_table_spec()` / `tfl_read_report_spec()` / `tfl_write_table_spec()` / `tfl_write_report_spec()` |
| several specs in one workbook; what a column means | `tfl_write_specs(path, ard, table, report, listing)`; `tfl_spec_columns(sheet)` |
| the specs as CDISC ARS (JSON), and its check | `tfl_ars(ard_spec, table_spec, report_spec)` (a figure printing a table's analyses: `references = data.frame(output_id, source, analysis_id)`) → `tfl_write_ars_json(ars, path)`; `tfl_check_ars(ars)`; what ARS cannot say: `tfl_ars_unmapped(ars)`; CDISC's Excel template to read: `tfl_write_ars_xlsx(ars, path)` (§A.1.1) |
| ARS siera can run, and its ARD (round trip) | `tfl_ars(ard_spec, profile = "siera")` → `tfl_ars_ard(ars, adam)` (siera writes and runs one programme per output) |
| an ARS JSON (anyone's) back as specs | `tfl_read_ars_json(path)` → `tfl_ars_to_specs(ars, table = FALSE)`: `$ard`, `$report` (`$table`: the groupings' levels); fill `datasets$path`; what has no place: `attr(, "unmapped")` |
| the plan a table spec stands for | `tfl_table_plan(data, spec)` (then any rtfreporter verb: last wins) |
| the plan as code | `tfl_table_code(spec)` |
| a plan written in code, back to a workbook | `tfl_as_table_spec(plan)` |
| a listing written in code (`listing_spec()` / `plan_listing()`), back to a listing spec | `tfl_as_listing_spec(x, output_id, dataset = )` |
| the document / its code / its file | `tfl_report(spec, output_id, content = plan)` / `tfl_report_code(spec)` / `tfl_report_path(spec)` |
| a listing's program / its pages | `tfl_listing_code(spec)` / `tfl_listing(data, spec)` |
| a figure: start / check / write the script / its size on the page | `tfl_fig_template()` / `tfl_check_fig_design()` / `tfl_fig_design_code()` / `tfl_fig_page_size()` |
| a figure report's content of function calls; a design as one | `tfl_content_code(report)` / `tfl_content_run(report, data)` (§7.1); `tfl_fig_content_from_design(design)`; a named plot `tfl_plot(name, items)` |
| an item's arguments as R text, and back (the builder's R form) | `tfl_args_to_r(fn, args)` / `tfl_args_from_r(fn, text)` |
| review the whole definition: errors, what is probably wrong, what to set by hand | `tfl_review_spec(spec, ard, listings, figures)`; against the data: `facts = tfl_data_facts(data, populations = ard)`; the rules: `tfl_review_rules()`; each row's sentence is `template` filled with `args` (`message` is the two put together; the sentences beyond the rules': `tfl_review_templates()`) |
| attach this manual to a chat session | `tflspec_ai_manual()` (its path) |
| draft a study's specs from its TOC and shells in an AI chat | `tflspec_ai_manual(dir)` writes the kit to attach; `tfl_ai_unpack(answer, dir)` writes the answer into a study folder; `tfl_ai_review(dir, toc = )` checks it (§9.3) |
| draft tables' report files (the code spec) from a TOC / SHELL workbook with an AI chat | `tfl_ai_study_init()`, `tfl_ai_adam_summary()`, `tfl_ai_read_shell()`, `tfl_ai_batches()` → `tfl_ai_context("table", ...)` → `tfl_ai_prompt("table", ctx)` → the chat → `tfl_ai_parse()` → `tfl_ai_check()` → `tfl_ai_unpack()` (§9.1) |
| draft a TOC or a figure design with an AI chat | `tfl_ai_context()` → `tfl_ai_prompt()` → the chat → `tfl_ai_parse()` → `tfl_ai_check()` (problems: `tfl_ai_repair_prompt()`) → `tfl_ai_diff()` (§9.2) |

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
## 5. The study folder — the YAML spec

A study's spec is a folder of YAML files, one file a report.  It holds
what the workbooks of Appendix A hold, in the form a study keeps (tflplanner
saves it; the workbooks are written from it and read back into it:
`tfl_write_spec_xlsx()` / `tfl_import_spec_xlsx()`; a study still in
workbooks converts once: `tfl_convert_sheets_to_yaml()`).

```text
study.yml                   the study: id, title, settings, keys
spec/data.yml               the data catalog: datasets, populations
spec/toc.yml                the reports, in order
spec/defaults.yml           what every report starts from
spec/patterns/<name>.yml    a layer a report may extend
spec/codelists.yml          code lists to copy from (optional)
spec/outputs/<id>.yml       one report a file
spec/_<name>.yml            a note, never read (_assumptions.yml)
```

```r
st   <- tfl_read_study_spec("XYZ-001")          # the study's files
outs <- tfl_read_outputs("XYZ-001")             # the reports, by id
o    <- tfl_resolve_output(outs[["T-1"]], st)   # the report over its layers
ard  <- tfl_build_ard(st, dir = "XYZ-001")      # the study ARD: every report's ard:
cat(tfl_content_code(o), sep = "\n")            # the table's plan, as code
plan <- tfl_content_run(o, tfl_ard_for(ard, "T-1"))
generate_rtfreport(tfl_report(o, content = plan, study = st), "T-1.rtf")
```

**study.yml** — `study_id`, `title`; `settings:` the table and report
study keys (`rounding`: `sas` / `iec` / `r`; `program_dir`; `program` and
`file`, each report's program and RTF file, `{output_id}.R` /
`{output_id}.rtf`); `keys:` the ARD's (`id`: the subject key, `USUBJID`;
`output`: where the study ARD goes; `source`: R files of the study's own
analysis functions).

**spec/data.yml** — `datasets:` (`dataset`, `level`, `path`, `derive`)
and `populations:` (`id`, `dataset`, `where`, `derive`, `label`): the
`datasets` and `populations` sheets of §A.1, a population's id under `id`;
`records:` (`id`, `dataset`, `where`, `label`, `any_label`): the study's
named sets of records (the `records` sheet; `- {id: TESAE, dataset: ADAE,
where: !r 'TRTEMFL == "Y" & AESER == "Y"', label: Treatment-emergent SAEs,
any_label: Any SAE}`), which an analysis names in `records: TESAE`;
`objects:` (`id`, `code`, `label`): the study's named values (the
`objects` sheet).

**spec/toc.yml** — `outputs:`, one entry a report in the TOC's order:
`id`, `kind` (`table` / `listing` / `figure`), `label` (its title),
`section`, `population` (a population's `id`).  The ARD program's
sections follow this order.

**A report file** (`spec/outputs/<id>.yml`):

| Key | What it holds |
|---|---|
| `id`, `kind`, `label`, `population` | as in the TOC; the file is named after `id` |
| `extends` | the layers under it, nearest first: `[defaults]` (the usual), `[patterns/<name>, defaults]`, or `none` |
| `ard` | the report's part of the study ARD: `analyses:` (the `analyses` sheet's rows, `analysis_id` as `id`, `population_id` as `population`) and `analysis_data:` (`data_id` as `id`) -- §A.1 |
| `data` | what the report reads.  `source: ard` (a table: the study ARD, `object: ard`; `steps:` R on it); `source: dataset` (a listing: `dataset`, `where`, `sort`); `source: code` (a figure: `steps:`, the design's data steps of §7; or `code: !r ...`, the report's own R) |
| `codelists` | by variable, a list of `{value, label}` in the order printed (the `codelists` sheet); a nested count's inner value may say `under:` (its parent's value, for `complete_hierarchy()`) |
| `content` | the report, a list of items, each one call: `- plan_cells: {categorical: '{n} ({p:.1f%})'}`.  A table's items are rtfreporter's `normalize_ard()`, `table_plan()` and `plan_*()` verbs; a listing's `plan_listing()` (its columns `listing_col()` calls); a figure's the calls of its plot -- `ggplot`, `geom_*`, `scale_*`, `labs`, `theme` ..., and `tfl_image` last (§7.1).  Or `content: !r ...`, the report's own R |
| `page` | the document: `titles`, `footnotes`, `header`, `footer` (maps by line number: `'1': {center: Table 14.1.1}`), `tokens`, and rtfreporter's calls by name (`rtf_page: {orientation: landscape}`, `rtf_tables: {auto_section: true}`, `rtf_document: {watermark: DRAFT}`); where a table's page names print is better said in its content, `plan_paginate_group: {heading_at: section}` -- a page's `rtf_tables: {auto_section: }` given wins |
| `template` | a template SPEC, `{name: km_survival, args: {...}}` (`tfl_templates()`; a catalog alias runs its template as itself, `{name: stf_t14_ae_soc_pt}`, and a report keeps the name it was written with -- `tfl_templates(source = "FDA STF IG v2")` / `"PhUSE CSS"` / `"CDISC ARS"` lists a collection's every item with its alias and template; FDA STF Figures 1, 4, 13: `km_cumulative`, its data stage in `data:` steps, samples in `extdata/report-definitions/`); a figure designer's preset is a name (`km_simple` ...), the designer's `plot`, `plots`, `compose` its old form (§7) |

**An item's keys are the function's arguments** (§A.2's tables say which
sheet column is which verb's argument: `pages_max_rows` is
`plan_paginate_rows: {max_rows: 40}`).  What each argument takes is
`tfl_schema(fn)` (the file `inst/spec/schema.yml`); `tfl_schema_check(item)`
checks one item; an argument the function does not have is an error.
Write values as YAML: a list `[TRT01A, SEX]`, a map `{AGE: Age (years)}`,
a number, `true` / `false`.  `!r` marks R code (`where: !r SAFFL == "Y"`,
`args: !r over_variables = TRUE`) and is kept for what only R can say;
`{fn: rtf_border, args: {top: single}}` is a nested call; `!unset`
removes what a layer below set.

**The layers.**  A report says only what differs from what it extends
(`tfl_resolve_output()`): `page` lines, `tokens`, `codelists` and a call's
arguments merge by key; in `content`, the report's items of one function
replace every item of that function below it, in the place the first had;
a function only the report has goes after them.  `defaults.yml` holds the
study-wide content and page (the running header and footer, the
rounding); a pattern (`@Compact` of the report sheets) is
`patterns/<name>.yml`.

---
## 6. A table's report file — the code spec

A table's file is short: what its shell shows, over the study's defaults.
The sample study's demographics table (`spec/outputs/T-14-1-1.yml`):

```yaml
id: T-14-1-1
kind: table
label: Demographic and Baseline Characteristics
population: SAF
extends: [defaults]
ard:
  analyses:
    - {id: N, method: total_n, dataset: ADSL, population: SAF}
    - {id: AGE, method: continuous, dataset: ADSL, population: SAF, by: [TRT01A], overall: true, variables: [AGE]}
    - {id: CAT, method: categorical, dataset: ADSL, population: SAF, by: [TRT01A], overall: true, variables: [SEX, AGEGR1, RACE]}
data: {source: ard}
codelists:
  SEX:
    - {value: M, label: Male}
    - {value: F, label: Female}
content:
  - table_plan: {cols: TRT01A, rows: {group: variable}}
  - plan_cells:
      continuous: {n: "{N:.0f}", Mean (SD): "{mean:.1f} ({sd:.2f})", Median: "{median:.1f}", "Min, Max": "{min:.0f}, {max:.0f}"}
  - plan_total: {label: Total}
  - plan_labels: {AGE: Age (years), SEX: "Sex, n (%)", AGEGR1: "Age group, n (%)", RACE: "Race, n (%)"}
  - plan_levels: {variable: [AGE, SEX, AGEGR1, RACE], TRT01A: [Drug X, Control]}
  - plan_col_header:
      header:
        - {line: "1", cols: .values, span: each, text: "{col}"}
        - {line: "2", cols: row_label, text: Characteristic}
        - {line: "2", cols: .values, span: each, text: "(N={n})"}
page:
  titles:
    "1": {center: Table 14.1.1}
    "2": {center: Demographic and Baseline Characteristics}
    "3": {center: Safety Population}
```

**From a shell to the file.**

| The shell shows | The file says |
|---|---|
| the arms across, `(N=xx)` under them | `ard`: `by: [TRT01A]`; `table_plan: {cols: TRT01A}`; the header's `(N={n})` |
| a Total column | `overall: true` on the analyses; `plan_total: {label: Total}` |
| a block of rows per variable | one analysis for the variables one method summarises (`continuous`, `categorical`); `rows: {group: variable}` |
| SOC / PT rows | `method: hierarchical`, `variables: [AEBODSYS, AEDECOD]`; `normalize_ard: {hierarchy: [AEBODSYS, AEDECOD], overall: <the any-AE row>}`; `rows: {group1: AEBODSYS}`; `plan_sort: {keys: [.overall, group1, .depth, -n, label]}` |
| a rate with its CI (ORR) | an `analysis_data` with `where` (the parameter) and `derive` (`!r 'RESP = AVALC %in% c("CR", "PR")'`); `method: proportion_ci`; `plan_cells: {proportion_ci: {"n (%)": "{n} ({estimate:.1f%})", 95% CI: "[{conf.low:.1f%}, {conf.high:.1f%}]"}}` |
| a shift table (baseline down, post-baseline across) | `by: [TRT01A, PARAMCD, BNRIND]`, `variables: [ANRIND]`; `table_plan: {cols: [TRT01A, .label], rows: {group: PARAMCD}, label: BNRIND}`; `plan_stub: {vars: [group, BNRIND]}`; the arm as a spanner `{line: "1", cols: .values, span: TRT01A, text: "{col1}"}` |
| `xx.x (xx.xx)`, `xx (xx.x)` | the template's decimals: `"{mean:.1f} ({sd:.2f})"`, `"{n} ({p:.1f%})"` |
| row headings, the order of the arms and values | `plan_labels`, `plan_levels`; a value printed in other words: `codelists` |
| titles and footnotes | `page: titles / footnotes`, by line number |

The keys of `ard:` are the ARD sheets' columns (Appendix A.1) under their
YAML names (`id`, `population`); the methods are `tfl_ard_methods()`.  The
content's functions and their arguments are `tfl_schema()` (the file
`inst/spec/schema.yml`); `tfl_schema_check(item)` checks one item.  A
listing's file is §11's; a figure's §7's.

---

## 7. Figure design (YAML)

A figure report's `content:` is written as **function calls** (§7.1).
The figure designer's design below -- its pieces (`km_curve`, `line` ...)
and `plot:` settings -- is the old form: still read and written, until the
designer works in function calls; `tfl_fig_content_from_design(design)`
converts one (the same figure: its layers' data, labels and axes).

### 7.1 A figure report's content: one item a call

```yaml
content:
  - ggplot: {data: sm, mapping: {x: AVISIT, 'y': mean, colour: TRTA}}   # makes the plot
  - geom_line: {position: {fn: position_dodge, args: {width: 0.3}}}     # its layers ...
  - geom_errorbar: {mapping: {ymin: lo, ymax: hi}, width: 0.2,
                    position: {fn: position_dodge, args: {width: 0.3}}}
  - scale_colour_tfl: {palette: treatment, values: !r levels(factor(df$TRTA))}   # ... its settings
  - labs: {x: Visit, 'y': Mean (SE), colour: !r NULL}
  - theme_tfl: {theme: boxed}
  - theme: {legend.position: bottom}
  - tfl_image: {dpi: 300}                                               # the image: always last
```

- The first item **makes the plot**: `ggplot` (data, mapping), `ggsurvfit`
  (`x` = the fit of a `survfit` data step), `ggcuminc`, or one of
  tflspec's own -- `tfl_risk_panel` (the number at risk under KM curves),
  `tfl_text_column` (a forest plot's text), `tfl_n_panel` -- or `!r`.  The
  items after it are its **layers** (`geom_*`, `annotate`, ggsurvfit's
  `add_*` on a `ggsurvfit` plot, `tfl_ard_number` -- a statistic of the
  ARD as a text) and its **settings** (`scale_*`, `coord_*`, `facet_*`,
  `labs`, `theme_tfl` / `theme`, `guides`), `+`-ed in their order.  An
  item's keys are the function's arguments (`tfl_schema("geom_line")`,
  the schema's figure catalog -- ggplot2, ggsurvfit, patchwork, and
  ggrepel, ggnewscale, ggh4x, ggtext, written `pkg::fn` in the code; a
  function it does not have, `pkg::fn`, is written by its formals).
- `mapping:` is `aes()`: a map of columns (`!r` for an expression); a
  constant is the function's argument, not the mapping (`colour: grey50`).
  `data:` and a fit name an object of the data steps.  `NULL` is `!r
  NULL`; `!unset` removes what a layer below set.  `scale_colour_tfl`,
  `scale_fill_tfl` and `theme_tfl` are the study's figure setup's
  (`tfl_fig_setup_code()`): the company standard's colours and theme.
- **Several plots** are named, each a sequence of its items, then the
  patchwork items that arrange them and `tfl_image`:

```yaml
content:
  - km:
      - ggsurvfit: {x: fit, linewidth: 0.3}
      - add_censor_mark: {shape: 4, size: 3}
      - labs: {x: Time (Months), 'y': Survival Probability}
  - risk:
      - tfl_risk_panel: {fit: fit, title: Number of Patients at Risk, size: 3, by: TRT01P, palette: treatment}
  - wrap_plots: {plots: [km, risk], ncol: 1, heights: [0.833, 0.167]}
  - tfl_image: {dpi: 300}
```

  A content with named plots holds nothing else but the arrangement
  (`wrap_plots`, `plot_layout`, `plot_annotation`) and `tfl_image`; a
  plot's first item makes it; a plot is not named as a function;
  `tfl_image` is the last item.  The code is each plot as `p_<name>`, then
  `content <- wrap_plots(p_km, p_risk, ...)`; one plot is `content`.  A
  `position_dodge()` said twice is written once as `pd`.
- `tfl_image: {width_in, height_in, dpi, format, file}` is the image:
  its size on the page (inches, or `fit` -- the page's body; left out: the
  page's `rtf_figures` size, else the figure's own 6.5 x 4.5 in) and its
  resolution (300 dpi when not said).  The report draws the figure at that
  size and resolution (`rtf_figures(doc, rtfplot(content, render_width =,
  render_height =, render_dpi =))`), so it is never stretched into its box;
  with `file` (`{output_id}.png`) the figure's code saves the image there
  too, in `format` (png, svg, pdf).
- The data steps (`data: {source: code, steps: [...]}`) are the
  designer's, below; the x axis's breaks a figure shares with its panel
  (`x_breaks`) are a `code` step.

`tfl_fig_design()`, `tfl_read_fig_design()` / `tfl_write_fig_design()`
(one `.yml` a figure), `tfl_fig_design_code()` writes the ggplot2 script,
`tfl_check_fig_design()` / `tfl_fig_advice()` check it, `tfl_fig_template()`
starts one (`tfl_fig_templates()` lists 38, each with its `category` --
the clinical category of `tfl_fig_catalog()` -- and the `data` it reads:
`"ADTR + ADRS"` both, `"ADLB / ADVS + ADSL"` one of ADLB / ADVS and ADSL),
`tfl_fig_parts()` lists every piece and field, `tfl_fig_objects()` the
object each data step reads (`in`) and leaves (`out`).

```yaml
template: km_risk_table
data:                                   # one ordered list of steps
- {step: read, dataset: ADTTE}          # -> df
- {step: param, value: OS}
- {step: flag, variable: FASFL}
- {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01A}   # -> fit
plot: {x_label: Time (Months), colour_by: TRT01A, legend: inside}
layers:                                 # what is drawn, in order
- {layer: km_curve}
- {layer: risk_table}                   # method: panel (default) | add_risktable
```

A read may take one of the report's own analysis data instead of a dataset
(`{step: read, data: adtte_os}`, its `ard: analysis_data:` row `adtte_os`):
the program makes it first (`tfl_ard_code(part = "data")`), as a table's
analyses do, and the figure starts from it.

- `data`: one list, in order. `read` starts `df`; `join`, `param`,
  `flag`, `filter`, `derive`, `time_unit`, `levels`, `rank` change the
  object above them; `survfit`, `summary`, `summary_by`, `rate`, `count`,
  `subset` make an object of their own by `name` (the steps after one
  change it, in its pipe); `function` any function's result
  (`fn: survival::survfit`, `args`, the object it reads as its
  `data_arg`); `code` is your own code; `ard` an analysis of the figure's
  ARD (`analysis_id`, `fn: cardx::ard_*`, `in` an analysis data, `args`)
  -- a row of the ARD definition, no line of the script.  Any step may
  say the object it reads (`in`) and the one it leaves (`out`): `filter`
  with `out: df_ev` makes `df_ev <- df |> filter(...)` and keeps `df`; a
  `survfit` with `in: df_ev` fits it.  Unset, a read makes `df` (then
  `df2`, `df3`), a step on rows changes the object above it, a named step
  reads `df`.  A design written
  before 0.0.24.9074 (`data:` and `stats:`, `data_code` / `stats_code`)
  reads as this one list and is written back in this form.
- The script (`tfl_fig_design_code()`) has two sections: `# ---- data ----`
  (one pipe an object, the code lists on `df`, the axis breaks, a panel's
  data) and `# ---- plot ----` (the palette, then one `+` chain into the
  figure; a panel under it is a chain of its own, joined on the last
  line: `plot <- plot / p_risk + plot_layout(heights = )`).  With
  `setup = TRUE` (a report program, after the study's figure setup) it has
  no header and no `library()`; its attributes `reads`, `libs`, `needs`
  say what it needs.
- The number at risk (`risk_table`): by default a panel of its own from
  the figure's fit (`summary(fit, times = x_breaks)`); `method:
  add_risktable` puts ggsurvfit's `add_risktable()` on the chain.
- A figure can read an ARD -- its own, or a table's (the program puts it
  in `ard`): the data step `ard_stats` (`name`, `analysis_id`,
  `variable`, `stats`, `by`) makes a data frame of statistics, a row per
  group, a column per statistic (`ard_stats(ard, ...)`); the layer
  `ard_number` prints one statistic (`analysis_id`, `variable`, `level`,
  `stat`, `group: TRT01A = Placebo`, `label: "Median: {value} days"` --
  `{conf.low}`, `{conf.high}` ... are other statistics of the same address:
  `"HR {estimate} (95% CI {conf.low}, {conf.high})"` --,
  `digits`; placed by `x`, `y`, `hjust`, `vjust`, `size`) as
  `annotate("text", label = ard_value(ard, ...))`.  The code's attribute
  `ard` is `TRUE` then; `tfl_check_fig_design(design, ard = <the ARD>)`
  looks for each address in it.  `ard_value()`, `ard_stats()` and
  `ard_fingerprint()` are study helpers (`tfl_helpers_code()`).
- A forest plot's numbers are an ARD of its own: `tfl_fig_template("forest_hr",
  data, param, pop, group, subgroups = "SEX, AGEGR1", comparison = <the
  arm against the reference>)` reads `ard_stats` of the analyses `HR`,
  `HR_SEX`, `HR_AGEGR1` (a Cox model of all subjects, one within each
  subgroup variable through `cards::ard_strata()`, each a `custom`
  analysis), which `tfl_fig_forest_analyses()` gives as rows of the
  `analysis_data` / `analyses` sheets (`attr(design, "analyses")`).  Its
  text at the right -- N, the estimate (95% CI) -- is the layer
  `text_column` (`data`, `y`, `columns: "N = n_obs | Hazard ratio (95%
  CI) = txt"`, `width`), a panel of its own: panels go below the plot or
  at its right, not both.  `errorbar_h` is the horizontal error bar.
- `layers`: catalog layers (`line`, `point`, `errorbar`, `boxplot` … see
  `tfl_fig_add_layer()`), `geom` (any geom by name), `call` (any function:
  `fn`, `package`, `data`, `aes`, `pos`, `args`), `layer_code`, `figure`.
- `plot.add`: `call`s written after the figure's settings (`theme()`,
  `scale_*()`, `facet_*()`, `labs()` …).
- Raw R inside a value is the `!r` tag: `labels: !r scales::label_number()`
  (`tfl_r()` in R).
- Composed figures: `plots:` (name → a whole design) and `compose:`
  (`layout: km | box`, `add:` patchwork calls).
- `ggplot2_version: "3.5"` or `"4.0"` writes the script for that version
  (`tfl_fig_compat()` lists the differences); `tfl_fig_calls()` lists
  extension functions offered by name.

The older sheet-based figure spec (`tfl_fig_spec()`, `tfl_fig_code()`,
`tfl_fig_km()`, `tfl_fig_waterfall()` …) and the quick figure functions
remain; new work should use the design.

---
## 8. What the written code looks like — the plan

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
## 9. Drafting with an AI chat

### 9.1 Tables' code spec from a TOC / SHELL workbook: the table task

The main path.  A company's TOC and shells (one Excel workbook, its own
layout) and its ADaM (XPT files, never sent) become report files, a batch
of 5 to 10 tables of one family at a time:

```r
tfl_ai_study_init("study", adam = "adam", study_id = "XYZ-ONC-01")   # study.yml, data.yml, defaults.yml
adam  <- tfl_ai_adam_summary("study/adam")          # variables, labels, short value lists; no records
shell <- tfl_ai_read_shell("TOC_SHELL.xlsx")        # every sheet as text, the TOC's reports found
b     <- tfl_ai_batches(shell)                      # one family a batch: demographics, ae, labs ...
ids   <- b$output_id[b$batch == "demographics-1"]
ctx   <- tfl_ai_context("table", outputs = ids, shell = shell, datasets = adam,
                        study_dir = "study", language = "Japanese")
p     <- tfl_ai_prompt("table", ctx)                # writeLines(format(p), "prompt.md"): paste it
a     <- tfl_ai_parse(readLines("answer.md"), "table")
tfl_ai_check(a, ctx)                                # errors: paste tfl_ai_repair_prompt(a, context = ctx)
tfl_ai_unpack(a, "study")                           # spec/outputs/<id>.yml, spec/_assumptions.yml
```

* `tfl_ai_read_shell(path, outputs = , sheets = , max_chars = )` writes
  every non-empty cell in reading order with its column, a merged cell once
  with its range, the indent as spaces; `outputs` keeps the TOC's header and
  those reports' rows and shells (a shell is the sheet named by its number,
  or the rows of a sheet from its number to the next).  `$toc` is the TOC
  it found (`output_id`, `title`, `type`, `shell`).
* `tfl_ai_adam_summary(path)` reads a folder of `.xpt` (haven); its
  `$variables` frame is the context's `datasets`.
* `tfl_ai_batches(x, size = 8)` splits the TOC's tables by the words of
  their titles (English or Japanese); `families =` adds a company's own.
* A whole study: `tfl_ai_write_requests(study_dir, shell, adam, size = 3)`
  writes one file a request to `ai/requests/` (one paste each; `max_chars`
  splits a long one, `instructions = "once"` writes the instructions to
  `00-instructions.md` and the requests without them).  The answers go to
  `ai/answers/<request>.md` (`tfl_ai_save_answer(study_dir, request,
  text)` writes one from pasted text); `tfl_ai_collect_answers(study_dir)`
  parses, checks and writes them: a list with `reports` (one row a report:
  `ok` / `warning` / `error` / `no answer`, its problems, its file, its
  request's repair prompt left in `ai/repairs/`), `next_steps`,
  `assumptions` and `manifest` (every report the answers list first,
  `reports:` -- `id`, `title`, `split`, `toc` --, with its status; also
  `ai/manifest.csv`, to check the count against the TOC).  `tfl_ai_prepare(dir, toc, ...)` does the steps before
  (the study folder when there is none, the ADaM summary, the workbook
  read, the requests) in one call and returns a list (`requests`, `toc`,
  `sheets`, `adam`, `files`, `next_steps`); `dry_run = TRUE` reads and
  writes nothing, `adam_summary =` reuses a call's `$adam`; `tfl_ai_requests(study_dir)`
  reads the requests again;
  a report always says its column widths: what an answer leaves out, R writes
  when the answer is read (`rtfreporter::default_col_widths()`, one width a printed
  column by name), with the assumption (item `widths`) that they are the default
  ones; `tfl_fill_widths(study_dir)` does the same for any report of the study;
  `tfl_ai_run(study_dir)` collects, then builds each report's ARD and RTF
  (`ai/status.md`; the answers' questions gathered into `ai/questions.md`);
  `mode = "agent"` writes, besides, a coding agent's instructions
  (`AGENTS.md`, `.github/copilot-instructions.md`), `run.R`,
  `ai/questions.md` and `ai/decisions.md` -- the agent answers the first
  requests it is given (`ai/agent_prompt.md`: one or two) and stops, the
  person checks the answers (`tfl_ai_collect_answers()`) and asks for the
  next (`tfl_ai_agent_prompt(study_dir)`: the message for the next ten); `agent_run = TRUE`: it also runs `run.R` and repairs; then a second
  round from the person's answers; `tfl_ai_questions(study_dir)`
  reads `ai/questions.md` and `tfl_ai_answer_questions(study_dir, answers)`
  writes the person's answers into it; `tfl_ai_outputs(study_dir)` lists
  the reports made, `tfl_ai_expected_write()` keeps the list the person
  expects (`ai/expected_outputs.csv`, which `run.R` compares with), and
  `tfl_ai_corrections(study_dir)` gathers the corrections and the answers
  for the agent's next round (`ai/corrections.md`); `tfl_ai_assumptions(study_dir)`
  lists the AI's assumptions with their ids (`A<n>`, kind `K<n>`), and
  `tfl_ai_assumption_answers_write()` keeps the person's answers to them
  (`ok`, or `wrong` with the right content: `ai/assumption_answers.csv`).  The TOC and its shells in
  workbooks apart: `tfl_ai_read_shell(path, shells = )` /
  `tfl_ai_prepare(toc, shells = )`; another workbook added to the study's
  requests: `tfl_ai_prepare(..., add = TRUE)`.  A figure of the TOC is a request
  of its own (`figure-1` ...: its shell, a figure design to answer), written
  as the figure's report file in the design's form and drawn by
  `tfl_ai_run()` (PNG and RTF); `workbook = "attach"` / `"both"` has
  each request say where its reports' shells are in the attached TOC /
  SHELL workbook (with the text too for `"both"`, the attachment right
  where they differ).
* `tfl_ai_context("table", examples = "study/spec/outputs/T-14-3-1.yml")`
  gives a reviewed report of the family: the prompt asks for the rest of the
  family written the same way.
* The answer is one YAML block: the header line (its task `table`, version
  1 and the reports), `outputs:` (one
  report file each, as §6), `assumptions:` (`output_id`, `where`, `item`,
  `assumption`, `question`).  `tfl_ai_check()` reads every report as
  `tfl_read_output()` does, checks each content item against the schema (an
  unknown function or argument, a value of the wrong type, a required
  argument missing), each analysis's fields and method, the analysis sets
  of `spec/data.yml`, every dataset and variable against the ADaM summary,
  and writes the ARD program and each table's code from a copy of the
  study folder -- nothing is run.  `tfl_ai_unpack()` writes the files;
  the batches add up in one folder.

### 9.2 A TOC or a figure design: the toc and figure tasks

tflspec makes the prompt and reads the answer; it sends nothing (no HTTP
client, no key). `tfl_ai_tasks()` lists what can be drafted: `toc` (the
study's list of outputs, read through `tfl_read_toc()`), `figure` (one
report's whole design, checked by `tfl_check_fig_design()`) and `table`
(§9.1). The prompt
carries the schema (`tfl_ai_schema()`, generated from what tflspec reads)
and the study's names and labels (`tfl_ai_context()`), never the data's
values; the answer is one fenced YAML (or JSON) block that starts with a
header naming its task and version. The result is always a draft: a
person reviews `tfl_ai_diff()` item by item before anything is applied.

```r
ctx <- tfl_ai_context("figure", output_id = "F-14-2-1",
                      titles = "Kaplan-Meier Plot of Overall Survival")
prompt <- tfl_ai_prompt("figure", ctx)    # format(prompt): paste into the chat
answer <- tfl_ai_parse(reply_text, "figure")
problems <- tfl_ai_check(answer, ctx)     # errors: send tfl_ai_repair_prompt(answer, problems)
tfl_ai_diff(current_design, answer)       # what to accept, piece by piece
```
### 9.3 A whole study at once: the kit

`tflspec_ai_manual(dir)` writes one flat folder to attach to a chat:
the request (`00_START_HERE.md`), this manual, the TOC / shell guide
(`02_tflspec-ai-toc-shell-guide.md`: how to read a company's TOC and
shells into the study's files, the checklist, the assumptions to list),
the schema (`03_spec_schema.yml`, every function a content item calls and
its arguments), every file of a study blank (`04_blank_spec.md`) and a
made-up study end to end (its TOC and shells, `05_example_toc_shell.xlsx`,
and the files written from them, `06_example_spec.md`).

The assistant answers with **one Markdown text holding every file**: a
heading with the file's path (`### spec/outputs/T-1.yml`), then a `yaml`
block, for `study.yml`, `spec/data.yml`, `spec/toc.yml`,
`spec/defaults.yml`, one `spec/outputs/<id>.yml` a report, and
`spec/_assumptions.yml` -- its guesses, a list of `output_id`, `where`,
`item`, `assumption`, `question`.  `tfl_ai_unpack(answer, dir)` checks the
paths (only `study.yml` and `spec/**.yml`) and that every block is YAML,
then writes the files; it writes nothing when one is wrong, and over
existing files only with `overwrite = TRUE`.

With `define =` (a `define.xml`), `adam_spec =` (an ADaM spec workbook) or
`datasets =` (the datasets, of which only their column information is
taken), the kit also holds `07_adam_metadata.csv`: `tfl_adam_metadata()`'s
result, one row a variable (`dataset`, `dataset_label`, `variable`,
`label`, `kind`, `levels` with ` | ` between them, `codelist`, `source`).
One source is used, the first of that order.  The assistant takes every
dataset, variable and level from it, and lists in `_assumptions.yml` any
it could not.  `tfl_ai_context("toc", datasets = )` and `("figure",
datasets = )` take the same frame.

`tfl_ai_review(dir, toc = , adam = , others = , metadata = )` checks the
study folder and splits what it finds by who acts: **the AI** (an output
or a title not the TOC's, `(N=80)` where `(N={n})` belongs, analyses no
table uses, no `_assumptions.yml`, every error and check of
`tfl_review_spec()` / `tfl_check_fig_design()`, each report file's items
against the schema: K16 an error, K17 an argument the function does not
have, K18 an `!r` where the schema has a form of its own) and **the
person** (each assumption; each analysis set's flag, percentage's
denominator, test or model, fixed order of levels).  Each finding names
the file and key it is in.  It writes `<dir>/<name>_repair.md` (paste into
the chat) and `<dir>/<name>_questions.md` (answer, then paste); `who` and
the kit's rules K01-K18 are `inst/review/ai_rules.csv`.  With `metadata =`
(the frame, or the kit's CSV), a dataset or variable the metadata has not
(K14) and a code list value that is not among a variable's levels (K15)
are the AI's to fix.  `tfl_ai_review(file)` still checks one workbook or
one report file.

```r
kit <- tflspec_ai_manual("ai-kit", define = "define.xml")   # attach every file
tfl_ai_unpack("answer.md", "XYZ-001")                       # the chat's answer, saved
toc <- tfl_read_toc("TOC.xlsx", sheet = "TOC", skip = 1,
                    map = list(output_id = "Shell Number",
                               title = c("Title1", "Title2"),
                               population = "Population"))
r <- tfl_ai_review("XYZ-001", toc = toc)                    # writes the two prompts
summary(r)                                                  # who x level
```

---

## 10. Composing with tflplanner

tflplanner is the GUI over these specs (one study folder: `spec/`,
`programs/`, `output/`). What it saves is these workbooks and YAML files;
anything written here can be opened there.

A study's setup file (tflplanner's `programs/study_setup.R`) defines the
folders as variables and attaches the packages every program uses. Two
options make the written code fit it: `options(tflspec.paths = c(path_adam =
"data/adam", path_ard = "output/ard"))` writes a file under one of those
folders through its variable (`readRDS(file.path(path_adam, "adsl.rds"))`),
and `options(tflspec.attached = c("cards", "dplyr"))` leaves out `pkg::` for
those packages in the code (a string or comment keeps it) and the ARD
program's own `library(cards)`. Unset, the code is as before.
`tfl_build_ard()` runs the program by itself and ignores both.

### Names in the generated programs

A program's own variables are never a data's or an ARD's column: ADaM
columns are upper case (`USUBJID`, `TRT01A`), ARD and spec columns lower
case with underscores (`output_id`, `analysis_id`, `stat_name`), and the
program's names are
`report_id` (the report's id, in the ARD and the report programs), `cl_<variable>` (a code
list), `path_<folder>` (a folder of the study's setup), `ard_<analysis id>`
(an analysis's ARD), `<dataset>_sub` (the data the program makes: an
analysis set's, `adsl_sub`, and an analysis's records, `adae_sub`; a
report's second of a dataset `adsl_sub2`, each under a comment saying
what it is), `fmt_default`
and `tfl_stats` (the company standards' values).  The ARD has no method
column: ARS's `methodId` comes from the spec.

---

## 11. A listing's report file

A listing reads a dataset (`data: {source: dataset, dataset: ADAE, where: !r 'AESER == "Y"', sort: [USUBJID, ASTDY]}`),
the subjects of an analysis set in it (`population: SAF`), or one of the
report's own analysis data (`data: adae_ser`, instead of `dataset` and
`population`; its `ard: analysis_data:` row),
and its `content:` is one `plan_listing` item, its columns
`listing_col()` calls:

```yaml
content:
  - plan_listing:
      type: multiline
      cols:
        - {fn: listing_col, args: {vars: [USUBJID], label: Subject, collapse_repeats: true}}
        - {fn: listing_col, args: {vars: [AEBODSYS, AEDECOD], label: "System Organ Class /\nPreferred Term", sep: "\" / \""}}
```

The listing sheets of Appendix A.4 say the same.

### 11.1 Several objects in one report (`contents:`)

One report (one RTF) can hold several tables and figures on its pages.
Write `contents:` (a map of named objects) in place of `content:`, and say
each object's place in `page: layout:`:

```yaml
contents:
  demog: [{table_plan: {cols: TRT01A}}, ...]      # reads the report's ard:
  km:                                              # its own data (a figure)
    data: {steps: [{step: read, dataset: ADTTE}, {step: param, value: OS}]}
    template: {name: km_survival, args: {by: TRT01A}}
page:
  titles: {"1": {center: Table 14.1.9}}            # above the first object
  footnotes: {"1": {left: ...}}                    # below the last
  layout:
    - {object: demog}
    - {object: km, page_break: false, space_before: 1, height_in: 3,
       titles: {"1": {left: Overall survival}}}
```

A layout row takes `object`, `page_break` (each object after the first
goes on under the one before unless `true`), `space_before`, `first_rows`,
`cont_title` (a table's), `titles`, `footnotes`, `width_in`, `height_in`
(a figure's). Tables share the report's ARD: keep each table to its own
analyses with `plan_filter: {where: !r 'analysis_id %in% c("AGE", "SEX")'}`.
`tfl_run_output("<id>")` writes the RTF. Not yet in the workbooks (sheets).

---

## 12. The sheet spec (Excel workbooks)

The same definition can be kept as Excel workbooks -- an ARD spec, a table
spec, a report spec, a listing spec -- the form tflspec read before the
YAML spec, and the form the review (`tfl_review_spec()`) and the CDISC ARS
writer (`tfl_ars()`) read.  Appendix A describes them.  A study in
workbooks converts once (`tfl_convert_sheets_to_yaml(from, to)`); a folder
is written as workbooks and read back with `tfl_write_spec_xlsx()` /
`tfl_import_spec_xlsx()`.

---

## 13. Names that do NOT exist

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

### 13.1 How the functions are named and take their arguments

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
## 14. Complete public API (nothing outside this list exists)

**Spec files (YAML, one file a report):** `tfl_output` `tfl_item` `tfl_plot` `tfl_call` `tfl_r`
`tfl_unset` `tfl_read_output` `tfl_write_output` `tfl_read_study_spec`
`tfl_read_outputs` `tfl_resolve_output` `tfl_run_output` `tfl_output_contents` `tfl_output_data_names` `tfl_code_refs` `tfl_write_study_part` `tfl_item_code`
`tfl_item_written` `tfl_item_own` `tfl_convert_sheets_to_yaml`
`tfl_yaml_from_sheets` `tfl_sheets_from_yaml` `tfl_sheets_from_spec` `tfl_write_spec_xlsx`
`tfl_import_spec_xlsx` `tfl_schema` `tfl_schema_check` `tfl_schema_help` `tfl_content_code` `tfl_content_run`
`tfl_ai_unpack` `tfl_args_to_r` `tfl_args_from_r` `tfl_fig_content_from_design`

**Manual:** `tflspec_ai_manual` `tfl_adam_metadata` `tfl_ai_review`

**The functions the generated programs call (their code):** `tfl_helpers_code`

**Template SPEC (prototype):** `tfl_tpl` `tfl_tpl_args` `tfl_tpl_help` `tfl_templates` `tfl_template_expand` `tfl_template_to_spec` `tfl_template` `tfl_param` `tfl_template_params` `tfl_template_variant` `tfl_template_run` `tfl_test_templates` `tfl_use_company_templates`
`tpl_demog` `tpl_summary` `tpl_disposition` `tpl_ae_soc_pt` `tpl_ae_pt_freq` `tpl_ae_max_grade` `tpl_hierarchical`
`tpl_shift` `tpl_lab_shift` `tpl_listing` `tpl_km_table` `tpl_param_change` `tpl_lab_abnormal` `tpl_ae_eair` `tpl_ancova` `tpl_mean_se` `tpl_mean_ci` `tpl_waterfall` `tpl_forest_hr` `tpl_forest_layout` `tpl_orr` `tpl_stat_block` `tpl_ae_overview`
`tfl_read_template_sheets` `tfl_write_template_yaml`

**AI-assisted drafting:** `tfl_ai_tasks` `tfl_ai_context` `tfl_ai_schema`
`tfl_ai_prompt` `tfl_ai_parse` `tfl_ai_check` `tfl_ai_repair_prompt`
`tfl_ai_diff` `tfl_ai_read_shell` `tfl_ai_adam_summary` `tfl_ai_batches`
`tfl_ai_write_requests` `tfl_ai_collect_answers` `tfl_ai_adopt` `tfl_ai_adopted_diff` `tfl_ai_style_prompt` `tfl_ai_style_read` `tfl_rtf_style_scan` `tfl_ai_migrate` `tfl_ai_outputs` `tfl_ai_expected_read` `tfl_ai_expected_write` `tfl_ai_corrections` `tfl_ai_assumptions` `tfl_ai_assumption_answers_read` `tfl_ai_assumption_answers_write` `tfl_ai_save_answer` `tfl_ai_study_init`
`tfl_ai_prepare` `tfl_ai_requests` `tfl_ai_agent_prompt` `tfl_fill_widths` `tfl_ai_run` `tfl_ai_compare` `tfl_ai_questions`
`tfl_ai_answer_questions`

**ARD spec:** `tfl_ard_spec` `tfl_read_ard_spec` `tfl_write_ard_spec`
`tfl_ard_spec_template` `tfl_ard_code` `tfl_ard_data_names` `tfl_build_ard` `tfl_ard_for`
`tfl_ard_spec_hash` `tfl_ard_methods` `tfl_ard_as_custom` `tfl_ard_statistics`
`tfl_ard_functions` `tfl_ard_args` `tfl_write_ard` `tfl_read_ard` `tfl_check_ard` `tfl_check_ard_function` `tfl_ard_function_template` `tfl_ard_function_info` `tfl_ard_conditions`

**CDISC ARS:** `tfl_ars` `tfl_write_ars_json` `tfl_check_ars`
`tfl_ars_unmapped` `tfl_ars_ard` `tfl_read_ars_json` `tfl_ars_to_specs`
`tfl_write_ars_xlsx`

**Table spec:** `tfl_normalize_code` `tfl_table_spec` `tfl_read_table_spec` `tfl_write_table_spec`
`tfl_write_report_spec` `tfl_write_specs` `tfl_spec_columns`
`tfl_table_spec_template` `tfl_table_plan` `tfl_table_code`
`tfl_as_table_spec`

**Report spec:** `tfl_read_report_spec` `tfl_read_toc` `tfl_report` `tfl_report_code`
`tfl_report_overwrite` `tfl_report_path` `tfl_report_setup_code` `tfl_report_tokens` `tfl_check_code`

**Document import (a define, by rule):** `tfl_read_define`
`tfl_read_define_spec` `tfl_define_profile` `tfl_define_codelists`

**Listing spec:** `tfl_listing_spec` `tfl_read_listing_spec`
`tfl_write_listing_spec` `tfl_listing` `tfl_listing_code`
`tfl_as_listing_spec` `tfl_read_data_code`

**Figure design:** `tfl_fig_design` `tfl_read_fig_design`
`tfl_write_fig_design` `tfl_fig_design_code` `tfl_check_fig_design`
`tfl_fig_advice` `tfl_fig_apply_fix` `tfl_fig_template` `tfl_fig_forest_analyses` `tfl_fig_templates`
`tfl_fig_parts` `tfl_fig_objects` `tfl_fig_page_size` `tfl_fig_uses_ard` `tfl_fig_add_layer` `tfl_fig_calls` `tfl_fig_compat`
`tfl_fig_data_name`
`tfl_r`

**Figure style:** `tfl_fig_style` `tfl_fig_style_template`
`tfl_read_fig_style` `tfl_fig_setup_code` `tfl_fig_palettes` `tfl_time_unit_days`
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

**Review:** `tfl_review_spec` `tfl_review_rules` `tfl_review_templates`
`tfl_data_facts` `tfl_ard_facts`

---
## 15. Troubleshooting

| Message | Cause | Fix |
|---|---|---|
| `The \`layout\` sheet has a column it does not read: 'stub_into' (renamed \`stub_name\`)` | a workbook from before 0.0.24 | rename the column as the message says (§13) |
| `tfl_table_plan(): 'notes' is not a role` | table-wide options passed to `tfl_table_plan()` | pipe into the verb: `plan_cells(notes = FALSE)` |
| `could not find function "table_plan"` | the program did not attach rtfreporter | `library(rtfreporter)` first |
| `... has a value containing the key separator "_"` | `sep = "_"` and a key value with `_` | rename the value, or `plan_columns(sep = )` / `tables$sep` |
| `The sheet X has rows in both a.xlsx and b.xlsx` | a sheet said in two workbooks | keep its rows in one |
| a header prints `N=NA` with a warning | the ARD does not state that population | add it to the ARD (`cards::ard_total_n()` …) or give `plan_col_header(values = list(n = ...))` |

---

## Appendix A. The sheet spec (Excel workbooks)

The older form of the definition: the workbooks a study kept before the YAML spec, and what the review and the ARS writer read. Every key of the code spec has its column here (§5 says which).

### Starter with workbooks

```r
library(rtfreporter)
library(tflspec)

d  <- system.file("extdata", "ard-spec", package = "tflspec")
sp <- tfl_read_table_spec(c(file.path(d, "study.xlsx"),
                            file.path(d, "report.xlsx")), output_id = "DM")

# the program a study keeps: the table plan + the document
cat(tfl_table_code(sp, pipe = "|>"), sep = "\n")
cat(tfl_report_code(sp, content = "plan"), sep = "\n")

# or run it on a normalized ARD (rtfreporter::normalize_ard())
# plan <- tfl_table_plan(normalize_ard(ard), sp)
# generate_rtfreport(tfl_report(sp, content = plan), tfl_report_path(sp), overwrite = TRUE)
```

`tfl_table_code()` writes `plan <- ard |> normalize_ard(...) |> table_plan(...) |> plan_*(...)`
(`normalize = FALSE`: `plan <- table_plan(data, ...)`, the data normalized already);
`tfl_report_code()` writes `doc <- rtf_document(...)`,
`rtf_section(...)`, `rtf_tables(doc, plan)`.

### A.1 ARD spec — five sheets

`tfl_ard_spec()`, `tfl_read_ard_spec()`, `tfl_write_ard_spec()`,
`tfl_ard_spec_template()`.

| Sheet | Columns | Meaning |
|---|---|---|
| `study` | `key`, `value` | `id`: the subject key (`USUBJID`); `output`: where the study ARD goes (`output/ard/ard.rds`); `source`: R files of the study's own analysis functions, relative to the study folder (`|` between them) |
| `datasets` | `dataset`, `level`, `path`, `derive` | a name for the data, its level (`SDTM` / `ADaM`), its file relative to the study folder, new columns (`NAME = R expression`, `|` between them) |
| `populations` | `population_id`, `dataset`, `where`, `derive`, `label` | an analysis set: the subjects of `dataset` for which `where` (R) holds; `derive` adds columns (`TRTA = TRT01A`); `label` is the text a header prints (Safety Analysis Set) and the set's name in ARS (blank: the id). "Population" is tflspec's word for what ICH E9 and CDISC ARS call an analysis set: `tfl_ars()` writes it as an `AnalysisSet`, an analysis's `population_id` as `analysisSetId` |
| `records` | `record_id`, `dataset`, `where`, `label`, `any_label` | named sets of records (CDISC ARS data subsets): an analysis's `records` reads the set's dataset under its condition (the analysis's `where` on top), instead of `dataset` and `where`; the program's data of it is commented with the set (`# adae_sub: TESAE — Treatment-emergent SAEs (...)`); `any_label` names the "any" row of a hierarchy of them (`normalize_ard(overall = )`) when the table says none, else `Any <record_id>` |
| `objects` | `output_id`, `object_id`, `code`, `label` | named values the ARD program makes at its top (`object_id <- code`, `label` a comment above): the study's (`output_id` blank; data.yml `objects:`) before a report's own (its `ard: objects:`); an analysis names one as its `denominator`, or in its `args` / `where`. YAML: `objects: [{id: n_saf_jp, code: !r 'c("Drug X" = 40, "Placebo" = 38)', label: ...}]` |
| `groups` | `group_id`, `label`, `level`, `where` | condition-defined groups (CDISC ARS grouping factors; data.yml `groups:`, each with its `levels:`): a row a level, the condition (R) its records meet -- levels may overlap ("1 or 2", ">= 3 months"), a level with no `where` is every record ("All"). An analysis's `groups` runs it once in each level of each group named; its rows say the group's `label` in `group2` and the level in `group2_level`, in this sheet's order |
| `analysis_data` | `output_id`, `data_id`, `label`, `from`, `population_id`, `subjects`, `where`, `add`, `derive`, `keep`, `distinct`, `code` | named data a report's analyses read (a report's, by `output_id`: the same `data_id` may mean something else in another report; a row names only its own report's rows above it): made `from` a dataset or an analysis data above, kept to a population's subjects and the records `where` keeps, with columns of the population's data added by the subject key (`add`), columns derived, and one row per set of values (`distinct`: `USUBJID | APHASE`, a denominator per subject and phase) -- in that order; `subjects` (instead of `population_id`) keeps the subjects of an analysis data above, a report's own subjects (the numerator kept to the denominator's), and `keep` the columns kept (with `derive`, transmute()). `data_id` is the object's name in the ARD program (lower case, not a dataset's or the program's own `<dataset>_sub`). When the columns cannot say it, `code` is R that makes the data itself (its value is the data; the datasets, the analysis set's data `<dataset>_sub` and the analysis data above are in reach); then the other columns but `from` stay blank |
| `analyses` | `output_id`, `analysis_id`, `label`, `method`, `data`, `dataset`, `population_id`, `where`, `by`, `overall`, `strata`, `groups`, `variables`, `statistics`, `denominator`, `formats`, `args`, `code`, `purpose`, `reason` | one analysis a row; `groups`: the condition-defined groups it runs in (each level's rows in `group2` / `group2_level`); both ids become ARD columns; `purpose` / `reason` (CDISC terms) only for `tfl_ars()` |

- `method`: a keyword — `continuous`, `categorical`, `dichotomous`,
  `missing`, `hierarchical`, `max`, `subjects`, `total_n`, `proportion_ci`,
  `mean_ci`, `ttest`, `wilcox`, `chisq`, `fisher`, `custom` — or any
  `pkg::function` (`cards::`, `cardx::`), or a function of the study's own
  that the study key `source` loads (`ard_riskdiff_mn`): it takes the
  analysis data first, `by` and `variables` as bare column names, and gives
  a cards ARD. The function a keyword calls (`cards::ard_summary`,
  `cards::ard_tabulate` …) is that keyword: the same statistics, defaults
  and formats. `tfl_ard_methods()` lists the
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
- `data`: an analysis data (`data_id`) instead of `dataset` /
  `population_id` (not both); the analysis set is its population, and the
  analysis's `where` is applied on top of its conditions. `denominator` may
  name an analysis data too (as it is: one row a subject and phase gives
  the N of each phase). A spec without the sheet reads and writes as
  before: the same code, the same fingerprints.
- `by`, `strata`, `variables`, `statistics`: `|` between several. `strata`:
  the analysis repeated within them (cards' `strata`: a subgroup, a
  parameter by visit).
- `by` is required in a table with columns (`table_plan(cols = TRT01A)`):
  an analysis without it is a problem that says `by: [TRT01A]`, but for
  the columns' own count (the N: `variables: TRT01A`); `by: []` (in the
  sheet `(none)`) says an analysis has no columns. A `by` the data does not
  have stops the program (never left out).
- `overall`: `TRUE` repeats the analysis without its `by`, over all its
  subjects — cards' own overall rows, with no group — bound under the
  grouped call (`bind_rows(ard_tabulate(adsl, by = ARM, ...),
  ard_tabulate(adsl, ...))`; on a `cards::ard_stack` row, `.overall =
  TRUE`). This is the ARD of a **Total column**: no `ARM = "Total"` value
  the data does not have, so the ARS stays true (`tfl_ars()` writes it as
  an analysis `..._TOTAL` without the grouping). Not inside a parent (put
  it on the parent), not on `custom` / `subjects`.
- `parent`: the analysis this one runs inside, when that one's method is
  `cards::ard_stack` (several analyses on the same data and `by`, plus the
  by counts and the total N with `args` `.total_n = TRUE`),
  `cards::ard_strata` (within the subgroups of its `by` / `strata`) or
  `cards::ard_pairwise` (each pair of the levels of its one `variables`).
  The rows inside leave `dataset` / `population_id` / `where` (and in a
  stack `by`) blank: they are the parent's. The program writes one call,
  `cards::ard_stack(adsl_sub, .by = ARM, cards::ard_summary(variables =
  c(AGE, BMIBL)), cards::ard_tabulate(variables = SEX))`, and tags each
  variable's rows with its own analysis (the stack's own rows with the
  parent's id). ARS gets each row inside as an analysis of its own.
- `post`: steps on the ARD after the call, the ARD left out, `|` between
  them — `cards::add_calculated_row(expr = sd / sqrt(N), stat_name = "se")`,
  `cards::filter_ard_hierarchical(p > 0.05)`, `cards::sort_ard_hierarchical()`,
  `complete_hierarchy()` (a nested count's code-list values no subject has,
  counted 0: cards counts only the values some group has; the outermost
  variable's from its code list, an inner one's where the code list's
  `under` says its parent; written with the row's `variables` and those
  `under`; put it before a filter or a sort; nothing under an overall row
  of 0 unless `when_none = TRUE`).  A nested count with no records is not a
  stop: the program's `ard_hierarchical_or_none()` gives the overall row 0
  and each group's N, and the table is that row alone.
  Written as `ard <- ard |> step1 |> step2`. Not on a row inside a parent
  (put it on the parent).
- `records`: a set of the `records` sheet (data.yml `records:`) the
  analysis reads -- its dataset and condition, the analysis's `where` on
  top; then `dataset` and `data` stay blank.
- `denominator`: what percentages are of — `population` (the analysis
  set; `hierarchical` and `max` take it anyway), `row` / `column` / `cell`
  (cards), another population, a dataset (its records of the analysis
  set's subjects), or numbers given: one (`denominator: 86`, every
  column's N) or one a value of the analysis's one `by` variable
  (`denominator: {Drug X: 86, Placebo: 85}`; in the sheet
  `c("Drug X" = 86, "Placebo" = 85)`). Not in `args` as well.
- `args`: more arguments as R (`over_variables = TRUE, overall = TRUE`),
  read as the arguments of a call — any order; refused if not R.
- `formats`: `mean=xx.x | p=xx.x% | AGE:sd=xx.xx` — the `xx` part says the
  decimals only. The program gives them to the cards call itself
  (`fmt_fun = everything() ~ modifyList(fmt_default, list(mean = 1L))`,
  over the catalog's defaults `fmt_default`); a function that takes no
  `fmt_fun` (cardx's tests, CIs and models, a study's own function,
  `custom` code, `ard_stack()`'s own rows) gets them after the call,
  `fmt_ard(modifyList(fmt_default, list(...)))`. Same `stat_fmt`.
- `custom` takes R in `code`; `args` passes arguments to the method.
  `tfl_ard_as_custom(spec, output_id, analysis_id)` writes an analysis as
  the code of a custom one (the same ARD), to start from when the columns
  cannot say it; the definition keeps the code, the program is never edited.
- In `args` and `code`, `data` is the analysis data and `population` the
  analysis set's subjects; the program writes them as the data's own names
  (`adtte_ttde`, `adsl_sub`), and a `custom` analysis's last expression is
  its ARD (`local()` only when its code defines a function or assigns a
  name the program has).
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

#### A.1.1 CDISC ARS — writing and reading the analyses as the standard

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

### A.2 Table spec — sheets and columns

`tfl_table_spec()`, `tfl_read_table_spec()`, `tfl_write_table_spec()`,
`tfl_table_spec_template(ard)` (scaffolds `tables` / `variables` / `cells`
from an ARD). A workbook may be split over several files; a sheet is said
once. The `study` sheet has `key` / `value` (`rounding`: `sas` / `iec` /
`r`; `output_path`; `program_dir`).

**tables** (one row a table) — the roles and the table-wide layers:

| Column | Goes to | Notes |
|---|---|---|
| `cols` | `table_plan(cols = )` | column keys, outermost first: `TRTA \| SEX` |
| `rows` | `table_plan(rows = )` | `name = column`; a quoted value is a constant heading |
| `label` | `table_plan(label = )` | blank keeps `.label`; `NA` builds then drops; `NULL` leaves out |
| `stats` | `plan_cells(stats = )` | `cells` (default: a cell from a template) or `rows` (one statistic a row, the raw values) |
| `value` | `plan_cells(value = )` | which of the ARD's values the table prints: `stat` (the number, rounded here by the `digits` sheet; default) or `stat_fmt` (the ARD's own text: no digits set here). For `cells` templates too; a row may say otherwise (`{mean:stat_fmt}`, `{mean:.1f}`). **Not** a template: templates are `cells$template` |
| `na` | `plan_cells(na = )` | what a cell no template could fill prints |
| `sep` | `plan_columns(sep = )` | what joins several column keys into a column name (default `____`) |
| `sort` | `plan_sort()` | `TRUE`, `FALSE`, or keys in order, `-` for descending |
| `sort_stat` | `plan_sort(stat = )` | the statistic totalled for a frequency order |
| `header_n` | `plan_col_header(values = list(n = ))` | `page`, `table`, or `n = page \| N = table` |
| `stat_groups` | `plan_stat_groups()` | one record's statistics in named groups, `n = N \| orr_ci = n, estimate, conf.low, conf.high`; the key `.stat_group` in `cols` puts N in one column and the rate and CI in the next, in `rows` one group under another; a `cells` row's `variable` may be a group's name, its template one row or several (n rows x m columns of one record). Keep the ARD as it is: no `mutate()` of `variable` |
| `show_keys` | `table_plan(show_keys = )` | row keys also shown as variables, `SEX | AGEGR1`: their own n (%) a block beside the subgroup ORR; with `stat_groups` add `.variables = RESP | .others = n` |
| `keep_where`, `keep_any`, `keep_parents` | `plan_filter()` | rows kept by an R condition over the record (`p >= 0.05`: PTs with 5% or more in any arm; a SOC with its PTs) |
| `compare`, `compare_by`, `compare_position` | `plan_compare()` | comparison columns, `p | diff`; each column's templates are `cells` rows with `variable` = its name (`{p.value:.3f}`, `{estimate:.2f} ({conf.low:.2f}, {conf.high:.2f})`); the ARD needs the test records (`cardx::ard_stats_t_test()`, `ard_stats_chisq_test()`, ...) |
| `stack`, `stack_sep`, `stack_nest` | `normalize_ard(stack = , stack_sep = , stack_nest = )` | subgroups as one pair of keys, `subgroup = ALL, SEX, AGEGR1, AGEGR1 * SEX` (`*` crosses; or the slots `subgroup = group2, group3`); `rows` then names `subgroup` / `subgroup_level`, never `group2` / `group2_level`; written by `tfl_table_code()` at the head of the pipe (`tfl_normalize_code()`: the line alone) |
| `overall_label` | `normalize_ard(overall = )` | the any-event row of a hierarchy ("Subjects with at least one TEAE"; its analysis `args` `over_variables = TRUE`): the program's normalization, written by `tfl_table_code(spec, ard_spec = )` (with the `hierarchy` of its `hierarchical` analysis) |

**variables**: `variable`, `label`, `order` (the variables' order: `plan_levels(variable = c("AGE", "SEX"))`), `levels` (`Grade 0 | Grade 1`; `F = Female | M = Male` gives each value its text too, `plan_levels(SEX = c(F = "Female", M = "Male"))` -- rarely needed: the ARD has the code list's labels already), `empty_levels` (`hide`: no row for a value no record has; blank: a 0 row), `under` (`RACE: Asian`: its rows under that level of RACE, one indent deeper, its own heading dropped -- `plan_nest()`)
→ `plan_labels()`, `plan_levels()`.

**codelists** (a report's code lists, one row a value): `output_id`, `variable`, `value`, `label`, `order`, `under` (a nested count's inner value: its parent's value as the data have it, `EYE DISORDERS` for a PT; `complete_hierarchy()` in the analysis's `post` then shows it 0 when no subject has it). Each value's place and **what it is in the data the report's program makes** (`label`; blank: the value itself): `tfl_ard_code(codelists = )`, `tfl_listing_code(codelists = )` and `tfl_fig_design_code(codelists = )` write `cl_sex <- c(F = "Female", M = "Male")` at the program's head and `set_levels(SEX = cl_sex, ...)` on the data the program reads (the ARD's analyses: the columns they read -- `by`, `strata`, `variables`, names in `args` / `code` / `post`; a listing: the columns it shows or sorts by; a figure: last of its data steps). Each listed column becomes a factor in the list's order with the labels as its values, so **the ARD holds "Female"**, in the list's order (cards keeps it; after an analysis that is not cards' -- custom code, cardx: survfit makes the arms' factor again, alphabetical -- the program writes `relevel_ard(TRT01A = cl_trt01a)`, so no `plan_levels()` of the arms is needed); a value the list does not have (not NA) stops the program with the column and the value. The table then orders the ARD's values (`plan_levels(SEX = c("Male", "Female"))`, the labels) and `plan_labels()` gives only the variables' headings (the `variables` sheet's `label`), written `plan_labels(variable = c(AGE = "Age (years)", SEX = "Sex"))` so they never meet a variable's value labels. A condition on a listed column the analysis reads is written in its labels (`SEX == "Female"`; `tfl_ard_code()` stops on `SEX == "F"` and says so); ARS writes the data's values back (`SEX == "F"`). Every row names its report: a blank `output_id` is an error (no study-wide rows; copy the rows into each report). A variable's `levels` on the `variables` sheet, when given, is the order instead (in the labels). The code list of `variable` (an earlier form of the headings) is moved to the `variables` sheet's `label` when the table spec is read, with a message.

**digits** (each statistic's decimals, once): `variable` (blank: every analysis variable; a name: its exception), `statistic` (`mean`, `sd`, `p` a percent ...), `digits` → written into the templates' tokens that say no format (`{mean} ({sd})` → `{mean:.1f} ({sd:.2f})`). A template's own format or a `cells` row's `digits` wins; a table of `tables$value = stat_fmt` (the ARD's own text) is not rounded here.

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
| `group_page`, `group_col`, `group_keep`, `group_heading`, `group_heading_at` | `plan_paginate_group(col, keep, heading, heading_at)` — one page per value; `group_heading = Treatment: {value} (N={n})` names each page (the `auto_section` heading) with the value and its N from the ARD (bind `cards::ard_tabulate(adsl, variables = TRT01A)`); `group_heading_at = section` prints it in the section header (`title`: under the titles), so the report needs no `auto_section` |
| `pages_heading`, `pages_heading_at` | `plan_paginate_rows(page_by, heading, heading_at)` | each BY page named like `group_heading`, and printed where `pages_heading_at` says, a page per arm with a row budget inside |
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

### A.3 Report spec — the document

`tfl_read_report_spec()` reads the table sheets **and** these; `tfl_report()`
runs them, `tfl_report_code()` writes them, `tfl_report_path()` gives the
file.

| Sheet | Columns |
|---|---|
| `report` | `type` (`table` / `listing` / `figure`), `file` (`{output_id}.rtf`), `program`, `auto_section`, `section_align`, `auto_title`, `title_align`, `table_font_size_half_points`, `title_font_size_half_points`, `footnote_font_size_half_points`, `page_header`, `page_footer`, `watermark` (`DRAFT`), `figure_width_in`, `figure_height_in` (a figure's size on the page: a number of inches, `fit` -- the page's body -- or blank, the figure's own size as rtfreporter places it -- `tfl_fig_page_size()`; a design's `plot` has only `dpi`, the resolution), `ard_source`, `pattern`, `data_code` and `content_code` (a step that is the report's own R instead of the definition: the data step leaves `data`, a figure's `df`; the content step leaves `content`; the report's datasets by their names in lower case, and `ard` when it reads its ARD, are in reach -- `tfl_check_code()`) |
| `page` | `paper_size`, `orientation`, `width_in`, `height_in`, margins `margin_*_in`, `header_dist_in`, `footer_dist_in`, `font` (the document's font; the study's row goes to the setup's `options()`), `font_size_half_points`, `title_format`, `footnote_format`, `title_width`, `footnote_width`, `markup` |
| `header`, `footer`, `titles`, `footnotes` | `line`, `left`, `center`, `right` — a report's line replaces the default line of the same number; a report's line that says `(none)` takes it out |
| `tokens` | `name`, `value` — a token of one's own, `{STUDY}` in any of the cells above (`rtf_document(tokens = )`); a report's row replaces the default of the same name, `(none)` takes it out; names upper case, not rtfreporter's own. A report's own tokens work without a row: `{OUTPUT_ID}`, `{OUTPUT_LABEL}` ("Table 14.1.1" from the ID; rows `OUTPUT_KIND_TABLE` / `_LISTING` / `_FIGURE` change the words), `{OUTPUT_TITLE}` `{OUTPUT_POPULATION}` `{OUTPUT_SECTION}` `{STUDY_ID}` (blank unless a row gives them); a line left with only empty tokens is not printed. What every report shares (the default token rows, the default header / footer) can be written once: `tfl_report_setup_code(spec)` (`options(rtfreporter.tokens = )`, `study_header`, `study_footer`; for the YAML spec `tfl_report_setup_code(study)`, from `spec/defaults.yml`), and each program with `tfl_report_code(..., setup = TRUE)` |

Page tokens in the running header / footer: `{PAGE}`, `{TOTAL_PAGES}`,
`{PROGRAM}` (or `{PROGRAM_FULL}`, its absolute path), `{DATETIME}`.

#### A.3.1 A company's TOC as report specs

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
  `population`, `footnote`, `program`, `file`, `note`, `section`,
  `datasets`, `label`); values are the
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
- Each report's section is `attr(, "sections")` (named by output id; `NA`
  for none): its `section` column, else the last heading row above it. It
  is not part of the spec.
- Each report's datasets are `attr(, "datasets")` (named by output id;
  `NA` for none): its `datasets` column, "ADSL, ADAE" or "ADSL / ADAE" as
  `"ADSL | ADAE"`. Not part of the spec either.
- `attr(, "labels")` (the map's `label`: the ID as printed), `attr(, "first_titles")`, `attr(, "populations")`: for a report list to write the report's own tokens.

A study's define gives a report's code lists and its variables' labels,
by rule (Define-XML 2.0 / 2.1 through `xml2`; or the define spec workbook
through an import profile, Pinnacle 21's by default):

```r
def <- tfl_read_define("define.xml")   # or tfl_read_define_spec("define.xlsx")
cl <- tfl_define_codelists(def, c("SEX", "AGEGR1"), output_id = "T-14-1-1",
                           datasets = "ADSL")
attr(cl, "set_by_hand")   # what the define could not give
```

- The rows are the `codelists` sheet's (`value` = coded value, `label` =
  decode, `order` = order number) plus `note` (`define: CL.SEX`); with
  `labels = TRUE` (default) the variables' labels are `variables` sheet
  rows in `attr(, "variables")` (`output_id`, `variable`, `label`,
  `note`). `output_id` is required: code lists are per report.
- A variable with different code lists in two datasets is an error: give
  `datasets`. A company layout: `tfl_read_define_spec(path, map =
  list(variables = list(label = "Variable Label")))`.

### A.4 Listing spec

`tfl_listing_spec()`, `tfl_read_listing_spec()`, `tfl_write_listing_spec()`;
`tfl_listing_code()` writes the program, `tfl_listing()` makes the pages.

| Sheet | Columns |
|---|---|
| `listings` (one row a listing) | `output_id`, `type` (`multiline`), `dataset`, `where` (R), `sort` (`-` for descending), `max_rows`, `blank_row` (`TRUE` / `FALSE`), `wrap` (the name of an R function), `align` (the columns' default alignment: `left` / `center` / `right`), `record` (the hidden record column: `FALSE` none, a name for it) |
| `listing_cols` (one row a column) | `output_id`, `vars` (`|` stacks variables in one column), `label`, `width` (characters), `sep` (quoted to keep spaces: `" / "`), `align`, `collapse_repeats` |

Layout on the page is rtfreporter's `listing_spec()` / `as_rtftables()`.
`tfl_as_listing_spec(x, output_id, dataset = )` writes a `listing_spec()`
or a `plan_listing()` plan back as a listing spec, listing what the sheets
cannot carry (`attr(, "not_converted")`); from a plan it compares the RTF.
`tfl_read_data_code(datasets, dataset)` writes the line that reads a
dataset of the data catalog (`.rds`, `.xpt`, `.sas7bdat`, `.csv`,
`.parquet`).
