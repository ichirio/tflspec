# SPEC coverage of rtfreporter's API

How much of rtfreporter's exported API (rtfreporter@next) a tflspec SPEC
(tflspec@next: the YAML spec's `content:` and `page:`, the sheets' columns
and `content_more` / `page_more`) can write.  The table, one row per
exported function and argument, is `spec-coverage.csv` (columns
`function`, `argument`, `covered`, `where`, `note`); it is made by
`dev/spec_coverage.R` from the installed packages' formals and
`inst/spec/schema.yml`.

* **yes**: a SPEC writes it -- a content item the schema checks, a `page:`
  argument the report code passes on (`R/spec_report.R`), or a value
  tflspec fills itself (the pipe, the page's lines, the study's file).
* **partial**: it can be written, but not as the function's own argument
  (`rtf_tables(cell_valign = )` is `plan_style(cell_valign = )`), or a
  content item the schema has no entry for (no check, rank or help).
* **no**: a SPEC cannot write it.
* Notes `[helper]`: options, inspection helpers, code writers and file
  assemblers -- no SPEC item by design.  `[out of scope]`: `plan_layers()`,
  designed in another session.

Percentage: yes counts 1, partial 0.5.

| Run        | All 720 arguments | SPEC-relevant (636, helpers out) | Functions yes / partial / no |
|------------|-------------------|----------------------------------|------------------------------|
| 2026-10-28 before | 41.9% (287 / 30 / 403) | 47.5% (287 / 30 / 319) | 38 / 7 / 90 |
| 2026-10-28 after  | 43.3% (300 / 24 / 396) | 49.1% (300 / 24 / 312) | 40 / 6 / 89 |

## Added in this run

* `plan_empty(text, when, position, footnote)`: a schema entry (types,
  enums, rank 75 with the mapping, help), so a spec's
  `- plan_empty: {text: ..., position: replace}` is checked and placed.
* `rtf_document(watermark = rtf_watermark(...))`: `page.rtf_document.watermark`
  takes `{fn: rtf_watermark, args: {text: DRAFT, color: "#FF0000"}}` as
  well as a word; it was written into the program as a raw `structure()`
  before.  The sheets keep a word in the report sheet's `watermark` and a
  call in `page_more`.

## Where the gaps are

Most "no" rows are rtfreporter's lower-level table API -- `rtftable()`,
`widen_ard()`, `as_rtftable(s)()`, the `style_*()`, `set_*()`, `fmt_*()`,
`paginate*()` and `listing_*()` helpers -- which the plan verbs already
stand for: a SPEC says the plan, not these calls.  The gaps that a SPEC
could usefully close next:

1. The border constructors as values (`rtf_border_tfl()`, `rtf_border_box()`,
   `rtf_border_top()` ..., `rtf_table_border()`): a border argument takes
   only `{fn: rtf_border}` in the schema.
2. `rtf_tables()`: `col_spec`, `style` (`rtf_table_style()`),
   `read_attributes`, `read_gt` are not passed on from `page.rtf_tables`.
3. `rtf_section(page =, secinfo =)`, `rtf_document(color_table =)`,
   `generate_rtfreport(program =, report =)`, `rtfplot(width_twips =,
   height_twips =)`.
