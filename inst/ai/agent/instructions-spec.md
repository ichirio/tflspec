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
  without them takes the study's default -- do not ask;
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
person decides in the builder), as AGENTS.md shows.
