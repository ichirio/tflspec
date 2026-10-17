# A workbook-shaped definition of how an ARD becomes a table

`tfl_table_spec()` validates the definition that
[`rtfreporter::widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.html)
accepts as `spec =`, and
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)
builds one from a workbook. It holds what would otherwise be repeated in
every script — which keys go across and down, the display label and
order of each variable, and the template and digits of every row — in
**one sheet per grain**, so that each fact is written once:

|               |                |                                         |
|---------------|----------------|-----------------------------------------|
| sheet         | one row per    | holds                                   |
| `study`       | study fact     | the rounding family (`key` / `value`)   |
| `tables`      | report         | the roles and the table-wide options    |
| `variables`   | variable       | display label, order, level order       |
| `cells`       | line of a cell | template, guard, digits                 |
| `digits`      | statistic      | its decimals, for every variable or one |
| `layout`      | report         | pages, groups, blank rows, stub         |
| `columns`     | printed column | width, row title, decimal split, hidden |
| `style`       | report         | border, row heights, font               |
| `cell_styles` | styling        | the cells chosen, bold, italic, colour  |
| `col_header`  | header cell    | line, columns, span, text, borders      |

The first four say how the ARD becomes a table data frame; the last four
how that becomes `rtftable` pages.
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
reads them all (`tfl_table_plan(data, )`), as the first layers of a
plan, so a verb written after it still wins.

## Usage

``` r
tfl_table_spec(
  tables = NULL,
  variables = NULL,
  cells = NULL,
  study = NULL,
  layout = NULL,
  columns = NULL,
  style = NULL,
  col_header = NULL,
  report = NULL,
  page = NULL,
  header = NULL,
  footer = NULL,
  titles = NULL,
  footnotes = NULL,
  cell_styles = NULL,
  tokens = NULL,
  digits = NULL
)
```

## Arguments

- tables, variables, cells, digits, layout, columns, style, col_header,
  cell_styles:

  Data frames with the columns above; missing columns are added as `NA`.

- study:

  The `study` sheet: a `key` / `value` frame, or a named vector such as
  `c(rounding = "sas")`.

- report, page, header, footer, titles, footnotes, tokens:

  The report sheets, as data frames with the columns
  [`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md)
  describes. `tables` may instead be a named list of the sheets, or an
  `tfl_table_spec` (returned as it is).

## Value

An object of class `tfl_table_spec`: a list of the sheets' data frames.

## `study`

What is **one for the whole study** by definition, as `key` / `value`
rows. Today that is `rounding` — `r` (half to even) or `sas` (half away
from zero), see
[`rtfreporter::round_num()`](https://ichirio.github.io/rtfreporter/reference/round_num.html)
— so that no two tables of one study can round differently. Blank leaves
it to `getOption("rtfreporter.rounding")`.

## One rule on the other sheets

Every sheet but `study` starts with `output_id`. **Blank means a
study-wide default; a row naming the report replaces the default row
with the same key** — the whole `tables` row for that report, the
`variables` row for that variable, the `cells` rows for that variable /
context / row. So one workbook can hold a house style and every report's
own changes to it.
[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)
narrows it to one report with `output_id =`.

## Page patterns

The report half (`report`, `page`, `header`, `footer`, `titles`,
`footnotes`, `tokens`) can hold **named page patterns** besides the
blank rows: rows whose `output_id` is `@` and a name (`@Compact`) — a
compact page for the PK tables, say. A pattern says only what differs
from the blank rows (Standard), by the same rule; a report chooses it
with `report$pattern` (blank: Standard). A report's own rows go over its
pattern's, its pattern's over Standard's, and `(none)` takes a line out
at either level. Standard's header, footer, tokens and font stay the
study's setup
([`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md));
what a pattern changes is written in the program of each report that
uses it.

Explicit
[`rtfreporter::widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.html)
arguments win over the spec, and the spec wins over the defaults.

Lists inside a cell are `|`-separated (`TRTA | SEX`). A column called
`note` is allowed on any sheet and never read; any other column a sheet
does not know is an error, so a mistyped header cannot become a setting
that silently never applies.

## `tables`

- `cols`:

  Column keys, outermost first: `TRTA | SEX`.

- `rows`:

  Row keys, in output order. `name = column` renames
  (`group1 = AEBODSYS`); a quoted value is a constant heading
  (`group1 = "Worst Post-Baseline Values"`).

- `label`:

  The row-label source, in the same notation (`label = AEDECOD`). Blank
  keeps `.label`; `NA` builds it to tell rows apart and then drops it;
  `NULL` leaves it out.

- `stats`, `value`, `na`:

  How a cell is filled, as
  [`rtfreporter::plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)
  takes them: `stats = rows` puts each statistic on a row of its own.

- `sort`:

  `TRUE`, `FALSE`, or the keys in order, `-` for descending:
  `.overall | group1 | .depth | -n | label`.

- `sort_stat`:

  The statistic totalled across the columns for a frequency order
  ([`rtfreporter::plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)'s
  `stat`).

- `sep`:

  What joins several column keys into one column's name (`Placebo____n`
  by default; `_` gives `Placebo_n`), the name the `columns` sheet
  refers to
  ([`rtfreporter::plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)'s
  `sep`).

- `header_n`:

  Which population a `col_header` text's `{n}` is, on pages split by a
  group value: `page` (each page's own — the subjects with that lab
  test) or `table` (the analysis set, the ARD rows without the page
  key). Several at once name their tokens: `n = page | N = table` gives
  `{n}` and `{N}`. Blank: the page's, with a warning when the ARD states
  both. See
  [`rtfreporter::plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)'s
  `values`.

- `total`:

  The heading of a **Total column** (`Total`): the ARD's overall rows —
  the analyses run again without their `by` (`analyses$overall`) — as
  one more column, its `{n}` the study total
  ([`rtfreporter::plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)).
  Blank: none. No `ARM = "Total"` in the data: the ARD and the ARS keep
  only what the data has.

- `total_position`:

  Where the Total column goes among the column key's values: `last`
  (blank) or `first`.

- `overall_label`:

  The row of a hierarchy's overall counts – the subjects with at least
  one event, from a `hierarchical` analysis with `args`
  `over_variables = TRUE` – and what it prints ("Subjects with at least
  one TEAE"). It is the normalization's
  ([`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)'s
  `overall`): the report's program writes it with
  [`tfl_normalize_code()`](https://ichirio.github.io/tflspec/reference/tfl_normalize_code.md).
  Blank: no such row.

- `stack`:

  Subgroups as one pair of keys, its layers after `=`:
  `subgroup = ALL, SEX, AGEGR1, AGEGR1 * SEX` (`*` crosses variables,
  `|` between several keys), or the strata slots,
  `subgroup = group2, group3`: the normalization's
  ([`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)'s
  `stack`), written by
  [`tfl_normalize_code()`](https://ichirio.github.io/tflspec/reference/tfl_normalize_code.md).
  `rows` then names `subgroup` and `subgroup_level`, whatever group slot
  each variable came in. Blank: none.

- `stack_sep`:

  What joins a crossed layer's names and levels (`AGEGR1 / SEX`,
  `65-80 / F`). Blank: `/`.

- `stack_nest`:

  `TRUE`: a crossed layer's outer levels in a key of their own,
  `<name>_outer`, its innermost in `<name>_level`, so `rows`
  `grp = subgroup | outer = subgroup_outer | lvl = subgroup_level` with
  a stub prints the outer level as a heading. Blank: `FALSE`.

- `compare`, `compare_by`, `compare_position`:

  Comparison columns
  ([`rtfreporter::plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)):
  their names (`p | diff`), each reading the ARD's comparison records –
  a test's p-value, a difference and its CI, the column key with no
  value – onto the row of their variable. A column's templates are the
  `cells` rows whose `variable` is its name (one a `context` when a test
  prints another way). `compare_by` splits the comparisons into columns,
  one a level; `compare_position` is `last` (blank) or `first`.

- `keep_where`, `keep_any`, `keep_parents`:

  Rows kept by a condition over the record
  ([`rtfreporter::plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)):
  one R condition in its guard language (`p >= 0.05`; several joined by
  `&`); `keep_any = FALSE` asks it of every column, not any;
  `keep_parents = drop` judges a parent row (a SOC) by its own counts
  instead of keeping it with its children. Blank: every row.

- `show_keys`:

  Row keys the table also shows as variables, `|` between them
  (`SEX | AGEGR1`): their own counts stay in the body, a block of their
  own beside a subgroup layer
  ([`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)'s
  `show_keys`). Blank: none.

- `stat_groups`:

  One record's statistics in named groups, `|` between groups, `,`
  between a group's statistics:
  `n = N | orr_ci = n, estimate, conf.low, conf.high`
  ([`rtfreporter::plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)).
  The key `.stat_group` then names them in `cols`
  (`TRT01A | .stat_group`: N in one column, the rate and CI in the
  next), and a `cells` row's `variable` may be a group's name. Blank:
  none.

## `variables`

- `variable`:

  An analysis variable, or any column key (`BGRADE`, `ATPT`, or the
  label column's own name).

- `label`:

  Display text replacing the variable's name.

- `order`:

  Number; the order the variables appear in
  (`plan_levels(variable = c("AGE", "SEX"))`: the values of the ARD's
  `variable` column; one not numbered comes after, in the ARD's order).

- `levels`:

  The order of its values: `Grade 0 | Grade 1 | Total`.

- `empty_levels`:

  `show` (blank, the default) or `hide`: whether its values that no
  record has – a code list's value, counted 0 in every column of an ARD
  made with the study's code lists – get a row
  (`plan_levels(.drop_empty = )`).

- `under`:

  Its rows under one level of another variable, `RACE: Asian` (that
  variable, a colon, the level as the table shows it): after that row,
  one stub indent deeper, its own heading dropped (`plan_nest()`); the
  sub-categories of a race from a second analysis on the same data.

## `cells`

- `variable`, `context`:

  What the row applies to. Either may be blank; both blank is the
  default for everything. `variable` may also be a context or kind
  (`continuous`, `categorical`), as a `cells` map key may.

- `row`:

  The row label (`Mean (SD)`). Blank means one row per level, labelled
  by the level.

- `when`:

  Optional guard, ordinary R over the statistics: `n == 0`. **Several
  rows with the same variable / context / row are one chain**, tried in
  sheet order; the first whose guard holds and whose template resolves
  wins.

- `template`:

  The cell recipe: `{mean} ({sd})`.

- `digits`:

  Decimal places for tokens that name none, per token when
  comma-separated: `1,2` for `{mean} ({sd})`.

- `signif`:

  Significant digits; wins over `digits`.

A token with no format and no `digits` takes its statistic's decimals
from the `digits` sheet.

For a `stats = rows` table (one statistic per row, the raw value in the
cell) a row with **no template** is instead that statistic's display
format: `row` names the statistic as the label column prints it (`N`,
`Mean`) and `digits` / `signif` say how many, for every value column
(`plan_digits(.rows = c(Mean = 2, SD = "3s"))`).

## `digits`

The decimals of each statistic, so that a template is written once
(`{mean} ({sd})`) and the decimals once:

- `variable`:

  Blank: every analysis variable (the table's rule). A variable: its
  exception, over the rule.

- `statistic`:

  The statistic, as the ARD names it: `mean`, `sd`, `p` (a percent: `1`
  prints `61.6`) ...

- `digits`:

  Its decimals, a whole number; or significant digits, `3s`
  (`{mean:.3s}`).

A token of a template that says its own format (`{mean:.2f}`) or the
row's `digits` win. A table of `tables$value = stat_fmt` prints the
ARD's own text (`stat_fmt`): the sheet does not apply to it.

## `layout`

One row per report, each column one argument of the plan verb its prefix
names:

- `pages_*`:

  [`rtfreporter::plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html):
  `max_rows`, `split`, `break_before` (row positions to cut before,
  `20 | 40`, with `split = rows`), `min_group_rows`, `cont_label`,
  `page_by` (BY pages: the column(s) partitioning the body first, the
  row budget inside each), `heading` (each BY page's name from a
  template, `Treatment: {value} (N={n})`).

- `group_*`:

  `mode` and `collapse` of
  [`rtfreporter::plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html).
  `group_page = TRUE` is
  [`rtfreporter::plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html):
  one page per value of `group_col` (blank: the outermost row key), and
  `group_keep = FALSE` leaves that column unprinted, and `group_heading`
  names each page from a template (`{value}`, `{n}`).

- `blank_*`:

  [`rtfreporter::plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html):
  `where`, `first`, `last`, `counted`.

- `stub_*`:

  [`rtfreporter::plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html):
  `vars`, `name`, `indent`, `summary`, `before`.

- `colpages_*`:

  [`rtfreporter::plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html):
  `every`, `at`, `cut_by` (a separator in the column names, `____`: one
  block per key), `keep`, `fit` (`TRUE`: every block on page 1's scale;
  `FALSE`: each column keeps its width), `allow_span_break`, `order`.

## `cell_styles`

One row per
[`rtfreporter::plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html),
in order; a report's own rows replace the default rows whole.

- `cols`:

  The columns styled, `|` between them (`.values` for every value
  column); blank, every column.

- `header`:

  `TRUE` styles the column header instead of the body.

- `where`:

  An R condition over the table's columns choosing the rows
  (`label == "Any TEAE"`, `is.na(label)`); blank, every row. A plan
  keeps one conditional rule per look, so two rows with a `where` may
  not set the same look – join their conditions with `|`.

- `bold`, `italic`, `underline`, `align`, `color`, `background`,
  `indent_twips`:

  The look: `TRUE` / `FALSE`, `left` / `center` / `right`, a colour
  (`#CC0000`), a left indent in twips (not on the header).

## `columns`

One row per printed column, by **name** — the finished table's, so a
folded stub is the name given to `stub_name`. `.values` stands for every
spread column, however many the data turned out to have.

`rel_width`

:   Relative width. Named columns win over `.values`; when widths are
    given, every printed column needs one.

`row_title`

:   `TRUE` for a row-heading column.

`decimal_split`

:   `TRUE` to line up the decimal points
    ([`rtfreporter::set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.html)).

`hide`

:   `TRUE` to use the column without printing it.

## `style`

One row per report, each column an argument of
[`rtfreporter::plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html):
`border`, `align_count_pct`, `font`, `font_size_half_points`,
`row_height_twips`, `row_height_exact`, `header_row_height_twips`,
`blank_row_height_twips`, `cell_padding_left_twips`,
`cell_padding_right_twips`, `cell_valign`, `table_align`, `markup`,
`blank_row_normalize`, and the rules of one kind of row,
`border_header`, `border_spanning`, `border_body`, `border_first_row`,
`border_last_row`: the sides drawn (`top | bottom`) or `none`, as
[`rtfreporter::rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.html)
takes them. `border` and the `border_*` columns are one or the other.
The table's default look, `header_align`, `header_bold`,
`header_italic`, `align`, `bold`, `italic`, `underline`
([`rtfreporter::rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.html)'s
fields, made into one style with the `border_*` columns), and its width,
`table_width_twips`, `table_width_pct`, `table_width_pct_of_writable`.
`auto_width` is
[`rtfreporter::plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)'s
and `col_header_align`
[`rtfreporter::plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)'s.

Values are checked where they are written: a number, `TRUE` / `FALSE` or
a `|`-list, as the column needs. Quote a text value (`" (Cont.)"`) to
keep its leading or trailing spaces.

## `col_header`

**One row per header cell**; `line` 1 is the top row. A report's own
cells replace the default header whole.

- `cols`:

  The columns the cell sits over, `|`-separated: a column name,
  `.values` (every spread column), a position or range (`3`, `3:31`,
  `3:last`), or `KEY = value` — the spread columns whose column key
  `KEY` has that value (`variable = n`).

- `span`:

  Blank: one cell over all of `cols`. `each`: one cell per column. A
  column key (`TRTA`): one cell per value of that key, over its columns
  — an arm's spanner, however many arms.

- `text`:

  The label. A line break is Alt+Enter or `\\n`. The tokens of
  [`rtfreporter::plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.html)
  work: `{col}` (the column's own value), `{col1}`, `{col2}` (its keys,
  outermost first), `{n}` (the population of what the cell stands for:
  its column, or over an arm's spanner the arm), `{n1}`, `{n2}` (the
  population at that depth of the keys) and `{n:sum}` (the total over
  the cell's columns). `{n}` is read from the ARD whenever a text uses
  it; a number the ARD does not state prints `NA` with a warning, and
  `plan_col_header(values = )` after
  [`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
  supplies it. Quote a text to keep leading spaces: `" Category"`.

- `align`, `bold`, `border_top`, `border_bottom`:

  As
  [`rtfreporter::col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.html)
  /
  [`rtfreporter::rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.html)
  take them (`single`, `none`, ...).

## Reserved for the rest of the report

A later version will read the sheet `figures` under the same `output_id`
rule, so the whole RTF deliverable can be defined in one workbook. They
are reported, not refused, when present today. An `about` sheet (`key` /
`value`) may state `spec_version`; sheets whose name starts with `_` are
ignored.

## The table engine

The ARD functions and the plan are rtfreporter's:
[`help("ard-tables", package = "rtfreporter")`](https://ichirio.github.io/rtfreporter/reference/ard-tables.html).

## See also

[`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md),
[`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md),
[`tfl_table_spec_template()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec_template.md)
