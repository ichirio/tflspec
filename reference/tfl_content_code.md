# The code of a report's content, from its spec file

Writes the program a report's `content:` stands for – for a table, the
`table_plan()` pipeline: one line an item, in the items' order, with
their arguments as R takes them (`!r` as code, `{fn:, args:}` as the
call it names, maps and sequences as the vectors and lists each argument
takes;
[`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md)).
An item the report takes from the study's defaults or a pattern says so
in a comment above it. The output is the report after its `extends` are
applied
([`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)),
so no `!unset` is left.

## Usage

``` r
tfl_content_code(
  output,
  data = NULL,
  plan = "plan",
  pipe = NULL,
  template = c("call", "items"),
  study = NULL
)
```

## Arguments

- output:

  A report
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)),
  resolved.

- data:

  The name of the object the pipe starts from; `NULL`: the output's
  `data$object`, else `ard` when the content normalizes it (a
  `normalize_ard()` item), else `data`. A table with `data.steps` (its
  Rework, the old process_code: optional R after `normalize_ard()` and
  before `table_plan()`) starts with `data <- ard |> normalize_ard(...)`
  (the content's own item with its arguments; `data <- ard` when it has
  none; `ard` is the output's object), then the steps as they are
  written, which start from `data` and leave `data`, and the pipe starts
  from `data`. Steps that never name `data` are an error. A table of
  records (`source: dataset`) and the report's own data code
  (`source: code`) have their steps alone, as they are written.

- plan:

  The name the plan is assigned to.

- pipe:

  `"|>"`, `"%>%"` or `"rstudio"` (RStudio's Insert Pipe Operator);
  `NULL` follows `getOption("rtfreporter.ard_pipe")`, else `"|>"`.

- template:

  For a resolved template report (`template: {name, args}`,
  [`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)):
  `"call"` (default) writes the template's call,
  `plan <- tpl_<name>(ard, ...)` with the arguments its SPEC gives and
  the study's defaults it took, then the study's and the report's own
  lines after it, each marked; `"items"` writes the items it stands for,
  as for any report (and so does `"call"`, with a message, when the call
  cannot say the report's own table roles, data or normalization). A
  figure's template (`km_survival`): its data steps written out, then
  `content <- tfl_tpl(<data>, "<name>", ...)` and the report's own items
  `+`-ed after it; `"items"` (or a report that replaces one of the
  template's layers) writes its design.

- study:

  The study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md))
  or its folder; for a listing of records (`data: {source: dataset}`)
  the code starts with its data part, as
  [`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md)
  `part = "data"` writes it (the dataset read, its condition, code lists
  and order). `NULL`: the content only.

## Value

The code, one element a line.

## Details

A table's `plan_levels()` that says what its ARD already says is left
out, here and in
[`tfl_content_run()`](https://ichirio.github.io/tflspec/reference/tfl_content_run.md):
a column's order that is the report's code list (applied when the ARD is
made, a factor cards keeps in its order), and the variables in the order
of the ARD's analyses. An order of the report's own, a column no
analysis reads, a column an analysis that is not cards' reads (cardx's
survfit names the arms from the strata) and a report whose data steps
make its data keep theirs.

A report whose content is its own code (`content: !r`, any kind: the
code leaves `content`) is that code, after the data's steps. A figure's
content is its layers, as the figure designer has them
([`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
of the design the report stands for: its `data` steps and statistics,
the layers, its code lists): the figure in `content`, not saved.

## See also

[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md)
for a table workbook;
[`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md).

## Examples

``` r
out <- tfl_output("T-1", "table", content = list(
  tfl_item("table_plan", list(cols = "TRTA", rows = list(group = "variable"))),
  tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")),
  tfl_item("plan_sort", list(keys = c("-n")), from = "defaults")))
cat(tfl_content_code(out), sep = "\n")
#> plan <- table_plan(data, cols = "TRTA", rows = c(group = "variable")) |>
#>   plan_cells(categorical = "{n} ({p:.1f%})") |>
#>   # (study default)
#>   plan_sort("-n")
```
