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
tfl_content_code(output, data = NULL, plan = "plan", pipe = NULL)
```

## Arguments

- output:

  A report
  ([`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)),
  resolved.

- data:

  The name of the object the pipe starts from; `NULL`: the output's
  `data$object`, else `ard` when the content normalizes it (a
  `normalize_ard()` item), else `data`. With `data.steps` (R that makes
  `data` from the output's object, the old process_code), the steps come
  first, as they are written, and the pipe starts from `data`.

- plan:

  The name the plan is assigned to.

- pipe:

  `"|>"` or `"%>%"`; `NULL` follows `getOption("rtfreporter.ard_pipe")`.

## Value

The code, one element a line.

## Details

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
#> plan <- table_plan(data, cols = "TRTA", rows = c(group = "variable")) %>%
#>   plan_cells(categorical = "{n} ({p:.1f%})") %>%
#>   # (study default)
#>   plan_sort("-n")
```
