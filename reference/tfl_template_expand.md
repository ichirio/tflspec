# Expand a template into a report's items

The B use of a template SPEC: the items its parameters stand for, to
save in a report's YAML (each item marked `template:<name>`) or to run.

## Usage

``` r
tfl_template_expand(name, args = list(), codelists = NULL, population = NULL)
```

## Arguments

- name:

  The template's name (`"demog"`).

- args:

  Its parameters, a named list.

- codelists:

  The report's code lists (as
  [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  reads them), for the order and labels of values.

- population:

  The study's analysis sets (`data.yml`'s populations), for their
  labels; `NULL` for none.

## Value

A list: `ard`, `data`, `content` (a list of
[`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)).

## See also

[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md);
[`tpl_demog()`](https://ichirio.github.io/tflspec/reference/tpl_demog.md)
and
[`tpl_summary()`](https://ichirio.github.io/tflspec/reference/tpl_summary.md)
for the objects (the A use).

## Examples

``` r
x <- tfl_template_expand("demog", list(population = "SAF"))
vapply(x$content, function(it) it$fn, "")
#> [1] "table_plan"         "plan_cells"         "plan_labels"       
#> [4] "plan_levels"        "plan_paginate_rows"
```
