# A template report written out as its items

The B use of a report's template SPEC (`template: {name, args}` in its
YAML file): the template's items – with the study's defaults for the
parameters that take them, as
[`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)
gives them – merged with the report's own `ard:`, `data:` and
`content:`, and the `template:` taken out. Each item the template made
keeps the mark `from = "template:<name>"`. The report is then a code
SPEC: later changes of the template do not reach it.

## Usage

``` r
tfl_template_to_spec(output, study)
```

## Arguments

- output:

  A `tfl_output` as read from its file
  ([`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)).

- study:

  Its study
  ([`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)).

## Value

The `tfl_output`, unresolved (its `extends` kept): write it with
[`tfl_write_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md).
A report with no template SPEC comes back as it is.

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
s <- tfl_read_study_spec(d)
o <- tfl_output("T-9", "table", population = "SAF",
                template = list(name = "demog", args = list(population = "SAF")))
b <- tfl_template_to_spec(o, s)
vapply(b$content, function(it) it$fn, "")
#> [1] "table_plan"         "normalize_ard"      "plan_cells"        
#> [4] "plan_labels"        "plan_paginate_rows"
```
