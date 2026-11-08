# A report over the layers it extends

`tfl_resolve_output()` puts a report's own file over the layers its
`extends` names (`defaults`, `patterns/<name>`), nearest last. Maps
(`page`'s lines by number, `tokens`, `codelists`, a call's arguments)
merge by key, and an
[`tfl_unset()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
removes the key. In `content`, the report's items of one function
replace every item of that function in the layers below, in the place
the first of them had; a function the report does not have keeps the
layers' items; a function only the report has goes after them. The
`content` of `defaults` is a table's (normalize_ard, the digits ...): a
figure or a listing takes its other parts (`page`, `codelists`), not its
content. A pattern's content (`patterns/<name>`, chosen report by
report) layers onto any kind. `extends: none` uses no layer. The
report's style (`style: compact`, a set of `defaults.yml`'s `styles:`;
`standard` when it names none) is the nearest layer below its own: the
look it picks, content and page. A template SPEC
(`template: {name, args}`) puts the template's items
([`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md),
marked `template:<name>`) between the layers and the report's own – its
look (plan_style, the widths, the rows a page ...) under the study's
layers, which say the look and win – and its `ard:` under the report's
(an entry of the report's replaces the template's of the same id); a
template parameter that says where its study default is takes it from
the layers when the SPEC does not give it, and a SPEC value over a
different one says so. Each item's `from` says where it came from. The
result holds no `tfl_unset`.

## Usage

``` r
tfl_resolve_output(output, study, provenance = FALSE)
```

## Arguments

- output:

  A `tfl_output` as read from its file.

- study:

  A `tfl_study_spec` (its `defaults` and `patterns`).

- provenance:

  `TRUE` to say where each content item's arguments come from: each item
  gets `arg_from` (a named character vector, one an argument: `"own"`,
  `"defaults"` or `"pattern:<name>"`; a verb whose `...` is named, such
  as `plan_cells()`, by top-level key, any other verb one layer's whole)
  and `below` (the same verb's item with the report's own layer left
  out, `NULL` when no layer has it: what a revert goes back to). `FALSE`
  (default): the items as they are.

## Value

The `tfl_output`, resolved (attribute `resolved` = `TRUE`).

## Examples

``` r
d <- system.file("extdata", "spec-yaml", package = "tflspec")
s <- tfl_read_study_spec(d)
out <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], s)
vapply(out$content, function(it) paste(it$fn, it$from), "")
#>  [1] "table_plan own"           "plan_cells own"          
#>  [3] "plan_labels own"          "plan_digits own"         
#>  [5] "plan_col_header defaults" "plan_stub defaults"      
#>  [7] "plan_blanks defaults"     "plan_paginate_rows own"  
#>  [9] "plan_style defaults"      "plan_cell_style own"     
```
