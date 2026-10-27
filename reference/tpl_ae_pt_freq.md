# Adverse events by preferred term, most frequent first: the template `ae_pt_freq`

A display template on the `hierarchical` layout with one level: the
treatment-emergent events' preferred terms (`AEDECOD`), the subjects
with any event first, then the terms by decreasing frequency – in the
first group of the code list of `by`, or the column `sort_by` names –
and, with `min_pct`, only the terms of that percentage of subjects or
more: in any column (`min_pct_in = "any"`, as FDA's tables of common
adverse events), in every column (`"all"`) or in the Total column
(`"total"`).

## Usage

``` r
tpl_ae_pt_freq(ard, population, ..., codelists = NULL)
```

## Arguments

- ard:

  The normalized ARD's source: the study ARD's rows of this report.

- population:

  The analysis set's id (`"SAF"`).

- ...:

  The template's other parameters: `term`, `by`, `where`, `overall`,
  `min_pct`, `min_pct_in`, `sort_by`, `heading` ...

- codelists:

  The report's code lists, for the order and labels of values; `NULL`
  takes the ARD's.

## Value

A
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html).

## Examples

``` r
tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 5))$content
#> [[1]]
#> <tfl_item> normalize_ard(hierarchy, overall)  # template:ae_pt_freq
#> 
#> [[2]]
#> <tfl_item> table_plan(cols, label)  # template:ae_pt_freq
#> 
#> [[3]]
#> <tfl_item> plan_sort(keys)  # template:ae_pt_freq
#> 
#> [[4]]
#> <tfl_item> plan_filter(where)  # template:ae_pt_freq
#> 
#> [[5]]
#> <tfl_item> plan_col_header(header)  # template:ae_pt_freq
#> 
#> [[6]]
#> <tfl_item> plan_paginate_rows(max_rows)  # template:ae_pt_freq
#> 
```
