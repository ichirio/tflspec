# hierarchical: the template hierarchical

The table template `hierarchical`, layout `hierarchical`. Its program is
one call, `tfl_tpl(ard, "hierarchical", ...)`; a parameter a report
leaves out takes the study standard's value, else the variant's default,
else the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `hierarchy` | required | call |  |  | the levels, outer first |
| `data` | other | call | ADAE |  | the dataset of records |
| `where` | usual | call |  |  | the records kept (R) |
| `by` | usual | call | TRT01A | settings.treatment | the column variable |
| `any_row` | usual | call | FALSE |  | TRUE: the row of the subjects with any record, first |
| `any_row_label` | other | call | Any event |  | that row's label |
| `heading` | usual | call | System Organ Class, Preferred Term |  | the label column's heading, a line a level |
| `sort` | other | call | .overall, group1, .depth, -n, label | plan_sort | the rows' order (plan_sort()) |
| `min_pct` | usual | call |  | plan_filter | the rows kept: this % of subjects or more (plan_filter()); NULL: all |
| `min_pct_in` | other | call | any |  | where min_pct is met: "any" column, "all" of them, or the "total" column |
| `count_cell` | other | call |  | plan_cells.categorical | a count's cell, n (%) (plan_cells(categorical = )); NULL: the study standard's |
| `denominator` | other | call |  |  | the percentages' denominator: NULL the analysis set's subjects, "data" the records' subjects |
| `total` | other | call | FALSE | plan_total | TRUE: a Total column (the analysis again without 'by', plan_total()) |

## Example

    plan <- tfl_tpl(ard, "hierarchical", population = "SAF", hierarchy = c("AEBODSYS", "AEDECOD"))

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("hierarchical", tier = c("required", "usual"))
#> hierarchical (table, layout hierarchical; tflspec)
#> Call: tfl_tpl(ard, "hierarchical", ...)
#> 
#> required (2):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#>   hierarchy  | - (template)      | the levels, outer first
#> 
#> usual (5):
#>   param   | effective (layer)                                | what
#>   where   | - (template)                                     | the records kept (R)
#>   by      | TRT01A (template)                                | the column variable
#>   any_row | FALSE (template)                                 | TRUE: the row of the subjects with any record, first
#>   heading | System Organ Class,    Preferred Term (template) | the label column's heading, a line a level
#>   min_pct | - (template)                                     | the rows kept: this % of subjects or more (plan_filter()); NULL: all
```
