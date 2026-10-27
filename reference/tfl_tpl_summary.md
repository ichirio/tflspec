# summary: the template summary

The table template `summary`, layout `summary`. Its program is one call,
`tfl_tpl(ard, "summary", ...)`; a parameter a report leaves out takes
the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADSL |  | the dataset |
| `where` | other | call |  |  | the records kept (R), for a dataset other than ADSL |
| `by` | usual | call | TRT01A | settings.treatment | the column variable |
| `continuous` | usual | call |  |  | the continuous variables, in order |
| `categorical` | usual | call |  |  | the categorical variables, in order |
| `labels` | usual | call |  |  | each variable's heading; given by name, merged (RACE: ~ drops one) |
| `order` | other | call |  | plan_levels.variable | the variables' order (plan_levels(variable = )); NULL: the continuous, then the categorical |
| `cells` | usual | call | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}); Median = ... | plan_cells.continuous | the rows of each continuous variable |
| `count_cell` | other | call |  | plan_cells.categorical | a categorical value's cell, n (%) (plan_cells(categorical = )); NULL: the study standard's |
| `continuous_statistics` | other | call | N, mean, sd, median, min, max |  | the continuous statistics the ARD has |
| `categorical_statistics` | other | call | n, p |  | the categorical statistics the ARD has; NULL: cards' own |
| `total` | usual | call | FALSE | plan_total | TRUE: a Total column (each analysis again without 'by', plan_total()) |
| `stack` | other | call | FALSE |  | TRUE: the ARD as one cards::ard_stack() call |

## Example

    plan <- tfl_tpl(ard, "summary", population = "SAF", continuous = "AGE")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("summary", tier = c("required", "usual"))
#> summary (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "summary", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#> 
#> usual (6):
#>   param       | effective (layer)                                                       | what
#>   by          | TRT01A (template)                                                       | the column variable
#>   continuous  | - (template)                                                            | the continuous variables, in order
#>   categorical | - (template)                                                            | the categorical variables, in order
#>   labels      | - (template)                                                            | each variable's heading; given by name, merged (RACE: ~ drops one)
#>   cells       | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}); Median = ... (template) | the rows of each continuous variable
#>   total       | FALSE (template)                                                        | TRUE: a Total column (each analysis again without `by`, plan_total())
```
