# Demographics, after FDA STF IG v2 Table 2: the template stf_t02_demog

The table template `stf_t02_demog`, layout `summary`: a variant of
`demog` (its defaults changed). Its program is one call,
`tfl_tpl(ard, "stf_t02_demog", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's. It follows the shape of a table in FDA's Standard Safety
Tables and Figures Integrated Guide (version 2); it is not endorsed or
recommended by FDA.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADSL |  | the dataset |
| `where` | other | call |  |  | the records kept (R), for a dataset other than ADSL |
| `by` | usual | call | TRT01A | settings.treatment | the column variable |
| `continuous` | usual | call | AGE |  | the continuous variables, in order |
| `categorical` | usual | call | SEX, AGEGR1, RACE, ETHNIC, COUNTRY |  | the categorical variables, in order |
| `labels` | usual | call | SEX = Sex, n (%); AGE = Age, years; AGEGR1 = Age groups (... |  | each variable's heading; given by name, merged (RACE: ~ drops one) |
| `order` | other | call | SEX, AGE, AGEGR1, RACE, ETHNIC, COUNTRY | plan_levels.variable | the variables' order (plan_levels(variable = )); NULL: the continuous, then the categorical |
| `cells` | usual | call | Mean (SD) = {mean:.1f} ({sd:.2f}); Median (min, max) = {m... | plan_cells.continuous | the rows of each continuous variable |
| `count_cell` | other | call |  | plan_cells.categorical | a categorical value's cell, n (%) (plan_cells(categorical = )); NULL: the study standard's |
| `continuous_statistics` | other | call | N, mean, sd, median, min, max |  | the continuous statistics the ARD has |
| `categorical_statistics` | other | call | n, p |  | the categorical statistics the ARD has; NULL: cards' own |
| `total` | usual | call | TRUE | plan_total | TRUE: a Total column (each analysis again without 'by', plan_total()) |
| `stack` | other | call | FALSE |  | TRUE: the ARD as one cards::ard_stack() call |

## Example

    plan <- tfl_tpl(ard, "stf_t02_demog", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("stf_t02_demog", tier = c("required", "usual"))
#> stf_t02_demog (table, layout summary; variant of demog; tflspec)
#> Demographics, after FDA STF IG v2 Table 2
#> It follows the shape of a table in FDA's Standard Safety Tables and Figures Integrated Guide (version 2); it is not endorsed or recommended by FDA.
#> Call: tfl_tpl(ard, "stf_t02_demog", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#> 
#> usual (6):
#>   param       | effective (layer)                                                                    | what
#>   by          | TRT01A (template)                                                                    | the column variable
#>   continuous  | AGE (variant:demog)                                                                  | the continuous variables, in order
#>   categorical | SEX, AGEGR1, RACE, ETHNIC, COUNTRY (variant:stf_t02_demog)                           | the categorical variables, in order
#>   labels      | SEX = Sex, n (%); AGE = Age, years; AGEGR1 = Age groups (... (variant:stf_t02_demog) | each variable's heading; given by name, merged (RACE: ~ drops one)
#>   cells       | Mean (SD) = {mean:.1f} ({sd:.2f}); Median (min, max) = {m... (variant:stf_t02_demog) | the rows of each continuous variable
#>   total       | TRUE (variant:stf_t02_demog)                                                         | TRUE: a Total column (each analysis again without `by`, plan_total())
```
