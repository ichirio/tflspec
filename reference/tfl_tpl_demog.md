# Demographics: the template demog

The table template `demog`, layout `summary`: a variant of `summary`
(its defaults changed). Its program is one call,
`tfl_tpl(ard, "demog", ...)`; a parameter a report leaves out takes the
study standard's value, else the variant's default, else the layout's.

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
| `categorical` | usual | call | AGEGR1, SEX, RACE, ETHNIC |  | the categorical variables, in order |
| `labels` | usual | call | AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ... |  | each variable's heading; given by name, merged (RACE: ~ drops one) |
| `order` | other | call |  | plan_levels.variable | the variables' order (plan_levels(variable = )); NULL: the continuous, then the categorical |
| `cells` | usual | call | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}), {mean:.1f... | plan_cells.continuous | the rows of each continuous variable |
| `sd_missing` | other | call | \- | settings.sd_missing | what prints in place of the SD when a group has no SD (n = 1): {mean} (\<sd_missing\>); "" in study.yml or a report's YAML prints the mean alone; no effect when 'cells' is given |
| `count_cell` | other | call |  | plan_cells.categorical | a categorical value's cell, n (%) (plan_cells(categorical = )); NULL: the study standard's |
| `continuous_statistics` | other | call | N, mean, sd, median, min, max |  | the continuous statistics the ARD has |
| `categorical_statistics` | other | call | n, p |  | the categorical statistics the ARD has; NULL: cards' own |
| `total` | usual | call | FALSE | plan_total | TRUE: a Total column (each analysis again without 'by', plan_total()) |
| `stack` | other | call | FALSE |  | TRUE: the ARD as one cards::ard_stack() call |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "ars_demog", ...)`).

|             |            |        |              |
|-------------|------------|--------|--------------|
| Alias       | Collection | Item   | Title        |
| `ars_demog` | CDISC ARS  | 14-1-1 | Demographics |

Sources:

- CDISC ARS: Clinical Data Interchange Standards Consortium (CDISC).
  Analysis Results Standard, v1.0, 19 April 2024.
  <https://www.cdisc.org/standards/foundational/analysis-results-standard>
  (accessed 2026-10-28). Copyright CDISC. Its examples:
  github.com/cdisc-org/analysis-results-standard (content CC BY 4.0,
  code MIT); cited by number only. CDISC is a trademark of the Clinical
  Data Interchange Standards Consortium.

The aliases follow the numbering of these publications, and the titles
are short descriptions of our own, not their text. The templates are
tflspec's, not endorsed or recommended by CDISC.

## Example

    plan <- tfl_tpl(ard, "demog", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("demog", tier = c("required", "usual"))
#> demog (table, layout summary; variant of summary; tflspec)
#> Demographics
#> Call: tfl_tpl(ard, "demog", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#> 
#> usual (6):
#>   param       | effective (layer)                                                            | what
#>   by          | TRT01A (template)                                                            | the column variable
#>   continuous  | AGE (variant:demog)                                                          | the continuous variables, in order
#>   categorical | AGEGR1, SEX, RACE, ETHNIC (variant:demog)                                    | the categorical variables, in order
#>   labels      | AGE = Age (years); AGEGR1 = Age group, n (%); SEX = Sex, ... (variant:demog) | each variable's heading; given by name, merged (RACE: ~ drops one)
#>   cells       | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}), {mean:.1f... (template)      | the rows of each continuous variable
#>   total       | FALSE (template)                                                             | TRUE: a Total column (each analysis again without `by`, plan_total())
```
