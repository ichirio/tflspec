# lab_abnormal: the template lab_abnormal

The table template `lab_abnormal`, layout `summary`. Its program is one
call, `tfl_tpl(ard, "lab_abnormal", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADLB |  | the dataset of records |
| `params` | other | call |  |  | the PARAMCD values; NULL: all |
| `where` | other | call |  |  | the records kept (R), e.g. ONTRTFL == "Y" |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `param_order` | other | call | PARAMN |  | the parameters' order variable; NULL: by name |
| `category` | other | call | ANRIND |  | the category's variable (ANRIND, a criterion's MCRIT1ML) |
| `levels` | other | call |  |  | the values counted, in order (lowest first); NULL: the code list's |
| `cumulative` | other | call | FALSE |  | count a subject in every level up to their worst (level 2 or more ...) |
| `baseline_normal` | other | call | FALSE |  | only the subjects normal at baseline, in n and in the N |
| `baseline` | other | call | BNRIND |  | the category at baseline |
| `baseline_value` | other | call | NORMAL |  | the value of 'baseline' that is normal |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `heading` | other | call | Parameter / Category |  | the label column's heading |

## Example

    plan <- tfl_tpl(ard, "lab_abnormal", population = "SAF", levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\"")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("lab_abnormal", tier = c("required", "usual"))
#> lab_abnormal (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "lab_abnormal", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
