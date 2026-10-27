# param_change: the template param_change

The table template `param_change`, layout `summary`. Its program is one
call, `tfl_tpl(ard, "param_change", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | required | call |  |  | the BDS dataset (ADLB, ADVS, ADEG) |
| `params` | other | call |  |  | the PARAMCD values, in the data's order; NULL: all |
| `visits` | other | call |  |  | the AVISIT values; NULL: all |
| `where` | other | call |  |  | the records kept (R), e.g. ANL01FL == "Y" |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `layout` | other | call | cols |  | cols: each arm's value and change side by side; rows: the value's rows, then the change's, under each visit |
| `param_order` | other | call | PARAMN |  | the parameters' order variable; NULL: by name |
| `visit_order` | other | call | AVISITN |  | the visits' order variable; NULL: by name |
| `baseline` | other | call | FALSE |  | TRUE: the baseline's rows (BASE) before the value's |
| `baseline_label` | other | call | Baseline |  | the baseline's heading |
| `value_label` | other | call | Value |  | the value's heading |
| `change_label` | other | call | Change from baseline |  | the change's heading |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `data_label` | other | call |  |  | the analysis data's label |
| `statistics` | other | call | N, mean, sd, median, min, max |  | the statistics the ARD has |
| `cells` | other | call | n = {N:.0f}; Mean (SD) = {mean:.1f} ({sd:.2f}); Median = ... | plan_cells.continuous | the rows of a value or a change |

## Example

    plan <- tfl_tpl(ard, "param_change", population = "SAF", data = "ADLB", params = c("ALT", "AST"), where = "ANL01FL == \"Y\"")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("param_change", tier = c("required", "usual"))
#> param_change (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "param_change", ...)
#> 
#> required (2):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#>   data       | - (template)      | the BDS dataset (ADLB, ADVS, ADEG)
```
