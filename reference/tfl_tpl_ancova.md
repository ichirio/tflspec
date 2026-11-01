# ancova: the template ancova

The table template `ancova`, layout `summary`. Its program is one call,
`tfl_tpl(ard, "ancova", ...)`; a parameter a report leaves out takes the
study standard's value, else the variant's default, else the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | required | call |  |  | the BDS dataset (ADVS, ADLB) |
| `param` | required | call |  |  | the PARAMCD (SYSBP) |
| `visit` | required | call |  |  | the AVISIT (CYCLE 9 DAY 1) |
| `by` | other | call | TRT01A | settings.treatment | the arm variable (the columns) |
| `response` | other | call | CHG |  | the response variable |
| `covariates` | other | call | BASE |  | the model's covariates besides the arm |
| `where` | other | call |  |  | the records kept besides the parameter and visit (R) |
| `ref` | other | call |  |  | the reference arm; NULL: the last arm in the code list's order |

## Example

    plan <- tfl_tpl(ard, "ancova", population = "SAF", data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ancova", tier = c("required", "usual"))
#> ancova (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "ancova", ...)
#> 
#> required (4):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#>   data       | - (template)      | the BDS dataset (ADVS, ADLB)
#>   param      | - (template)      | the PARAMCD (SYSBP)
#>   visit      | - (template)      | the AVISIT (CYCLE 9 DAY 1)
```
