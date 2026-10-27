# lab_shift: the template lab_shift

The table template `lab_shift`, layout `shift`. Its program is one call,
`tfl_tpl(ard, "lab_shift", ...)`; a parameter a report leaves out takes
the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `visit` | required | call |  |  | the visit (AVISIT) |
| `params` | required | call |  |  | the PARAMCD values |
| `where` | other | call |  |  | the records kept besides the visit and parameters (R) |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `by_levels` | other | call |  |  | the column variable's values in order; NULL: the ARD's (its code list) |
| `param_labels` | other | call |  |  | each parameter's heading (a named vector); given by name, merged |
| `levels` | other | call | LOW, NORMAL, HIGH |  | the categories, in order |

## Example

    plan <- tfl_tpl(ard, "lab_shift", population = "SAF", visit = "CYCLE 2 DAY 1", params = c("ALT", "AST"))

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("lab_shift", tier = c("required", "usual"))
#> lab_shift (table, layout shift; tflspec)
#> Call: tfl_tpl(ard, "lab_shift", ...)
#> 
#> required (3):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
#>   visit      | - (template)      | the visit (AVISIT)
#>   params     | - (template)      | the PARAMCD values
```
