# shift: the template shift

The table template `shift`, layout `shift`. Its program is one call,
`tfl_tpl(ard, "shift", ...)`; a parameter a report leaves out takes the
study standard's value, else the variant's default, else the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `data` | other | call | ADLB |  | the dataset of records |
| `where` | other | call |  |  | the records kept (R): the visit, the parameters |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `analysis_id` | other | call | SHIFT |  | the analysis's id |
| `param` | other | call | PARAMCD |  | the parameter's variable (a block of rows each) |
| `baseline` | other | call | BNRIND |  | the category at baseline (the rows) |
| `post` | other | call | ANRIND |  | the category after it (the columns under each arm) |
| `levels` | other | call | LOW, NORMAL, HIGH |  | the categories, in order |
| `by_levels` | other | call |  |  | the column variable's values in order; NULL: the ARD's (its code list) |
| `param_labels` | other | call |  |  | each parameter's heading (a named vector); given by name, merged |
| `cell` | other | call | {n} |  | a cell's template |
| `heading` | other | call | Parameter / Baseline |  | the label column's heading |

## Example

    plan <- tfl_tpl(ard, "shift", population = "SAF", data_id = "adlb_shift", by_levels = c("Drug X", "Control"))

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("shift", tier = c("required", "usual"))
#> shift (table, layout shift; tflspec)
#> Call: tfl_tpl(ard, "shift", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
