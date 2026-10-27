# ae_pt_freq: the template ae_pt_freq

The table template `ae_pt_freq`, layout `hierarchical`. Its program is
one call, `tfl_tpl(ard, "ae_pt_freq", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `term` | other | call | AEDECOD |  | the term variable (the PT) |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `data` | other | call | ADAE |  | the dataset of records |
| `where` | other | call | TRTEMFL == "Y" |  | the records kept (R) |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `data_label` | other | call | Treatment-emergent AEs of the safety set |  | the analysis data's label |
| `analysis_id` | other | call | TEAE_PT |  | the analysis's id |
| `analysis_label` | other | call | TEAE by PT |  | the analysis's label |
| `overall` | other | call | Any TEAE |  | the any-record row's label; NULL: none |
| `min_pct` | other | call |  |  | the PTs kept: this % of subjects or more; NULL: all |
| `min_pct_in` | other | call | any |  | where min_pct is met: "any" column, "all" of them, or the "total" column |
| `sort_by` | other | call |  |  | the column (a value of 'by') whose counts order the PTs; NULL: the Total column with min_pct_in = "total", else the code list's first value of 'by', else all the columns |
| `heading` | other | call | Preferred Term |  | the label column's heading |

## Example

    plan <- tfl_tpl(ard, "ae_pt_freq", population = "SAF", min_pct = 5)

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_pt_freq", tier = c("required", "usual"))
#> ae_pt_freq (table, layout hierarchical; tflspec)
#> Call: tfl_tpl(ard, "ae_pt_freq", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
