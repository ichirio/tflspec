# ae_eair: the template ae_eair

The table template `ae_eair`, layout `hierarchical`. Its program is one
call, `tfl_tpl(ard, "ae_eair", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADAE |  | the dataset of records |
| `where` | other | call | TRTEMFL == "Y" |  | the records kept (R) |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `hierarchy` | other | call | AEBODSYS, AEDECOD |  | the levels, outer first |
| `exposure` | other | call | TRTDURD |  | the ADSL variable of the days of exposure |
| `start` | other | call | ASTDY |  | the event's day from the start of treatment: a variable or R (ASTDT - TRTSDT + 1) |
| `per` | other | call | 100 |  | the rate per this many person-years |
| `overall` | other | call | Any TEAE |  | the any-record row's label |

## Example

    plan <- tfl_tpl(ard, "ae_eair", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_eair", tier = c("required", "usual"))
#> ae_eair (table, layout hierarchical; tflspec)
#> Call: tfl_tpl(ard, "ae_eair", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
