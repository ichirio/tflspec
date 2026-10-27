# ae_max_grade: the template ae_max_grade

The table template `ae_max_grade`, layout `hierarchical`. Its program is
one call, `tfl_tpl(ard, "ae_max_grade", ...)`; a parameter a report
leaves out takes the study standard's value, else the variant's default,
else the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `grade` | other | call | AESEV |  | the severity or grade variable (AESEV, AETOXGR) |
| `grade_levels` | other | call |  |  | its values, lowest first; NULL: the code list of 'grade' |
| `grade_in` | other | call | rows |  | the grades as "rows" under each term, or as "cols" in each group |
| `hierarchy` | other | call | AEBODSYS, AEDECOD |  | the levels, outer first |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `data` | other | call | ADAE |  | the dataset of records |
| `where` | other | call | TRTEMFL == "Y" |  | the records kept (R) |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `overall` | other | call | Any TEAE |  | the any-record row's label |
| `any_grade` | other | call | Any grade |  | the label of a term's count at any grade |
| `sort` | other | call | n |  | the SOCs and PTs by their count, most first ("n"), or by "name" |
| `heading` | other | call | System Organ Class, Preferred Term |  | the label column's heading, a line a level |

## Example

    plan <- tfl_tpl(ard, "ae_max_grade", population = "SAF", grade_levels = c("MILD", "MODERATE", "SEVERE"))

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_max_grade", tier = c("required", "usual"))
#> ae_max_grade (table, layout hierarchical; tflspec)
#> Call: tfl_tpl(ard, "ae_max_grade", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
