# pk_conc: the template pk_conc

The table template `pk_conc`, layout `listing_summary`. Its program is
one call, `tfl_tpl(ard, "pk_conc", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADPC |  | the dataset of concentrations |
| `where` | other | call |  |  | the records kept (R), e.g. ANL01FL == "Y" |
| `by` | other | call | PARAM, TRT01A | settings.treatment | the groups: a page each (an analyte, an arm), the summary's too |
| `heading` | other | call |  |  | a page's name in the titles ({\<group\>}: its value); NULL: the groups' values, a line each; the last title line, or where the title line '{PAGE_HEADING}' is |
| `time` | other | call | ATPT |  | the time point's variable (the columns) |
| `time_order` | other | call | ATPTN |  | the time points' order variable; NULL: by name |
| `times` | other | call |  |  | the time points, in order; NULL: by time_order |
| `value` | other | call | AVAL |  | the concentration |
| `blq` | other | call |  |  | the records below the limit of quantification (R), e.g. BLQFL == "Y": the listing prints blq_label for them; the summary reads the values |
| `blq_label` | other | call | BLQ |  | what the listing prints for a BLQ record |
| `cell` | other | call | {value:.3s} |  | a subject's value (3 significant digits) |
| `subject` | other | call | {USUBJID}/{AGE}/{SEX} |  | a subject's row label ({\<variable\>}) |
| `subject_heading` | other | call | Subject ID/Age (years)/Gender |  | the subjects' column heading |
| `time_heading` | other | call | Time Relative to Dose |  | the heading over the time points |
| `summary_label` | other | call | Summary Statistics |  | the summary's heading row |
| `stats` | other | call | N = {N:.0f}; Mean = {mean:.4s}; SD = {sd:.5s}; SE = {se:.... |  | the summary's rows: label = cell |
| `split_at` | other | call |  |  | the time point that starts a second block of columns (each block its pages, the group's first); NULL: one |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \<data\>\_\<population\> |

## Example

    plan <- tfl_tpl(ard, "pk_conc", population = "SAF", blq = "BLQFL == \"Y\"")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("pk_conc", tier = c("required", "usual"))
#> pk_conc (table, layout listing_summary; tflspec)
#> Call: tfl_tpl(ard, "pk_conc", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
