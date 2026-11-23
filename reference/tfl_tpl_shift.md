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
| `data_id` | other | call |  |  | the analysis data's id; NULL: \<data\>\_\<population\> |
| `analysis_id` | other | call | SHIFT |  | the analysis's id |
| `param` | other | call | PARAMCD |  | the parameter's variable (a block of rows each) |
| `baseline` | other | call | BNRIND |  | the category at baseline (the rows) |
| `post` | other | call | ANRIND |  | the category after it (the columns under each arm) |
| `levels` | other | call | LOW, NORMAL, HIGH |  | the categories, in order |
| `by_levels` | other | call |  |  | the column variable's values in order; NULL: the ARD's (its code list) |
| `param_labels` | other | call |  |  | each parameter's heading (a named vector); given by name, merged |
| `cell` | other | call | {n} |  | a cell's template |
| `heading` | other | call | Parameter / Baseline |  | the label column's heading |
| `post_header` | other | call | {col} |  | the header over each post-baseline category ('{col}': the category; a typed \n breaks the line) |
| `worst` | other | call | HIGH, LOW, NORMAL |  | a subject once a parameter, at its worst category after baseline, the categories from the worst; NULL: every record (the baseline's records too) |
| `worst_by` | other | call |  |  | a parameter's own order from the worst, by its value (HGB: LOW, HIGH, NORMAL); the others take 'worst' |
| `percent_of` | other | call | total |  | what a cell's {p} is of, set apart from the header N ('header_n'): "total" (default) the Total row – the arm's subjects with both a baseline and a category after baseline for the parameter, so the Total is 100%; "row" the subjects of its baseline category |
| `missing_label` | other | call |  |  | the row of the subjects with no baseline category, counted in the Total and in the % denominator; NULL (default): those subjects are left out of the table, not in any row or N |
| `total_label` | other | call | Total |  | the row of all the subjects; NULL: none |
| `param_page` | other | call | TRUE |  | TRUE: a page a parameter, named in the titles by 'param_heading' |
| `param_heading` | other | call | Parameter: {value} |  | a parameter's page name ('{value}': the parameter); the last title line, or where the title line '{PAGE_HEADING}' is |
| `header_n` | other | call | set |  | the arm headings' N, set apart from the % denominator ('percent_of'): "set" (default): the arm's subjects in the analysis set (with 'subset'); "tested": the page's subjects with a baseline and a post-baseline category (needs 'param_page'); "none": no N |

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
