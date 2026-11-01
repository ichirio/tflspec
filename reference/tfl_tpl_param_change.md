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
| `param_page` | other | call | TRUE |  | TRUE: a page a parameter, named in the titles by 'param_heading' |
| `param_heading` | other | call | Parameter: {value} |  | a parameter's page name ('{value}': the parameter) |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "phuse_ct_summary", ...)`).

|  |  |  |  |
|----|----|----|----|
| Alias | Collection | Item | Title |
| `phuse_ct_summary` | PhUSE CSS | CT table | Observed values and change by visit |
| `ars_vs_chg_h` | CDISC ARS | 14-3-3-1a | Vital signs values and change horizontal |
| `ars_vs_chg_v` | CDISC ARS | 14-3-3-1b | Vital signs values and change vertical |

Sources:

- PhUSE CSS: PhUSE CSS Development of Standard Scripts for Analysis and
  Programming Working Group. Analyses and Displays Associated with
  Measures of Central Tendency: Focus on Vital Sign, Electrocardiogram,
  and Laboratory Analyte Measurements in Phase 2-4 Clinical Trials and
  Integrated Submission Documents (white paper), Version 1.0, 10
  October 2013.
  <https://github.com/phuse-org/phuse-scripts/tree/master/whitepapers>
  (accessed 2026-10-28). Copyright PHUSE Limited, all rights reserved;
  cited by number only. Its standard scripts: phuse-org/phuse-scripts,
  MIT licence.

- PhUSE CSS: PhUSE CSS Standard Analyses Working Group. Analyses and
  Displays Associated with Outliers or Shifts from Normal to Abnormal:
  Focus on Vital Signs, Electrocardiogram, and Laboratory Analyte
  Measurements in Phase 2-4 Clinical Trials and Integrated Summary
  Documents (white paper), Version 1.0, 10 September 2015.
  <https://github.com/phuse-org/phuse-scripts/tree/master/whitepapers>
  (accessed 2026-10-28). Copyright PHUSE Limited, all rights reserved;
  cited by number only.

- PhUSE CSS: PhUSE CSS Standard Analyses Working Group. Analysis and
  Displays Associated with Adverse Events: Focus on Adverse Events in
  Phase 2-4 Clinical Trials and Integrated Summary Documents (white
  paper), Version 1.0, 3 February 2017.
  <https://phuse.global/Deliverables> (accessed 2026-10-28). Copyright
  PHUSE Limited, all rights reserved; cited by number only.

- PhUSE CSS: PhUSE CSS Standard Analyses Working Group. Analyses and
  Displays Associated with Demographics, Disposition, and Medications in
  Phase 2-4 Clinical Trials and Integrated Summary Documents (white
  paper), Version 2.0, 2 March 2018. <https://phuse.global/Deliverables>
  (accessed 2026-10-28). Copyright PHUSE Limited, all rights reserved;
  cited by number only.

- CDISC ARS: Clinical Data Interchange Standards Consortium (CDISC).
  Analysis Results Standard, v1.0, 19 April 2024.
  <https://www.cdisc.org/standards/foundational/analysis-results-standard>
  (accessed 2026-10-28). Copyright CDISC. Its examples:
  github.com/cdisc-org/analysis-results-standard (content CC BY 4.0,
  code MIT); cited by number only. CDISC is a trademark of the Clinical
  Data Interchange Standards Consortium.

The aliases follow the numbering of these publications, and the titles
are short descriptions of our own, not their text. The templates are
tflspec's, not endorsed or recommended by PhUSE or CDISC.

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
