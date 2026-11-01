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
| `worst` | other | call | HIGH, LOW, NORMAL |  | a subject once a parameter, at its worst category after baseline, the categories from the worst; NULL: every record (the baseline's records too) |
| `missing_label` | other | call | Missing |  | the row of the subjects with no baseline category; NULL: none |
| `total_label` | other | call | Total |  | the row of all the subjects; NULL: none |
| `param_page` | other | call | TRUE |  | TRUE: a page a parameter, named in the titles by 'param_heading' |
| `param_heading` | other | call | Parameter: {value} |  | a parameter's page name ('{value}': the parameter) |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "phuse_os_shift", ...)`).

|                  |            |                |                               |
|------------------|------------|----------------|-------------------------------|
| Alias            | Collection | Item           | Title                         |
| `phuse_os_shift` | PhUSE CSS  | OS table shift | Shift from normal to abnormal |

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

The aliases follow the numbering of these publications, and the titles
are short descriptions of our own, not their text. The templates are
tflspec's, not endorsed or recommended by PhUSE.

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
