# lab_abnormal: the template lab_abnormal

The table template `lab_abnormal`, layout `summary`. Its program is one
call, `tfl_tpl(ard, "lab_abnormal", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `data` | other | call | ADLB |  | the dataset of records |
| `params` | other | call |  |  | the PARAMCD values; NULL: all |
| `where` | other | call |  |  | the records kept (R), e.g. ONTRTFL == "Y" |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `param_order` | other | call | PARAMN |  | the parameters' order variable; NULL: by name |
| `category` | other | call | ANRIND |  | the category's variable (ANRIND, a criterion's MCRIT1ML) |
| `levels` | other | call |  |  | the values counted, in order (lowest first); NULL: the code list's |
| `cumulative` | other | call | FALSE |  | count a subject in every level up to their worst (level 2 or more ...) |
| `baseline_normal` | other | call | FALSE |  | only the subjects normal at baseline, in n and in the N |
| `baseline` | other | call | BNRIND |  | the category at baseline |
| `baseline_value` | other | call | NORMAL |  | the value of 'baseline' that is normal |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \<data\>\_\<population\> |
| `heading` | other | call | Parameter / Category |  | the label column's heading |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "stf_t20_kidney_level", ...)`).

|  |  |  |  |
|----|----|----|----|
| Alias | Collection | Item | Title |
| `stf_t20_kidney_level` | FDA STF IG v2 | T20 | Kidney function values above levels |
| `stf_t36_sbp_max` | FDA STF IG v2 | T36 | Maximum postbaseline systolic BP categories |
| `stf_t37_dbp_max` | FDA STF IG v2 | T37 | Maximum postbaseline diastolic BP categories |
| `stf_t38_hypotension` | FDA STF IG v2 | T38 | Postbaseline hypotension criteria |
| `stf_t52_chem_last_l2` | FDA STF IG v2 | T52 | Last on-treatment chemistry at level 2 or above |
| `stf_t53_hem_last_l2` | FDA STF IG v2 | T53 | Last on-treatment hematology at level 2 or above |
| `phuse_os_abnormal` | PhUSE CSS | OS table abnormal | Postbaseline values outside normal range |

Sources:

- FDA STF IG v2: U.S. Food and Drug Administration, CDER. Standard
  Safety Tables and Figures: Integrated Guide, Version 2.0, April 2025.
  <https://www.fda.gov/media/187065/download> (accessed 2026-10-28). A
  U.S. Government work; FDA's website content is in the public domain
  unless noted
  (https://www.fda.gov/about-fda/about-website/website-policies).

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
tflspec's, not endorsed or recommended by FDA or PhUSE.

## Example

    plan <- tfl_tpl(ard, "lab_abnormal", population = "SAF", levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\"")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("lab_abnormal", tier = c("required", "usual"))
#> lab_abnormal (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "lab_abnormal", ...)
#> 
#> required (1):
#>   param      | effective (layer) | what
#>   population | - (template)      | the analysis set's id (SAF)
```
