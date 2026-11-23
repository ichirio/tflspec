# Adverse events by SOC and PT (FDA STF Table 14): the template ae_soc_pt

The table template `ae_soc_pt`, layout `hierarchical`: a variant of
`hierarchical` (its defaults changed). Its program is one call,
`tfl_tpl(ard, "ae_soc_pt", ...)`; a parameter a report leaves out takes
the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | required | call |  | output.population | the analysis set's id (SAF) |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `hierarchy` | required | call | AEBODSYS, AEDECOD |  | the levels, outer first |
| `data` | other | call | ADAE |  | the dataset of records |
| `where` | usual | call | TRTEMFL == "Y" |  | the records kept (R) |
| `by` | usual | call | TRT01A | settings.treatment | the column variable |
| `any_row` | usual | call | TRUE |  | TRUE: the row of the subjects with any record, first |
| `any_row_label` | other | call | Any TEAE |  | that row's label |
| `heading` | usual | call | System Organ Class, Preferred Term |  | the label column's heading, a line a level |
| `sort` | other | call | .overall, group1, .depth, -n, label | plan_sort | the rows' order (plan_sort()) |
| `min_pct` | usual | call |  | plan_filter | the rows kept: this % of subjects or more (plan_filter()); NULL: all |
| `min_pct_in` | other | call | any |  | where min_pct is met: "any" column, "all" of them, or the "total" column |
| `count_cell` | other | call |  | plan_cells.categorical | a count's cell, n (%) (plan_cells(categorical = )); NULL: the study standard's |
| `denominator` | other | call |  |  | the percentages' denominator: NULL the analysis set's subjects, "data" the records' subjects |
| `total` | other | call | FALSE | plan_total | TRUE: a Total column (the analysis again without 'by', plan_total()) |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "stf_t14_ae_soc_pt", ...)`).

|  |  |  |  |
|----|----|----|----|
| Alias | Collection | Item | Title |
| `stf_t14_ae_soc_pt` | FDA STF IG v2 | T14 | AEs by SOC and PT |
| `phuse_ae_soc_pt` | PhUSE CSS | AE table | AEs by SOC and PT with risk difference |
| `ars_ae_soc_pt` | CDISC ARS | 14-3-2-1 | TEAEs by SOC and PT |

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

- CDISC ARS: Clinical Data Interchange Standards Consortium (CDISC).
  Analysis Results Standard, v1.0, 19 April 2024.
  <https://www.cdisc.org/standards/foundational/analysis-results-standard>
  (accessed 2026-10-28). Copyright CDISC. Its examples:
  github.com/cdisc-org/analysis-results-standard (content CC BY 4.0,
  code MIT); cited by number only. CDISC is a trademark of the Clinical
  Data Interchange Standards Consortium.

The aliases follow the numbering of these publications, and the titles
are short descriptions of our own, not their text. The templates are
tflspec's, not endorsed or recommended by FDA, PhUSE or CDISC.

## Example

    plan <- tfl_tpl(ard, "ae_soc_pt", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_soc_pt", tier = c("required", "usual"))
#> ae_soc_pt (table, layout hierarchical; variant of hierarchical; tflspec)
#> Adverse events by SOC and PT (FDA STF Table 14)
#> Call: tfl_tpl(ard, "ae_soc_pt", ...)
#> 
#> required (2):
#>   param      | effective (layer)                     | what
#>   population | - (template)                          | the analysis set's id (SAF)
#>   hierarchy  | AEBODSYS, AEDECOD (variant:ae_soc_pt) | the levels, outer first
#> 
#> usual (5):
#>   param   | effective (layer)                                | what
#>   where   | TRTEMFL == "Y" (variant:ae_soc_pt)               | the records kept (R)
#>   by      | TRT01A (template)                                | the column variable
#>   any_row | TRUE (variant:ae_soc_pt)                         | TRUE: the row of the subjects with any record, first
#>   heading | System Organ Class,    Preferred Term (template) | the label column's heading, a line a level
#>   min_pct | - (template)                                     | the rows kept: this % of subjects or more (plan_filter()); NULL: all
```
