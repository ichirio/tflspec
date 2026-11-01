# The templates

Every table and figure template takes the same pair of filters, its
input form's first group with `population`: `subset` (which subjects: R
on the analysis set's ADSL) and, with records data, `where` (which
records). Both are `NULL` by default; see
[`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md).

## Usage

``` r
tfl_templates(source = NULL)
```

## Arguments

- source:

  `NULL` (default): the templates. A collection – `"tflspec"`,
  `"FDA STF IG v2"`, `"PhUSE CSS"`, `"CDISC ARS"` – for its items.

## Value

A data frame. Without `source`, one row a template: `name`, `kind`,
`layout`, `params` (its parameters, comma-separated), `package` (the
package that registered it: tflspec, or a company's,
[`tfl_template()`](https://ichirio.github.io/tflspec/reference/tfl_template.md)),
`call` (the function its program calls, the A use), `from`, `label`,
`ai` and `aliases` (its catalog aliases, comma-separated, or `NA`). With
`source`, one row an item: `source`, `item` (its number there), `title`,
`alias`, `template` (the template that writes it, `NA` when none does
yet), `status` (`"same"`: the template writes it; `"close"`: the
template writes it, other defaults or a column it lacks would match the
collection exactly; `"missing"`; `"not_output"`: a criteria table, not
an output), `kind` and `note`.

## Details

A template is one program; the items of a published collection – FDA's
Standard Safety Tables and Figures IG v2, PhUSE CSS's white papers,
CDISC ARS's examples – are in the templates' catalog, each with an alias
(`stf_t14_ae_soc_pt`) that runs its template wherever a template's name
is taken
([`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
a report's `template: {name: }`, the template sheet), as itself.
`source` lists a collection's every item, whichever template writes it,
and those none writes yet. The catalog keeps each item's number and a
short title of our own, not the publications' text; the templates are
tflspec's, not endorsed or recommended by FDA, PhUSE or CDISC. The
publications are cited below.

## Sources

The catalog's collections, as published:

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

## Examples

``` r
tfl_templates()
#>              name    kind       layout
#> 1  ae_disc_soc_pt   table hierarchical
#> 2         ae_eair   table hierarchical
#> 3    ae_max_grade   table hierarchical
#> 4     ae_overview   table      summary
#> 5      ae_pt_freq   table hierarchical
#> 6   ae_ser_soc_pt   table hierarchical
#> 7       ae_soc_pt   table hierarchical
#> 8          ancova   table      summary
#> 9           demog   table      summary
#> 10  demog_age_sex   table      summary
#> 11    disposition   table      summary
#> 12      forest_hr  figure       forest
#> 13   hierarchical   table hierarchical
#> 14             km  figure           km
#> 15  km_cumulative  figure           km
#> 16    km_survival  figure           km
#> 17       km_table   table     km_table
#> 18   lab_abnormal   table      summary
#> 19      lab_shift   table        shift
#> 20        listing listing      listing
#> 21        mean_ci  figure         mean
#> 22        mean_se  figure         mean
#> 23        my_ages   table      summary
#> 24            orr   table   stat_block
#> 25   param_change   table      summary
#> 26          shift   table        shift
#> 27     stat_block   table   stat_block
#> 28  stf_t02_demog   table      summary
#> 29        summary   table      summary
#> 30      waterfall  figure    waterfall
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             params
#> 1                                                                                                                                                                                                                                                                                                                                                                                                       population, subset, hierarchy, data, where, by, any_row, any_row_label, heading, sort, min_pct, min_pct_in, count_cell, denominator, total
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                    population, subset, data, where, by, hierarchy, exposure, start, per, overall
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                        population, subset, grade, grade_levels, grade_in, hierarchy, by, data, where, data_id, overall, any_grade, sort, heading
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     population, subset, by, data, where, data_id, flags, heading
#> 5                                                                                                                                                                                                                                                                                                                                                                                                      population, subset, term, by, data, where, data_id, data_label, analysis_id, analysis_label, overall, min_pct, min_pct_in, sort_by, heading
#> 6                                                                                                                                                                                                                                                                                                                                                                                                       population, subset, hierarchy, data, where, by, any_row, any_row_label, heading, sort, min_pct, min_pct_in, count_cell, denominator, total
#> 7                                                                                                                                                                                                                                                                                                                                                                                                       population, subset, hierarchy, data, where, by, any_row, any_row_label, heading, sort, min_pct, min_pct_in, count_cell, denominator, total
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                     population, subset, data, param, visit, by, response, covariates, where, ref
#> 9                                                                                                                                                                                                                                                                                                                                                                                      population, subset, data, where, by, continuous, categorical, labels, order, cells, count_cell, continuous_statistics, categorical_statistics, total, stack
#> 10                                                                                                                                                                                                                                                                                                                                                                                     population, subset, data, where, by, continuous, categorical, labels, order, cells, count_cell, continuous_statistics, categorical_statistics, total, stack
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                            population, subset, categorical, cat_id, cat_label, labels, by, categorical_statistics, groupn_label
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                              param, population, subset, data, group, subgroups, comparison, time, censor, where
#> 13                                                                                                                                                                                                                                                                                                                                                                                                      population, subset, hierarchy, data, where, by, any_row, any_row_label, heading, sort, min_pct, min_pct_in, count_cell, denominator, total
#> 14 population, subset, data, param, where, event, time, time_unit, censor, by, y_type, y_percent, ci, ci_alpha, ci_times, conf_level, conf_type, censor_marks, censor_shape, censor_size, censor_stroke, median_line, median_linetype, median_colour, median_linewidth, risk_table, risk_title, risk_statistics, risk_height, risk_size, x_max, x_step, x_label, y_min, y_max, y_step, y_label, linewidth, palette, linetype_by, legend, legend_title, legend_median, legend_median_digits, pvalue, pvalue_location, title, theme, base_size, plot
#> 15                                                                                                                                                                                                                                        time, time_unit, censor, by, y_percent, ci, ci_times, conf_level, conf_type, censor_marks, median_line, risk_table, risk_statistics, x_max, x_step, x_label, y_min, y_max, y_step, y_label, linetype_by, legend, legend_title, legend_median, legend_median_digits, pvalue, pvalue_location, title, plot
#> 16                                                                                                                                                                                                                                        time, time_unit, censor, by, y_percent, ci, ci_times, conf_level, conf_type, censor_marks, median_line, risk_table, risk_statistics, x_max, x_step, x_label, y_min, y_max, y_step, y_label, linetype_by, legend, legend_title, legend_median, legend_median_digits, pvalue, pvalue_location, title, plot
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                by, by_levels, median_label, time_prefix, time_unit, drop_time_0
#> 18                                                                                                                                                                                                                                                                                                                                                                                             population, subset, data, params, where, by, param_order, category, levels, cumulative, baseline_normal, baseline, baseline_value, data_id, heading
#> 19                                                                                                                                                                                                                                                                                                                                                                                            population, subset, visit, params, where, by, data_id, by_levels, param_labels, levels, worst, missing_label, total_label, param_page, param_heading
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              data, columns, where, subset, sort
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                    param, population, subset, data, value, group, visit_var, visit_order, where
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                    param, population, subset, data, value, group, visit_var, visit_order, where
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      population
#> 24                                                                                                                                                                                                                                                                                                                                                                                                               population, subset, by, by_levels, data, param, category, responders, where, data_id, ci_method, conf_level, stat_layout, heading
#> 25                                                                                                                                                                                                                                                                                                                                   population, subset, data, params, visits, where, by, layout, param_order, visit_order, baseline, baseline_label, value_label, change_label, data_id, data_label, statistics, cells, param_page, param_heading
#> 26                                                                                                                                                                                                                                                                                                                                                  population, subset, by, data, where, data_id, analysis_id, param, baseline, post, levels, by_levels, param_labels, cell, heading, worst, missing_label, total_label, param_page, param_heading
#> 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          by, by_levels, variable, rows, cells, direction, label
#> 28                                                                                                                                                                                                                                                                                                                                                                                     population, subset, data, where, by, continuous, categorical, labels, order, cells, count_cell, continuous_statistics, categorical_statistics, total, stack
#> 29                                                                                                                                                                                                                                                                                                                                                                                     population, subset, data, where, by, continuous, categorical, labels, order, cells, count_cell, continuous_statistics, categorical_statistics, total, stack
#> 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      param, population, subset, data, value, where, by_response
#>      package    call         from
#> 1    tflspec tfl_tpl    ae_soc_pt
#> 2    tflspec tfl_tpl         <NA>
#> 3    tflspec tfl_tpl         <NA>
#> 4    tflspec tfl_tpl         <NA>
#> 5    tflspec tfl_tpl         <NA>
#> 6    tflspec tfl_tpl    ae_soc_pt
#> 7    tflspec tfl_tpl hierarchical
#> 8    tflspec tfl_tpl         <NA>
#> 9    tflspec tfl_tpl      summary
#> 10 (session) tfl_tpl        demog
#> 11   tflspec tfl_tpl         <NA>
#> 12   tflspec tfl_tpl         <NA>
#> 13   tflspec tfl_tpl         <NA>
#> 14   tflspec tfl_tpl         <NA>
#> 15   tflspec tfl_tpl           km
#> 16   tflspec tfl_tpl           km
#> 17   tflspec tfl_tpl         <NA>
#> 18   tflspec tfl_tpl         <NA>
#> 19   tflspec tfl_tpl         <NA>
#> 20   tflspec tfl_tpl         <NA>
#> 21   tflspec tfl_tpl         <NA>
#> 22   tflspec tfl_tpl         <NA>
#> 23 (session) tfl_tpl         <NA>
#> 24   tflspec tfl_tpl         <NA>
#> 25   tflspec tfl_tpl         <NA>
#> 26   tflspec tfl_tpl         <NA>
#> 27   tflspec tfl_tpl         <NA>
#> 28   tflspec tfl_tpl        demog
#> 29   tflspec tfl_tpl         <NA>
#> 30   tflspec tfl_tpl         <NA>
#>                                                                                          label
#> 1         Adverse events leading to treatment discontinuation by SOC and PT (FDA STF Table 12)
#> 2                                                                                         <NA>
#> 3                                                                                         <NA>
#> 4                                                                                         <NA>
#> 5                                                                                         <NA>
#> 6                                      Serious adverse events by SOC and PT (FDA STF Table 10)
#> 7                                              Adverse events by SOC and PT (FDA STF Table 14)
#> 8                                                                                         <NA>
#> 9                                                                                 Demographics
#> 10                                                                                Demographics
#> 11                                                                                        <NA>
#> 12                                                           Hazard ratio by subgroup (forest)
#> 13                                                                                        <NA>
#> 14                                                            Kaplan-Meier curves (the layout)
#> 15 Kaplan-Meier plot of the cumulative percentage (1 - S(t)): CI bars, number at risk, p-value
#> 16             Kaplan-Meier plot of the survival: curves, median line, number at risk, p-value
#> 17                                                                                        <NA>
#> 18                                                                                        <NA>
#> 19                                                                                        <NA>
#> 20                                                                                        <NA>
#> 21                                                                      Mean (95% CI) by visit
#> 22                                                                        Mean +/- SE by visit
#> 23                                                                                        <NA>
#> 24                                                                                        <NA>
#> 25                                                                                        <NA>
#> 26                                                                                        <NA>
#> 27                                                                                        <NA>
#> 28                                                   Demographics, after FDA STF IG v2 Table 2
#> 29                                                                                        <NA>
#> 30                                                          Waterfall of each subject's change
#>       ai
#> 1   TRUE
#> 2  FALSE
#> 3  FALSE
#> 4  FALSE
#> 5  FALSE
#> 6   TRUE
#> 7   TRUE
#> 8  FALSE
#> 9   TRUE
#> 10  TRUE
#> 11 FALSE
#> 12 FALSE
#> 13 FALSE
#> 14 FALSE
#> 15  TRUE
#> 16  TRUE
#> 17 FALSE
#> 18 FALSE
#> 19 FALSE
#> 20 FALSE
#> 21 FALSE
#> 22 FALSE
#> 23 FALSE
#> 24 FALSE
#> 25 FALSE
#> 26 FALSE
#> 27 FALSE
#> 28 FALSE
#> 29 FALSE
#> 30 FALSE
#>                                                                                                                                      aliases
#> 1                                                                                                                        stf_t12_disc_soc_pt
#> 2                                                                                                                              stf_t16_ae_ir
#> 3                                                                                                                                       <NA>
#> 4                                                                                                        stf_t04_populations, ars_ae_summary
#> 5                                                                                                                          stf_t15_ae_common
#> 6                                                                                                                         stf_t10_sae_soc_pt
#> 7                                                                                          stf_t14_ae_soc_pt, phuse_ae_soc_pt, ars_ae_soc_pt
#> 8                                                                                                                                       <NA>
#> 9                                                                                                                                  ars_demog
#> 10                                                                                                                                      <NA>
#> 11                                                                                                                      stf_t05_disc_reasons
#> 12                                                                                                                                      <NA>
#> 13                                                                                                                                      <NA>
#> 14                                                                                                                            stf_f13_km_lfu
#> 15                                                                           stf_f01_km_disc, stf_f04_km_aesi, stf_f11_km_ole, stf_f14_km_ae
#> 16                                                                                                                                      <NA>
#> 17                                                                                                                                      <NA>
#> 18 stf_t20_kidney_level, stf_t36_sbp_max, stf_t37_dbp_max, stf_t38_hypotension, stf_t52_chem_last_l2, stf_t53_hem_last_l2, phuse_os_abnormal
#> 19                                                                                                                            phuse_os_shift
#> 20                                                                                     stf_t01_trials, stf_t09_death_list, stf_t39_disc_list
#> 21                                                                                                stf_f06_chem_mean_chg, stf_f09_vs_mean_chg
#> 22                                                                                                                                      <NA>
#> 23                                                                                                                                      <NA>
#> 24                                                                                                                                      <NA>
#> 25                                                                                              phuse_ct_summary, ars_vs_chg_h, ars_vs_chg_v
#> 26                                                                                                                                      <NA>
#> 27                                                                                                                                      <NA>
#> 28                                                                                                                                      <NA>
#> 29                                                                                                                                      <NA>
#> 30                                                                                                                                      <NA>
head(tfl_templates(source = "FDA STF IG v2"))
#>          source item                                       title
#> 1 FDA STF IG v2  T01                    Trials in the submission
#> 2 FDA STF IG v2  T02   Demographics and baseline characteristics
#> 3 FDA STF IG v2  T03           Screening and enrollment by trial
#> 4 FDA STF IG v2  T04                        Analysis populations
#> 5 FDA STF IG v2  T05 Treatment and study discontinuation reasons
#> 6 FDA STF IG v2  T06                 Treatment exposure duration
#>                  alias      template  status    kind
#> 1       stf_t01_trials       listing    same listing
#> 2        stf_t02_demog stf_t02_demog   close   table
#> 3    stf_t03_screening          <NA> missing    <NA>
#> 4  stf_t04_populations   ae_overview   close   table
#> 5 stf_t05_disc_reasons   disposition   close   table
#> 6     stf_t06_exposure          <NA> missing    <NA>
#>                                                         note
#> 1                                trial metadata outside ADaM
#> 2                                         registered variant
#> 3 needs screen-failure population; ae_overview counts likely
#> 4                  ADSL data with population flags; untested
#> 5            reason indent under discontinued is approximate
#> 6      needs person-years and cumulative duration categories
```
