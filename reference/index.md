# Package index

## ARD spec and ARD code

The analyses of a study as a workbook – its datasets, analysis sets,
analysis data and one cards / cardx call per `analyses` row – and the
short R program it stands for
([`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)),
or that program run
([`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)):
one study ARD.

- [`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  [`tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)
  : Read and check an ARD definition workbook
- [`tfl_ard_spec_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_spec_template.md)
  : An empty ARD spec
- [`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md)
  : The R code that makes the study's ARD
- [`tfl_ard_data_names()`](https://ichirio.github.io/tflspec/reference/tfl_ard_data_names.md)
  : The data each analysis reads, by its name in the ARD program
- [`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)
  : Make the study's ARD from its definition
- [`tfl_ard_for()`](https://ichirio.github.io/tflspec/reference/tfl_ard_for.md)
  : One output's part of the study ARD
- [`tfl_ard_spec_hash()`](https://ichirio.github.io/tflspec/reference/tfl_ard_spec_hash.md)
  : A fingerprint of one output's ARD definition

### Methods, statistics and functions of one’s own

What a `method` may name: the keywords, any cards / cardx function and
its arguments, an analysis written as R (`custom`), a function of the
study’s own.

- [`tfl_ard_methods()`](https://ichirio.github.io/tflspec/reference/tfl_ard_methods.md)
  : The methods an analysis row may name
- [`tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.md)
  : The statistics an ARD analysis may ask for
- [`tfl_ard_functions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_functions.md)
  : The ARD functions an analysis may call
- [`tfl_ard_args()`](https://ichirio.github.io/tflspec/reference/tfl_ard_args.md)
  : The arguments of one ARD function, and how each is filled in
- [`tfl_ard_as_custom()`](https://ichirio.github.io/tflspec/reference/tfl_ard_as_custom.md)
  : An analysis as R: the code of a custom analysis
- [`tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.md)
  : Start an ARD function of one's own from a template
- [`tfl_ard_function_info()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_info.md)
  : What the ARD functions of one's own in some R files are

## The functions the programs call

The code of the functions the generated programs call as they run (the
code lists on a data, the ids in front, the formats, the study ARD),
which a study keeps in a file of its own: its programs run without
tflspec.

- [`tfl_helpers_code()`](https://ichirio.github.io/tflspec/reference/tfl_helpers_code.md)
  : The functions a study's generated programs call

## Data, analysis data and populations

The `datasets`, `populations` and `analysis_data` sheets of the ARD spec
say what is read and how the analysis data are made (see
[`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md));
these write the line that reads a dataset and give data to try things
on.

- [`tfl_read_data_code()`](https://ichirio.github.io/tflspec/reference/tfl_read_data_code.md)
  : The code that reads one dataset of a data catalog
- [`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md)
  : Read ADaM datasets
- [`tfl_example_adam()`](https://ichirio.github.io/tflspec/reference/tfl_example_adam.md)
  : Example ADaM-like data for trying tflspec

### An oncology sample study

Simulated ADaM datasets of one oncology study, XYZ-ONC-01 (Drug X vs
Control), that agree with one another; its SAP and CRF outline are in
`system.file("sample", "onco", package = "tflspec")`.

- [`adsl`](https://ichirio.github.io/tflspec/reference/adsl.md) :
  Oncology sample study XYZ-ONC-01: subject level (ADSL)
- [`adex`](https://ichirio.github.io/tflspec/reference/adex.md) :
  Oncology sample study XYZ-ONC-01: exposure (ADEX)
- [`adae`](https://ichirio.github.io/tflspec/reference/adae.md) :
  Oncology sample study XYZ-ONC-01: adverse events (ADAE)
- [`adlb`](https://ichirio.github.io/tflspec/reference/adlb.md) :
  Oncology sample study XYZ-ONC-01: laboratory (ADLB)
- [`advs`](https://ichirio.github.io/tflspec/reference/advs.md) :
  Oncology sample study XYZ-ONC-01: vital signs (ADVS)
- [`adrs`](https://ichirio.github.io/tflspec/reference/adrs.md) :
  Oncology sample study XYZ-ONC-01: tumour response (ADRS)
- [`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md) :
  Oncology sample study XYZ-ONC-01: tumour sizes (ADTR)
- [`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md) :
  Oncology sample study XYZ-ONC-01: time to event (ADTTE)

## Document import

A study’s define (Define-XML 2.0 / 2.1, or the workbook it is made from)
read by rule: its datasets, variables and code lists, and a report’s
code lists and labels as `codelists` rows.

- [`tfl_read_define()`](https://ichirio.github.io/tflspec/reference/tfl_read_define.md)
  : Read a Define-XML (2.0 or 2.1)

- [`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)
  : Read a define spec workbook (an import profile)

- [`tfl_define_profile()`](https://ichirio.github.io/tflspec/reference/tfl_define_profile.md)
  : An import profile for a define spec workbook

- [`tfl_define_codelists()`](https://ichirio.github.io/tflspec/reference/tfl_define_codelists.md)
  :

  A define's code lists as a report's `codelists` rows

## Spec files (YAML)

A study’s spec as files: one report a file, its `content:` a list of
items, one function call each; read, layered over the study’s defaults,
checked against the schema, written as code, and converted from the
workbooks.

- [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  [`tfl_read_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  : Read a study's YAML spec
- [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  [`tfl_write_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  : Read and write a report's YAML file
- [`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)
  : A report over the layers it extends
- [`tfl_write_study_part()`](https://ichirio.github.io/tflspec/reference/tfl_write_study_part.md)
  : Write one part of a study's YAML spec
- [`tfl_item_code()`](https://ichirio.github.io/tflspec/reference/tfl_item_code.md)
  [`tfl_item_written()`](https://ichirio.github.io/tflspec/reference/tfl_item_code.md)
  [`tfl_item_own()`](https://ichirio.github.io/tflspec/reference/tfl_item_code.md)
  : One content item: its code, its written arguments, its own part
- [`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md)
  : The spec schema: the arguments a spec item may give, and their types
- [`tfl_args_to_r()`](https://ichirio.github.io/tflspec/reference/tfl_args_to_r.md)
  [`tfl_args_from_r()`](https://ichirio.github.io/tflspec/reference/tfl_args_to_r.md)
  : An item's arguments as R, and R as an item's arguments
- [`tfl_schema_check()`](https://ichirio.github.io/tflspec/reference/tfl_schema_check.md)
  : Check one spec item against the schema
- [`tfl_schema_help()`](https://ichirio.github.io/tflspec/reference/tfl_schema_help.md)
  : The help of a spec item's argument
- [`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md)
  : The code of a report's content, from its spec file
- [`tfl_content_run()`](https://ichirio.github.io/tflspec/reference/tfl_content_run.md)
  : Make a report's content, from its spec file, without its code
- [`tfl_convert_sheets_to_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_convert_sheets_to_yaml.md)
  [`tfl_yaml_from_sheets()`](https://ichirio.github.io/tflspec/reference/tfl_convert_sheets_to_yaml.md)
  : Convert a study's definition sheets to the YAML spec
- [`tfl_write_spec_xlsx()`](https://ichirio.github.io/tflspec/reference/tfl_write_spec_xlsx.md)
  : Write the flat view of a study's YAML spec as a workbook
- [`tfl_import_spec_xlsx()`](https://ichirio.github.io/tflspec/reference/tfl_import_spec_xlsx.md)
  : Take an edited flat-view workbook back into the YAML spec
- [`tfl_sheets_from_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_sheets_from_yaml.md)
  [`tfl_sheets_from_spec()`](https://ichirio.github.io/tflspec/reference/tfl_sheets_from_yaml.md)
  : The YAML spec as sheets

## Table spec and table plan

A table as a workbook: its roles and the rtfreporter plan verbs it
stands for, as a plan
([`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md))
or as code
([`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md));
a plan written in code back to a workbook.

- [`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
  : A workbook-shaped definition of how an ARD becomes a table
- [`tfl_table_spec_template()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec_template.md)
  : Scaffold a definition workbook from an ARD
- [`tfl_table_plan()`](https://ichirio.github.io/tflspec/reference/tfl_table_plan.md)
  : A table's plan from its definition
- [`tfl_normalize_code()`](https://ichirio.github.io/tflspec/reference/tfl_normalize_code.md)
  : The line that normalizes a table's ARD
- [`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md)
  : The code of a table's plan, from its definition
- [`tfl_as_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_table_spec.md)
  : Write a plan as a table definition workbook

## Template SPEC

A report kind from a few parameters: one definition gives the table plan
(`tpl_*()`, the A use) and the report’s items
([`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md),
the B use). Layout templates (`summary`) and display templates on them
(`demog`). A report’s YAML file may be a template SPEC,
`template: {name, args}`:
[`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)
resolves it, its program is the template’s call, and
[`tfl_template_to_spec()`](https://ichirio.github.io/tflspec/reference/tfl_template_to_spec.md)
writes it out as its items.

- [`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md)
  : A table or listing template's plan: one call for every template

- [`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)
  : The templates

- [`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
  : Expand a template into a report's items

- [`tfl_template_to_spec()`](https://ichirio.github.io/tflspec/reference/tfl_template_to_spec.md)
  : A template report written out as its items

- [`tpl_demog()`](https://ichirio.github.io/tflspec/reference/tpl_demog.md)
  :

  Demographics: a table plan from the template `demog`

- [`tpl_summary()`](https://ichirio.github.io/tflspec/reference/tpl_summary.md)
  :

  Continuous variables by a column key: a table plan from the layout
  `summary`

- [`tpl_ae_soc_pt()`](https://ichirio.github.io/tflspec/reference/tpl_ae_soc_pt.md)
  :

  Adverse events by SOC and PT: a table plan from the template
  `ae_soc_pt`

- [`tpl_ae_pt_freq()`](https://ichirio.github.io/tflspec/reference/tpl_ae_pt_freq.md)
  :

  Adverse events by preferred term, most frequent first: the template
  `ae_pt_freq`

- [`tpl_ae_max_grade()`](https://ichirio.github.io/tflspec/reference/tpl_ae_max_grade.md)
  :

  Adverse events by SOC and PT at the maximum severity or grade: the
  template `ae_max_grade`

- [`tpl_hierarchical()`](https://ichirio.github.io/tflspec/reference/tpl_hierarchical.md)
  :

  A hierarchy's counts: a table plan from the layout `hierarchical`

- [`tpl_disposition()`](https://ichirio.github.io/tflspec/reference/tpl_disposition.md)
  :

  Disposition: a table plan from the template `disposition`

- [`tpl_shift()`](https://ichirio.github.io/tflspec/reference/tpl_shift.md)
  [`tpl_lab_shift()`](https://ichirio.github.io/tflspec/reference/tpl_shift.md)
  :

  Shift tables: table plans from the templates `shift` and `lab_shift`

- [`tpl_listing()`](https://ichirio.github.io/tflspec/reference/tpl_listing.md)
  :

  A listing: a listing plan from the layout `listing`

- [`tpl_km_table()`](https://ichirio.github.io/tflspec/reference/tpl_km_table.md)
  :

  Kaplan-Meier estimates: a table plan from the template `km_table`

- [`tpl_param_change()`](https://ichirio.github.io/tflspec/reference/tpl_param_change.md)
  :

  Parameters' value and change: a table plan from the template
  `param_change`

- [`tpl_lab_abnormal()`](https://ichirio.github.io/tflspec/reference/tpl_lab_abnormal.md)
  :

  Laboratory abnormalities: a table plan from the template
  `lab_abnormal`

- [`tpl_ae_eair()`](https://ichirio.github.io/tflspec/reference/tpl_ae_eair.md)
  :

  Exposure-adjusted incidence rates by SOC and PT: the template
  `ae_eair`

- [`tpl_ancova()`](https://ichirio.github.io/tflspec/reference/tpl_ancova.md)
  :

  An ANCOVA of the change from baseline: a table plan from the template
  `ancova`

- [`tpl_mean_se()`](https://ichirio.github.io/tflspec/reference/tpl_mean_se.md)
  [`tpl_mean_ci()`](https://ichirio.github.io/tflspec/reference/tpl_mean_se.md)
  :

  Mean by visit: a figure from the template `mean_se` or `mean_ci`

- [`tpl_waterfall()`](https://ichirio.github.io/tflspec/reference/tpl_waterfall.md)
  :

  Waterfall: a figure from the template `waterfall`

- [`tpl_forest_hr()`](https://ichirio.github.io/tflspec/reference/tpl_forest_hr.md)
  [`tpl_forest_layout()`](https://ichirio.github.io/tflspec/reference/tpl_forest_hr.md)
  :

  Hazard ratio by subgroup: a forest plot from the template `forest_hr`

- [`tpl_orr()`](https://ichirio.github.io/tflspec/reference/tpl_orr.md)
  :

  Objective response: a table plan from the template `orr`

- [`tpl_stat_block()`](https://ichirio.github.io/tflspec/reference/tpl_stat_block.md)
  :

  One record's statistics over rows and columns: a table plan from the
  template `stat_block`

- [`tpl_ae_overview()`](https://ichirio.github.io/tflspec/reference/tpl_ae_overview.md)
  :

  Overview of adverse events: a table plan from the template
  `ae_overview`

- [`tfl_read_template_sheets()`](https://ichirio.github.io/tflspec/reference/tfl_read_template_sheets.md)
  : Read reports' template SPECs from a workbook

- [`tfl_write_template_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_write_template_yaml.md)
  : Write a report's template SPEC as its YAML file

## Template SPEC, a company’s templates

A company’s templates are an R package of its own: it registers them
when it is loaded
([`tfl_template()`](https://ichirio.github.io/tflspec/reference/tfl_template.md),
usually a tflspec template with the company’s defaults), exports the
function its reports’ programs call
([`tfl_template_run()`](https://ichirio.github.io/tflspec/reference/tfl_template_run.md))
and tests that no parameter is the look
([`tfl_test_templates()`](https://ichirio.github.io/tflspec/reference/tfl_test_templates.md)).
[`tfl_use_company_templates()`](https://ichirio.github.io/tflspec/reference/tfl_use_company_templates.md)
writes one to start from.

- [`tfl_template()`](https://ichirio.github.io/tflspec/reference/tfl_template.md)
  : Register a template
- [`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md)
  : A template parameter
- [`tfl_template_params()`](https://ichirio.github.io/tflspec/reference/tfl_template_params.md)
  : A template's parameters
- [`tfl_template_variant()`](https://ichirio.github.io/tflspec/reference/tfl_template_variant.md)
  : A template with other defaults
- [`tfl_template_run()`](https://ichirio.github.io/tflspec/reference/tfl_template_run.md)
  : Run a template: its table plan
- [`tfl_test_templates()`](https://ichirio.github.io/tflspec/reference/tfl_test_templates.md)
  : Test templates: no parameter is the look, each has its type
- [`tfl_use_company_templates()`](https://ichirio.github.io/tflspec/reference/tfl_use_company_templates.md)
  : Write a company's template package to start from

## Template SPEC, a template’s help

A template’s parameters by tier (required, usual, other): the value in
effect and where it is from – the study standard, a variant, the layout
([`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md))
– and each table and listing template’s page and the Kaplan-Meier report
types’ (`km_survival`, `km_cumulative`), `?tfl_tpl_<name>`, made from
the registry.

- [`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md)
  : A template's parameters, as a table
- [`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md)
  : A template's help
- [`tfl_tpl_ae_disc_soc_pt`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_disc_soc_pt.md)
  : Adverse events leading to treatment discontinuation by SOC and PT
  (FDA STF Table 12): the template ae_disc_soc_pt
- [`tfl_tpl_ae_eair`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_eair.md)
  : ae_eair: the template ae_eair
- [`tfl_tpl_ae_max_grade`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_max_grade.md)
  : ae_max_grade: the template ae_max_grade
- [`tfl_tpl_ae_overview`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_overview.md)
  : ae_overview: the template ae_overview
- [`tfl_tpl_ae_pt_freq`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_pt_freq.md)
  : ae_pt_freq: the template ae_pt_freq
- [`tfl_tpl_ae_ser_soc_pt`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_ser_soc_pt.md)
  : Serious adverse events by SOC and PT (FDA STF Table 10): the
  template ae_ser_soc_pt
- [`tfl_tpl_ae_soc_pt`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ae_soc_pt.md)
  : Adverse events by SOC and PT (FDA STF Table 14): the template
  ae_soc_pt
- [`tfl_tpl_ancova`](https://ichirio.github.io/tflspec/reference/tfl_tpl_ancova.md)
  : ancova: the template ancova
- [`tfl_tpl_demog`](https://ichirio.github.io/tflspec/reference/tfl_tpl_demog.md)
  : Demographics: the template demog
- [`tfl_tpl_disposition`](https://ichirio.github.io/tflspec/reference/tfl_tpl_disposition.md)
  : disposition: the template disposition
- [`tfl_tpl_hierarchical`](https://ichirio.github.io/tflspec/reference/tfl_tpl_hierarchical.md)
  : hierarchical: the template hierarchical
- [`tfl_tpl_km_cumulative`](https://ichirio.github.io/tflspec/reference/tfl_tpl_km_cumulative.md)
  : Kaplan-Meier plot of the cumulative percentage (1 - S(t)): CI bars,
  number at risk, p-value: the template km_cumulative
- [`tfl_tpl_km_survival`](https://ichirio.github.io/tflspec/reference/tfl_tpl_km_survival.md)
  : Kaplan-Meier plot of the survival: curves, median line, number at
  risk, p-value: the template km_survival
- [`tfl_tpl_km_table`](https://ichirio.github.io/tflspec/reference/tfl_tpl_km_table.md)
  : km_table: the template km_table
- [`tfl_tpl_lab_abnormal`](https://ichirio.github.io/tflspec/reference/tfl_tpl_lab_abnormal.md)
  : lab_abnormal: the template lab_abnormal
- [`tfl_tpl_lab_shift`](https://ichirio.github.io/tflspec/reference/tfl_tpl_lab_shift.md)
  : lab_shift: the template lab_shift
- [`tfl_tpl_listing`](https://ichirio.github.io/tflspec/reference/tfl_tpl_listing.md)
  : listing: the template listing
- [`tfl_tpl_orr`](https://ichirio.github.io/tflspec/reference/tfl_tpl_orr.md)
  : orr: the template orr
- [`tfl_tpl_param_change`](https://ichirio.github.io/tflspec/reference/tfl_tpl_param_change.md)
  : param_change: the template param_change
- [`tfl_tpl_shift`](https://ichirio.github.io/tflspec/reference/tfl_tpl_shift.md)
  : shift: the template shift
- [`tfl_tpl_stat_block`](https://ichirio.github.io/tflspec/reference/tfl_tpl_stat_block.md)
  : stat_block: the template stat_block
- [`tfl_tpl_stf_t02_demog`](https://ichirio.github.io/tflspec/reference/tfl_tpl_stf_t02_demog.md)
  : Demographics, after FDA STF IG v2 Table 2: the template
  stf_t02_demog
- [`tfl_tpl_summary`](https://ichirio.github.io/tflspec/reference/tfl_tpl_summary.md)
  : summary: the template summary

## Report spec, titles, footnotes, headers and tokens

The page, the running header and footer, the titles and footnotes, the
tokens: the document around a table, a listing or a figure.

- [`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md)
  : Read a report definition: the table and everything around it
- [`tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.md)
  : Build a report's RTF document from its definition
- [`tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.md)
  : The code of a report's document, from its definition
- [`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md)
  : The study's setup code for its report programs
- [`tfl_report_tokens()`](https://ichirio.github.io/tflspec/reference/tfl_report_tokens.md)
  : A report's tokens
- [`tfl_report_overwrite()`](https://ichirio.github.io/tflspec/reference/tfl_report_overwrite.md)
  : Whether a report's RTF file may be written over
- [`tfl_report_path()`](https://ichirio.github.io/tflspec/reference/tfl_report_path.md)
  : Where a report's RTF file goes

## Listings

Two sheets keyed by `output_id`, the pages and the program.

- [`tfl_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
  [`tfl_read_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
  [`tfl_write_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_listing_spec.md)
  : A listing definition: read, write and check it
- [`tfl_listing()`](https://ichirio.github.io/tflspec/reference/tfl_listing.md)
  : A listing's pages from its definition
- [`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md)
  : The code that makes a listing from its definition
- [`tfl_as_listing_spec()`](https://ichirio.github.io/tflspec/reference/tfl_as_listing_spec.md)
  : A listing written in code, as a listing spec

## Figures

A figure as a YAML design – its data steps, statistics and layers – and
the ggplot2 script it stands for.

- [`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  [`tfl_write_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  [`tfl_read_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  [`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  [`tfl_check_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
  : A figure design
- [`tfl_fig_content_from_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_content_from_design.md)
  : A figure design as a figure report's content
- [`tfl_fig_forest_analyses()`](https://ichirio.github.io/tflspec/reference/tfl_fig_forest_analyses.md)
  : The analyses of a forest plot: the figure's own ARD
- [`tfl_fig_templates()`](https://ichirio.github.io/tflspec/reference/tfl_fig_templates.md)
  [`tfl_fig_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_templates.md)
  : Figure templates
- [`tfl_fig_parts()`](https://ichirio.github.io/tflspec/reference/tfl_fig_parts.md)
  : The pieces of a figure design
- [`tfl_fig_objects()`](https://ichirio.github.io/tflspec/reference/tfl_fig_objects.md)
  : The objects a figure's data steps read and make
- [`tfl_fig_page_size()`](https://ichirio.github.io/tflspec/reference/tfl_fig_page_size.md)
  : A figure's size on its report's page
- [`tfl_fig_uses_ard()`](https://ichirio.github.io/tflspec/reference/tfl_fig_uses_ard.md)
  : Whether a figure's design reads an ARD
- [`tfl_fig_add_layer()`](https://ichirio.github.io/tflspec/reference/tfl_fig_add_layer.md)
  : Add a layer to the figure designs' catalog
- [`tfl_fig_calls()`](https://ichirio.github.io/tflspec/reference/tfl_fig_calls.md)
  : Extension functions a figure design knows
- [`tfl_fig_compat()`](https://ichirio.github.io/tflspec/reference/tfl_fig_compat.md)
  : ggplot2 3.5 / 4.0 differences for figure designs
- [`tfl_fig_data_name()`](https://ichirio.github.io/tflspec/reference/tfl_fig_data_name.md)
  : The name of a figure's data in its program
- [`tfl_r()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_unset()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_call()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  : R objects of the YAML spec

### Figure style

Every look-and-feel value of a study’s figures, in one place.

- [`tfl_fig_style()`](https://ichirio.github.io/tflspec/reference/tfl_fig_style.md)
  [`tfl_fig_style_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_style.md)
  [`tfl_read_fig_style()`](https://ichirio.github.io/tflspec/reference/tfl_fig_style.md)
  : The figure style standard
- [`tfl_fig_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md)
  [`tfl_check_fig()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md)
  : The helper script of a study's figure programs
- [`tfl_time_unit_days()`](https://ichirio.github.io/tflspec/reference/tfl_time_unit_days.md)
  : The days in a week, a month and a year
- [`tfl_fig_palettes()`](https://ichirio.github.io/tflspec/reference/tfl_fig_palettes.md)
  : Colour palette presets

### Figure spec (sheets) and quick figures

The older sheet-based figure spec and its quick figures, kept while the
design is decided ([Discussion
\#63](https://github.com/ichirio/tflspec/discussions/63)).

- [`tfl_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_fig_spec.md)
  : Build a spec from data frames

- [`tfl_fig_spec_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_spec_template.md)
  : Write an Excel spec template with drop-down lists built from ADaM
  data

- [`tfl_read_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_fig_spec.md)
  : Read an Excel plot spec

- [`tfl_example_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_example_fig_spec.md)
  : Example plot spec

- [`tfl_fig_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_code.md)
  : Generate ggplot2 code from a plot spec

- [`tfl_write_fig_code()`](https://ichirio.github.io/tflspec/reference/tfl_write_fig_code.md)
  :

  Write generated code to `.R` files

- [`tfl_fig_schema()`](https://ichirio.github.io/tflspec/reference/tfl_fig_schema.md)
  : The arguments of every figure type, described

- [`tfl_fig_types()`](https://ichirio.github.io/tflspec/reference/tfl_fig_types.md)
  : Styles of the quick API

- [`tfl_fig_catalog()`](https://ichirio.github.io/tflspec/reference/tfl_fig_catalog.md)
  : Catalogue of clinical figure types

- [`tfl_fig_list_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_list_template.md)
  : Write an Excel plot list template

- [`tfl_fig_list_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_list_code.md)
  : Generate code for every row of a plot list

- [`tfl_fig_km()`](https://ichirio.github.io/tflspec/reference/tfl_fig_km.md)
  : Kaplan-Meier plot code (ggsurvfit)

- [`tfl_fig_waterfall()`](https://ichirio.github.io/tflspec/reference/tfl_fig_waterfall.md)
  : Waterfall plot code

- [`tfl_fig_swimmer()`](https://ichirio.github.io/tflspec/reference/tfl_fig_swimmer.md)
  : Swimmer plot code

- [`tfl_fig_forest()`](https://ichirio.github.io/tflspec/reference/tfl_fig_forest.md)
  : Forest plot code

- [`tfl_fig_mean()`](https://ichirio.github.io/tflspec/reference/tfl_fig_mean.md)
  : Mean over time code

- [`tfl_fig_box()`](https://ichirio.github.io/tflspec/reference/tfl_fig_box.md)
  : Box plot code

- [`tfl_fig_bar()`](https://ichirio.github.io/tflspec/reference/tfl_fig_bar.md)
  : Bar chart code for rates and category percentages

- [`tfl_fig_scatter()`](https://ichirio.github.io/tflspec/reference/tfl_fig_scatter.md)
  : Scatter plot code

- [`tfl_fig_individual()`](https://ichirio.github.io/tflspec/reference/tfl_fig_individual.md)
  : Individual profile code (spaghetti / spider)

- [`tfl_fig_pk()`](https://ichirio.github.io/tflspec/reference/tfl_fig_pk.md)
  : PK concentration-time plot code

- [`tfl_fig_ae_dot()`](https://ichirio.github.io/tflspec/reference/tfl_fig_ae_dot.md)
  : AE dot plot code

- [`tfl_fig_butterfly()`](https://ichirio.github.io/tflspec/reference/tfl_fig_butterfly.md)
  : AE butterfly plot code

- [`tfl_fig_edish()`](https://ichirio.github.io/tflspec/reference/tfl_fig_edish.md)
  : eDISH plot code

- [`tfl_fig_sankey()`](https://ichirio.github.io/tflspec/reference/tfl_fig_sankey.md)
  : Sankey diagram code for treatment sequences

- [`tfl_fig_sunburst()`](https://ichirio.github.io/tflspec/reference/tfl_fig_sunburst.md)
  : Sunburst code for treatment sequences

### Sankey diagrams and sunbursts

- [`tfl_plot_sankey()`](https://ichirio.github.io/tflspec/reference/tfl_plot_sankey.md)
  : Plot Sankey Diagram with Rectangular Nodes and Bezier Polygon Links
- [`tfl_plot_sankey_batch()`](https://ichirio.github.io/tflspec/reference/tfl_plot_sankey_batch.md)
  : Batch Create Sankey Plots for Subgroup Analysis
- [`tfl_sankey_data()`](https://ichirio.github.io/tflspec/reference/tfl_sankey_data.md)
  [`tfl_sunburst_data()`](https://ichirio.github.io/tflspec/reference/tfl_sankey_data.md)
  : Sankey / sunburst input from treatment-line data
- [`tfl_plot_sunburst()`](https://ichirio.github.io/tflspec/reference/tfl_plot_sunburst.md)
  : Plot a Static Sunburst of Treatment Sequences

## CDISC ARS export and import

The specs as CDISC’s Analysis Results Standard (JSON, and CDISC’s Excel
template to read), run by siera; anyone’s ARS back as specs.

- [`tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.md)
  : The specs as a CDISC ARS reporting event
- [`tfl_write_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_json.md)
  : Write a CDISC ARS reporting event as JSON
- [`tfl_write_ars_xlsx()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_xlsx.md)
  : Write a CDISC ARS reporting event as CDISC's Excel template
- [`tfl_ars_unmapped()`](https://ichirio.github.io/tflspec/reference/tfl_ars_unmapped.md)
  : What the ARS does not say
- [`tfl_ars_ard()`](https://ichirio.github.io/tflspec/reference/tfl_ars_ard.md)
  : Make the ARD of a CDISC ARS reporting event with siera
- [`tfl_read_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_read_ars_json.md)
  : Read a CDISC ARS reporting event from JSON
- [`tfl_ars_to_specs()`](https://ichirio.github.io/tflspec/reference/tfl_ars_to_specs.md)
  : A CDISC ARS reporting event as tflspec specs

## The YAML spec (one file a report)

The study’s definition as YAML – `study.yml`, `spec/data.yml`,
`spec/defaults.yml`, `spec/patterns/`, `spec/toc.yml`, one
`spec/outputs/<id>.yml` a report – its objects, the layering of a report
over its defaults, the one-time conversion of the sheets, and the flat
view as a workbook.

- [`tfl_r()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_unset()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_call()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  [`tfl_output()`](https://ichirio.github.io/tflspec/reference/tfl_r.md)
  : R objects of the YAML spec
- [`tfl_plot()`](https://ichirio.github.io/tflspec/reference/tfl_plot.md)
  : A plot of a figure's content: its name and its items
- [`tfl_read_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  [`tfl_write_output()`](https://ichirio.github.io/tflspec/reference/tfl_read_output.md)
  : Read and write a report's YAML file
- [`tfl_read_study_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  [`tfl_read_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_read_study_spec.md)
  : Read a study's YAML spec
- [`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)
  : A report over the layers it extends
- [`tfl_convert_sheets_to_yaml()`](https://ichirio.github.io/tflspec/reference/tfl_convert_sheets_to_yaml.md)
  [`tfl_yaml_from_sheets()`](https://ichirio.github.io/tflspec/reference/tfl_convert_sheets_to_yaml.md)
  : Convert a study's definition sheets to the YAML spec
- [`tfl_write_spec_xlsx()`](https://ichirio.github.io/tflspec/reference/tfl_write_spec_xlsx.md)
  : Write the flat view of a study's YAML spec as a workbook
- [`tfl_import_spec_xlsx()`](https://ichirio.github.io/tflspec/reference/tfl_import_spec_xlsx.md)
  : Take an edited flat-view workbook back into the YAML spec

## Reading and writing workbooks, the TOC

Each spec has its own reader and writer; several specs may share one
workbook. A company’s table of contents becomes report specs, and an ARD
a JSON / YAML / XPT copy.

- [`tfl_write_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard_spec.md)
  : Write an ARD definition to a workbook
- [`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md)
  : Read an ARD table definition from a workbook
- [`tfl_write_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md)
  [`tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.md)
  : Write a table or report definition to a workbook
- [`tfl_write_specs()`](https://ichirio.github.io/tflspec/reference/tfl_write_specs.md)
  : Write several specs to one workbook
- [`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md)
  : What each column of a spec workbook means
- [`tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.md)
  : Read a table of contents (TOC) as report specs
- [`tfl_write_ard()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard.md)
  : Write the study ARD as JSON, YAML or XPT
- [`tfl_read_ard()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard.md)
  : Read an ARD written by tfl_write_ard()

## Checks

What went wrong, said before a report is made: an ARD and what a table
reads from it, a function of one’s own, the ARS, a figure, a report’s
own code.

- [`tfl_check_ard()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard.md)
  : Check an ARD before a report uses it
- [`tfl_check_code()`](https://ichirio.github.io/tflspec/reference/tfl_check_code.md)
  : Check a report's own code against the rules
- [`tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.md)
  : Try an ARD function of one's own
- [`tfl_ard_conditions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_conditions.md)
  : What went wrong while an ARD was made
- [`tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.md)
  : Check a CDISC ARS reporting event
- [`tfl_fig_advice()`](https://ichirio.github.io/tflspec/reference/tfl_fig_advice.md)
  [`tfl_fig_apply_fix()`](https://ichirio.github.io/tflspec/reference/tfl_fig_advice.md)
  : Advice on a figure design
- [`tfl_check_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_check_fig_spec.md)
  : Check a plot spec against ADaM data

## Review of the whole definition

Every report’s problems at once, one row each: what cannot be used, what
is probably wrong and what to set by hand; with the facts of the data
and of each report’s ARD.

- [`tfl_review_spec()`](https://ichirio.github.io/tflspec/reference/tfl_review_spec.md)
  : Review a study's definition
- [`tfl_review_rules()`](https://ichirio.github.io/tflspec/reference/tfl_review_rules.md)
  : The rules of the review
- [`tfl_review_templates()`](https://ichirio.github.io/tflspec/reference/tfl_review_templates.md)
  : The sentences of the review beyond its rules
- [`tfl_data_facts()`](https://ichirio.github.io/tflspec/reference/tfl_data_facts.md)
  : What the data hold, for the review
- [`tfl_ard_facts()`](https://ichirio.github.io/tflspec/reference/tfl_ard_facts.md)
  : What an ARD holds, for the review

## AI manual and kit

The manual of the installed version, and the kit to attach to a chat
that drafts a study’s specs from its TOC and shells; each file it
returns reviewed, what the AI can correct apart from what the person
decides.

- [`tflspec_ai_manual()`](https://ichirio.github.io/tflspec/reference/tflspec_ai_manual.md)
  : The AI assistant manual that ships with this package
- [`tfl_ai_unpack()`](https://ichirio.github.io/tflspec/reference/tfl_ai_unpack.md)
  : Write an AI assistant's answer into a study's spec files
- [`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md)
  : The ADaM metadata to give an AI assistant
- [`tfl_ai_review()`](https://ichirio.github.io/tflspec/reference/tfl_ai_review.md)
  : Review the spec an AI assistant wrote

## AI-assisted drafting

A prompt for an AI chat that drafts tables’ report files (the code spec)
from a company’s TOC / SHELL workbook and an ADaM summary, the study’s
TOC, or a figure design; the answer read and checked by tflspec’s own
checks, and written as files or shown as what it changes; nothing is
sent anywhere.

- [`tfl_ai_read_shell()`](https://ichirio.github.io/tflspec/reference/tfl_ai_read_shell.md)
  : A TOC / SHELL workbook as text for an AI chat
- [`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md)
  : The ADaM datasets as a summary for an AI chat
- [`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md)
  : The TOC's tables in batches, one family each
- [`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md)
  : Write a study's table requests, one file a request
- [`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
  : Prepare a study's AI table requests in one call
- [`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
  : Read the answers of a study's requests, check them, write the report
  files
- [`tfl_ai_save_answer()`](https://ichirio.github.io/tflspec/reference/tfl_ai_save_answer.md)
  : Save a chat's answer to a request where tfl_ai_collect_answers()
  reads it
- [`tfl_ai_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_requests.md)
  : A study's requests, read again
- [`tfl_ai_run()`](https://ichirio.github.io/tflspec/reference/tfl_ai_run.md)
  : Run the study's AI answers through: collect, build, status,
  questions
- [`tfl_ai_compare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_compare.md)
  : Compare a study's AI answers: with templates or the code SPEC only
- [`tfl_ai_questions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_questions.md)
  [`tfl_ai_answer_questions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_questions.md)
  : The questions of a study's AI drafts, and the person's answers to
  them
- [`tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.md)
  : Start a study folder for drafted report files
- [`tfl_ai_tasks()`](https://ichirio.github.io/tflspec/reference/tfl_ai_tasks.md)
  : The tasks AI-assisted drafting knows
- [`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
  : The study facts a drafting prompt carries
- [`tfl_ai_schema()`](https://ichirio.github.io/tflspec/reference/tfl_ai_schema.md)
  : The schema text of a drafting task
- [`tfl_ai_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prompt.md)
  : A drafting prompt for an AI chat
- [`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md)
  : Read a drafting answer
- [`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md)
  : Check a drafting answer in its context
- [`tfl_ai_repair_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_repair_prompt.md)
  : The prompt that sends an answer's problems back
- [`tfl_ai_diff()`](https://ichirio.github.io/tflspec/reference/tfl_ai_diff.md)
  : What a drafting answer changes

## Helpers

- [`tflspec_code_options`](https://ichirio.github.io/tflspec/reference/tflspec_code_options.md)
  : How the generated programs name folders and packages
- [`print(`*`<tfl_code>`*`)`](https://ichirio.github.io/tflspec/reference/print.tfl_code.md)
  : Code object returned by the quick API
