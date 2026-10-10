# The specs as a CDISC ARS reporting event

Writes what the ARD spec analyses – and, when given, what the table and
report specs say of each output – as the CDISC Analysis Results Standard
(ARS) v1.0 model: a `ReportingEvent` with its analysis sets, data
subsets, groupings, methods, analyses, outputs and list of contents.
[`tfl_write_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_json.md)
writes it as the ARS JSON, the form the standard is exchanged in;
[`tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.md)
checks it. The specs stay the source: the ARS is written from them, not
edited.

## Usage

``` r
tfl_ars(
  ard_spec,
  table_spec = NULL,
  report_spec = NULL,
  profile = c("cdisc", "siera"),
  study_id = NULL,
  purpose = NULL,
  reason = "SPECIFIED IN SAP",
  dataset_names = NULL,
  dir = ".",
  references = NULL
)
```

## Arguments

- ard_spec:

  An ARD spec
  ([`tfl_read_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.md)).

- table_spec, report_spec:

  Optional table / report specs
  ([`tfl_read_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_table_spec.md),
  [`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md);
  one object may be given for both): the levels of a grouping, and each
  output's titles, footnotes, header, footer and file.

- profile:

  `"cdisc"`: the layout of CDISC's examples. `"siera"`: the same, made
  runnable by siera's `readARS()` (still valid CDISC ARS): each method
  carries an R code template, a proportion with its CI is an analysis of
  the variable, each output's subject count comes first, ids keep only
  letters, digits and `_`, and the list of outputs is there. An analysis
  siera has no template for (a test other than chi-square, `missing`,
  `mean_ci`, `custom` ...) is left out and listed by
  [`tfl_ars_unmapped()`](https://ichirio.github.io/tflspec/reference/tfl_ars_unmapped.md).
  [`tfl_ars_ard()`](https://ichirio.github.io/tflspec/reference/tfl_ars_ard.md)
  runs it.

- study_id:

  The reporting event's id; default the ARD spec's `study` key
  `study_id`, else `"STUDY"`.

- purpose, reason:

  For analyses whose `purpose` / `reason` is blank: one of the CDISC
  terms, or `NULL`. `reason` defaults to `"SPECIFIED IN SAP"`.

- dataset_names:

  A named character vector: the ADaM name the ARS uses for a spec
  dataset (default: the spec's name in upper case).

- dir:

  The study folder: the files of the study key `source` (its own ARD
  functions) are read from it – not run – for the statistics a function
  says it gives (`cards::as_cards_fn(stat_names = )`).

- references:

  The outputs that print another output's analyses (a figure printing a
  table's median and hazard ratio, \#293): a data frame with `output_id`
  (the figure), `source` (the table) and `analysis_id` (the table's
  analysis), a row each. Such an output is an ARS `Output` (its displays
  and file, from the report spec) whose list of contents names the
  source's analyses – they are not written twice; one whose analyses are
  not in the ARS is listed by
  [`tfl_ars_unmapped()`](https://ichirio.github.io/tflspec/reference/tfl_ars_unmapped.md).

## Value

A `tfl_ars`: the reporting event as a nested list, with attributes
`profile`, `unmapped` (a data frame `where`, `item`, `reason`: what the
ARS does not say) and `ids` (each spec analysis and the ARS analyses
written for it).

## Details

The layout follows CDISC's own example (Common Safety Displays):

- a population is an `AnalysisSet`, an analysis's `where` a `DataSubset`
  (an R condition on one variable at a time, joined by `&`, `|`, `!`,
  becomes a `WhereClause`);

- each `by` variable is an `AnalysisGrouping`; its groups are listed
  (and `dataDriven = FALSE`) when the table spec gives the variable's
  `levels`;

- a continuous variable is an analysis of the variable; a categorical
  one an analysis of the subject key grouped by the variable, its
  percentage pointing at the output's subject count by group as its
  denominator (added, and said in `unmapped`, when the output has none);

- a hierarchy (SOC / PT) is one analysis per depth;

- each method is an `AnalysisMethod` whose operations are its
  statistics; a `custom` or `pkg::function` method carries its code as
  the method's `codeTemplate`.

`purpose` and `reason` are the SAP's decisions, so they come from the
analyses' own `purpose` / `reason` columns (or the arguments), never
guessed. ARS requires a purpose: an analysis without one is written
without it and
[`tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.md)
names it.

## See also

[`tfl_write_ars_json()`](https://ichirio.github.io/tflspec/reference/tfl_write_ars_json.md),
[`tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.md),
[`tfl_ars_unmapped()`](https://ichirio.github.io/tflspec/reference/tfl_ars_unmapped.md)
