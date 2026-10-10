# Figure templates

A template fills a figure design's four parts at once – its data steps,
statistics, settings and layers – for a kind of figure; each piece is
then edited on its own
([`tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)).
Sizes, line widths and colours come from the figure style standard
([`tfl_fig_style()`](https://ichirio.github.io/tflspec/reference/tfl_fig_style.md)).
The templates of the types not yet in parts (forest, AE dot, butterfly,
eDISH, sankey, sunburst) give a design of one `figure` layer: the type's
whole script, with its arguments (`parts = FALSE` in
`tfl_fig_templates()`; see
[`tfl_fig_schema()`](https://ichirio.github.io/tflspec/reference/tfl_fig_schema.md)
for the arguments).

## Usage

``` r
tfl_fig_templates()

tfl_fig_template(
  template,
  data = NULL,
  param = NULL,
  pop = NULL,
  group = NULL,
  time = NULL,
  censor = "CNSR",
  time_unit = "months",
  value = NULL,
  visit = "AVISITN",
  visit_label = "AVISIT",
  x = NULL,
  y = NULL,
  at_visit = NULL,
  response_data = "ADRS",
  response = "BOR",
  category = "AVALC",
  responders = "CR, PR",
  id = "USUBJID",
  duration = "TRTDURD",
  join_adsl = NULL,
  title = NULL,
  ...
)
```

## Arguments

- template:

  One of `tfl_fig_templates()$template`.

- data, param, pop, group:

  The dataset, its PARAMCD, the analysis set flag and the group variable
  (joined from ADSL when `join_adsl`).

- time, censor, time_unit:

  KM: the time, the censor variable, the axis's unit. PK: `time` is the
  nominal time.

- value, visit, visit_label:

  The value and the visit (number and label): mean, box, spaghetti,
  scatter.

- x, y:

  Scatter: the two variables.

- at_visit:

  Box by group, shift: the visit kept (its label; empty = the last).

- response_data, response:

  Best response: its dataset and PARAMCD.

- category, responders:

  Bar: the category variable, and the values counted as response.

- id, duration:

  Swimmer: the subject and the bar's length (days).

- join_adsl:

  Join `group` (and `pop`) from ADSL (`TRUE` for the longitudinal kinds;
  KM, waterfall and swimmer read them from their own dataset).

- title:

  The figure's title.

- ...:

  For a whole-script template: the type's other arguments.

## Value

`tfl_fig_templates()`: a data frame (`template`, `kind`, `label`,
`parts`, `category`, `data`): `category` is the clinical category of
[`tfl_fig_catalog()`](https://ichirio.github.io/tflspec/reference/tfl_fig_catalog.md)
(Efficacy: time to event, Safety, PK / PD ...), `data` the datasets the
template reads, as the catalog writes them (`"ADTR + ADRS"`: both;
`"ADLB / ADVS + ADSL"`: ADLB or ADVS, and ADSL) – for a GUI's headings,
and to say which templates a study's data can draw;
`tfl_fig_template()`: a `tfl_fig_design`.
