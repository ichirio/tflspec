# Box plot code

Box plot code

## Usage

``` r
tfl_fig_box(
  adam = NULL,
  style = c("by_visit", "by_group", "change"),
  param = "ALT",
  data = "ADLB",
  value = NULL,
  visit = "AVISITN",
  visit_label = "AVISIT",
  at_visit = NULL,
  group = "TRT01A",
  pop = "SAFFL",
  where = NULL,
  legend = "bottom",
  palette = "treatment",
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 7.5,
  height = 4.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "box"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `by_visit` (boxes by visit and group), `by_group` (one box per group
  at one visit, with the data points) or `change` (change from baseline
  by visit and group, with a zero line).

- param:

  PARAMCD of the time-to-event parameter.

- data:

  Dataset name.

- value:

  Analysis variable (`AVAL`, `CHG`, `PCHG`, ...).

- visit, visit_label:

  Visit order variable and its label.

- at_visit:

  Visit label kept for `by_group` (default: last visit).

- group:

  Treatment variable; its first value is the reference.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- legend:

  Legend preset (`none`, `right`, `bottom`, `top`, `inside`, ...).

- palette:

  Palette preset of the group (`rate_ci`, `dodged`) or of the category
  (`stacked`, default `response`).

- key:

  Subject key used to join ADSL.

- theme:

  Theme preset.

- title:

  Figure title.

- width, height, dpi, units:

  Figure size.

- file:

  Write the script to this file.

- plot_id:

  Used in the output file name.
