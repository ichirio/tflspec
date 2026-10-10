# Scatter plot code

Scatter plot code

## Usage

``` r
tfl_fig_scatter(
  adam = NULL,
  style = c("shift", "xy"),
  param = "ALT",
  data = "ADLB",
  x = NULL,
  y = NULL,
  visit = "AVISITN",
  visit_label = "AVISIT",
  at_visit = NULL,
  group = "TRT01A",
  pop = "SAFFL",
  where = NULL,
  legend = "inside_tl",
  palette = "treatment",
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 6,
  height = 5.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "scatter"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `shift` (baseline vs post-baseline value at one visit, with the
  identity line) or `xy` (any two variables with a linear fit per
  group).

- param:

  PARAMCD of the time-to-event parameter.

- data:

  Dataset name.

- x, y:

  Variables of `xy` (defaults `BASE` and `CHG`).

- visit, visit_label:

  Visit order variable and its label.

- at_visit:

  Visit label kept for `shift` (default: last visit).

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
