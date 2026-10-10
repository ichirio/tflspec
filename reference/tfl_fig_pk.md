# PK concentration-time plot code

PK concentration-time plot code

## Usage

``` r
tfl_fig_pk(
  adam = NULL,
  style = c("mean", "mean_log", "individual"),
  param = NULL,
  data = "ADPC",
  value = "AVAL",
  time = "NFRLT",
  time_label = "Nominal time (h)",
  group = "TRT01A",
  pop = "SAFFL",
  where = NULL,
  legend = "inside",
  palette = "treatment",
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 7.5,
  height = 4.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "pk"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `mean` (mean +/- SD by nominal time and group, linear axis),
  `mean_log` (the same on a log axis) or `individual` (one line per
  subject, log axis, one panel per group).

- param:

  PARAMCD of the time-to-event parameter.

- data:

  Dataset name.

- value:

  Analysis variable (`AVAL`, `CHG`, `PCHG`, ...).

- time:

  Nominal time variable.

- time_label:

  Axis label of `time`.

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
