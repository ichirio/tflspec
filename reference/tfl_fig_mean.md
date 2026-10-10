# Mean over time code

Mean with error bars by visit and group.

## Usage

``` r
tfl_fig_mean(
  adam = NULL,
  style = c("se", "sd", "ci", "se_n"),
  param = "ALT",
  data = "ADLB",
  value = "AVAL",
  visit = "AVISITN",
  visit_label = "AVISIT",
  group = "TRT01A",
  pop = "SAFFL",
  where = NULL,
  flag = NULL,
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
  plot_id = "mean"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `se` (mean +/- SE), `sd` (mean +/- SD), `ci` (mean with 95% CI) or
  `se_n` (mean +/- SE with a table of n below).

- param:

  PARAMCD of the time-to-event parameter.

- data:

  Dataset name.

- value:

  Analysis variable (`AVAL`, `CHG`, `PCHG`, ...).

- visit, visit_label:

  Visit order variable and its label.

- group:

  Treatment variable; its first value is the reference.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- flag:

  Optional record flag (`== "Y"`), e.g. `ANL01FL`.

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
