# Individual profile code (spaghetti / spider)

Individual profile code (spaghetti / spider)

## Usage

``` r
tfl_fig_individual(
  adam = NULL,
  style = c("spaghetti", "spider"),
  param = NULL,
  data = NULL,
  value = NULL,
  x = NULL,
  group = NULL,
  pop = NULL,
  where = NULL,
  response = "BOR",
  time_unit = "weeks",
  legend = "right",
  palette = NULL,
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 7.5,
  height = 4.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "individual"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `spaghetti` (one line per subject by visit, coloured by group, with
  the group means) or `spider` (percent change in tumour size over time,
  coloured by best overall response, with +20% / -30% lines).

- param:

  PARAMCD of the time-to-event parameter.

- data:

  Dataset name.

- value:

  Analysis variable (`AVAL`, `CHG`, `PCHG`, ...).

- x:

  Time variable (`AVISITN` for spaghetti, `ADY` for spider).

- group:

  Treatment variable; its first value is the reference.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- response:

  PARAMCD of the best overall response in `ADRS` (spider).

- time_unit:

  Unit shown for a day-based `x` (`days`, `weeks`, `months`).

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
