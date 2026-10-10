# Bar chart code for rates and category percentages

Bar chart code for rates and category percentages

## Usage

``` r
tfl_fig_bar(
  adam = NULL,
  style = c("rate_ci", "stacked", "dodged"),
  param = "BOR",
  data = "ADRS",
  category = "AVALC",
  group = "TRT01P",
  pop = "FASFL",
  where = NULL,
  responders = c("CR", "PR"),
  legend = NULL,
  palette = NULL,
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 7,
  height = 4.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "bar"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `rate_ci` (response rate by group with exact 95% CI), `stacked` (100%
  stacked bars of a category by group) or `dodged` (percentages of each
  category, groups side by side).

- param:

  PARAMCD of the time-to-event parameter.

- data:

  Dataset name.

- category:

  Categorical variable (default `AVALC`).

- group:

  Treatment variable; its first value is the reference.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- responders:

  Values of `AVALC` counted as responders (`or`).

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
