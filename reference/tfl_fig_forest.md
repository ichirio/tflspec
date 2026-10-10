# Forest plot code

Estimates by subgroup with a text column (N, estimate and 95% CI).

## Usage

``` r
tfl_fig_forest(
  adam = NULL,
  style = c("hr", "or", "estimates"),
  param = NULL,
  group = "TRT01P",
  subgroups = c("SEX", "AGEGR1"),
  pop = "FASFL",
  where = NULL,
  data = NULL,
  responders = c("CR", "PR"),
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 8,
  height = 5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "forest"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `hr` (Cox hazard ratio from a time-to-event parameter), `or` (odds
  ratio of response from logistic regression) or `estimates` (a data
  frame `est_df` you provide with `label`, `est`, `lcl`, `ucl` and
  optionally `n`).

- param:

  PARAMCD of the time-to-event parameter.

- group:

  Treatment variable; its first value is the reference.

- subgroups:

  ADSL variables defining the subgroups.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- data:

  Dataset name.

- responders:

  Values of `AVALC` counted as responders (`or`).

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
