# AE dot plot code

Incidence by preferred term for two arms, with the risk difference and
its 95% CI (Wald) in a second panel.

## Usage

``` r
tfl_fig_ae_dot(
  adam = NULL,
  style = c("risk_diff", "incidence"),
  data = "ADAE",
  term = "AEDECOD",
  group = "TRT01A",
  pop = "SAFFL",
  where = NULL,
  tefl = "TRTEMFL",
  top = 20,
  min_pct = 5,
  legend = "bottom",
  palette = "treatment",
  key = "USUBJID",
  theme = "boxed",
  title = NULL,
  width = 8,
  height = 5.5,
  dpi = 300,
  units = "in",
  file = NULL,
  plot_id = "ae_dot"
)
```

## Arguments

- adam:

  Optional ADaM data
  ([`tfl_read_adam()`](https://ichirio.github.io/tflspec/reference/tfl_read_adam.md));
  values found in the data (groups, colours) are written literally into
  the code.

- style:

  `risk_diff` (incidence + risk difference) or `incidence`.

- data:

  Dataset name.

- term:

  AE term variable.

- group:

  Treatment variable; its first value is the reference.

- pop:

  Population flag (`== "Y"`); `NULL` for none.

- where:

  Extra record condition as R code, e.g. `'AVISIT != "Retrieval"'`;
  `NULL` for none.

- tefl:

  Treatment-emergent flag (`NULL` for all records).

- top, min_pct:

  Show at most `top` terms with incidence of at least `min_pct` percent
  in any arm.

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
