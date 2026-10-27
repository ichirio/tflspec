# km_table: the template km_table

The table template `km_table`, layout `km_table`. Its program is one
call, `tfl_tpl(ard, "km_table", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `by_levels` | other | call |  |  | its values in order; NULL: its code list's |
| `median_label` | other | call | Median (95% CI) |  | the median's row |
| `time_prefix` | other | call | Day |  | a rate's row: |
| `time_unit` | other | call | days |  | the times' unit, in the median block's heading |
| `drop_time_0` | other | call | TRUE |  | leave out the rate at time 0 |

## Example

    plan <- tfl_tpl(ard, "km_table")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("km_table", tier = c("required", "usual"))
#> km_table (table, layout km_table; tflspec)
#> Call: tfl_tpl(ard, "km_table", ...)
```
