# stat_block: the template stat_block

The table template `stat_block`, layout `stat_block`. Its program is one
call, `tfl_tpl(ard, "stat_block", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `by` | other | call | TRT01A | settings.treatment | the column key (the arms) |
| `by_levels` | other | call |  |  | its values in order; NULL: its code list's |
| `variable` | required | call |  |  | the analysis variable whose record is split (RESP) |
| `rows` | other | call |  |  | the row keys (subgroup, subgroup_level); NULL: one row of blocks |
| `cells` | required | call |  |  | the block's grid: heading = template, one element a column (or a row), a vector its lines (N: "{N}", ORR(%) 90%CI: \["{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}"\]) |
| `direction` | other | call | cols |  | the cells' elements as columns or rows |
| `label` | other | call |  |  | the row label column's heading |

## Example

    plan <- tfl_tpl(ard, "stat_block", variable = "RESP", cells = list(N = "{N}", "ORR (%)" = "{n} ({estimate:.1f%})"))

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("stat_block", tier = c("required", "usual"))
#> stat_block (table, layout stat_block; tflspec)
#> Call: tfl_tpl(ard, "stat_block", ...)
#> 
#> required (2):
#>   param    | effective (layer) | what
#>   variable | - (template)      | the analysis variable whose record is split (RESP)
#>   cells    | - (template)      | the block's grid: heading = template, one element a column (or a row), a vector its lines (N: "{N}", ORR(%) 90%CI: ["{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}"])
```
