# orr: the template orr

The table template `orr`, layout `stat_block`. Its program is one call,
`tfl_tpl(ard, "orr", ...)`; a parameter a report leaves out takes the
study standard's value, else the variant's default, else the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | other | call | ITT | output.population | the analysis set's id |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `by` | other | call | TRT01P | settings.treatment | the column variable |
| `by_levels` | other | call |  |  | its values in order; NULL: its code list's |
| `data` | other | call | ADRS |  | the response dataset |
| `param` | other | call | BOR |  | the PARAMCD of the best overall response |
| `category` | other | call | AVALC |  | the response category's variable |
| `responders` | other | call | CR, PR |  | the categories that respond |
| `where` | other | call |  |  | the records kept besides the parameter (R) |
| `data_id` | other | call |  |  | the analysis data's id; NULL: \_ |
| `ci_method` | other | call | clopper-pearson |  | the rate's CI (cardx's method) |
| `conf_level` | other | call | 0.95 |  | the rate's CI's level |
| `stat_layout` | other | call | rows |  | the rate and its CI: "rows", under each other in the arm's column, or "cols", each arm's two columns (stat_block) |
| `heading` | other | call | Response |  | the label column's heading |

## Example

    plan <- tfl_tpl(ard, "orr")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("orr", tier = c("required", "usual"))
#> orr (table, layout stat_block; tflspec)
#> Call: tfl_tpl(ard, "orr", ...)
```
