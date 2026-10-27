# Kaplan-Meier plot of the cumulative percentage (1 - S(t)): CI bars, number at risk, p-value: the template km_cumulative

The figure template `km_cumulative`, layout `km`: a variant of `km` (its
defaults changed). Its program is one call,
`tfl_tpl(adtte, "km_cumulative", ...)`; a parameter a report leaves out
takes the study standard's value, else the variant's default, else the
layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `time` | usual | call | AVAL |  | the time variable (Surv(time, ...)), in 'time_unit' |
| `time_unit` | usual | call | months |  | the time's unit: the call's first step converts the days to it; the x axis's title |
| `censor` | usual | call | CNSR |  | the censoring variable, 1 censored (Surv(time, censor == 0)) |
| `by` | required | call |  | settings.treatment | the curves' variable (survfit2(~ by), the colours) |
| `y_percent` | other | call | TRUE |  | TRUE: the y axis in % (scale_y_continuous(labels = scales::label_percent())) |
| `ci` | usual | call | bars |  | the confidence interval: band (add_confidence_interval()), bars at the axis's times (geom_errorbar()) |
| `ci_times` | other | call |  |  | the bars' times (summary(fit, times = )); NULL: the x axis's breaks |
| `conf_level` | other | call |  |  | the interval's level (survfit2(conf.int = )); NULL: 0.95 |
| `conf_type` | other | call |  |  | the interval's transformation (survfit2(conf.type = )); NULL: log |
| `censor_marks` | usual | call | FALSE |  | the censor marks (add_censor_mark()) |
| `median_line` | usual | call | FALSE |  | the line at 50% (geom_hline(yintercept = 0.5)) |
| `risk_table` | usual | call | TRUE |  | the number at risk under the curves (add_risktable(times = x_breaks)) |
| `risk_statistics` | other | call |  |  | the risk table's rows (add_risktable(risktable_stats = )); NULL: n.risk |
| `x_max` | usual | call |  |  | the x axis's end (x_breaks \<- seq(0, x_max, by = x_step)); NULL: the last time |
| `x_step` | usual | call |  |  | the x axis's step, the risk table's times; NULL: pretty() |
| `x_label` | usual | call |  |  | the x axis's title (labs(x = )); NULL: Time () |
| `y_min` | other | call |  |  | the y axis's start (coord_cartesian(ylim = )); NULL: 0 |
| `y_max` | other | call |  |  | the y axis's end; NULL: 1 |
| `y_step` | other | call |  | km.y_by | the y axis's step (scale_y_continuous(breaks = )); NULL: the figure style's |
| `y_label` | usual | call |  |  | the y axis's title (labs(y = )); NULL: by y_type |
| `linetype_by` | other | call | FALSE |  | TRUE: a line type an arm too (ggsurvfit(linetype_aes = TRUE)) |
| `legend` | usual | call | bottom |  | the legend's place (theme(legend.position = )); NULL: inside |
| `legend_title` | other | call |  |  | the legend's title (labs(colour = )); NULL: none |
| `pvalue` | usual | call | TRUE |  | the log-rank p-value (add_pvalue()) |
| `pvalue_location` | other | call | annotation |  | where the p-value goes (add_pvalue(location = )) |
| `title` | other | call |  |  | the figure's title (labs(title = )) |
| `plot` | other | call |  |  | any other figure setting (plot:): dpi, facet_by, add ..., over the template's |

## Example

    fig <- tfl_tpl(adtte, "km_cumulative", by = "TRT01A", time_unit = "weeks")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("km_cumulative", tier = c("required", "usual"))
#> km_cumulative (figure, layout km; variant of km; tflspec)
#> Kaplan-Meier plot of the cumulative percentage (1 - S(t)): CI bars, number at risk, p-value
#> Call: tfl_tpl(adtte, "km_cumulative", ...)
#> 
#> required (1):
#>   param | effective (layer) | what
#>   by    | - (template)      | the curves' variable (survfit2(~ by), the colours)
#> 
#> usual (13):
#>   param        | effective (layer)              | what
#>   time         | AVAL (template)                | the time variable (Surv(time, ...)), in `time_unit`
#>   time_unit    | months (template)              | the time's unit: the call's first step converts the days to it; the x axis's title
#>   censor       | CNSR (template)                | the censoring variable, 1 censored (Surv(time, censor == 0))
#>   ci           | bars (variant:km_cumulative)   | the confidence interval: band (add_confidence_interval()), bars at the axis's times (geom_errorbar())
#>   censor_marks | FALSE (variant:km_cumulative)  | the censor marks (add_censor_mark())
#>   median_line  | FALSE (variant:km_cumulative)  | the line at 50% (geom_hline(yintercept = 0.5))
#>   risk_table   | TRUE (variant:km_cumulative)   | the number at risk under the curves (add_risktable(times = x_breaks))
#>   x_max        | - (template)                   | the x axis's end (x_breaks <- seq(0, x_max, by = x_step)); NULL: the last time
#>   x_step       | - (template)                   | the x axis's step, the risk table's times; NULL: pretty()
#>   x_label      | - (template)                   | the x axis's title (labs(x = )); NULL: Time (<unit>)
#>   y_label      | - (template)                   | the y axis's title (labs(y = )); NULL: by y_type
#>   legend       | bottom (variant:km_cumulative) | the legend's place (theme(legend.position = )); NULL: inside
#>   pvalue       | TRUE (variant:km_cumulative)   | the log-rank p-value (add_pvalue())
```
