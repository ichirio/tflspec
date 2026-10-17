# The templates

The templates

## Usage

``` r
tfl_templates()
```

## Value

A data frame, one row a template: `name`, `kind`, `layout`, `params`
(its parameters, comma-separated).

## Examples

``` r
tfl_templates()
#>            name    kind       layout
#> 1     ae_soc_pt   table hierarchical
#> 2         demog   table      summary
#> 3   disposition   table      summary
#> 4  hierarchical   table hierarchical
#> 5     km_simple  figure           km
#> 6      km_table   table     km_table
#> 7     lab_shift   table        shift
#> 8       listing listing      listing
#> 9           orr   table      summary
#> 10        shift   table        shift
#> 11      summary   table      summary
#> 12    vs_change   table      summary
#>                                                                                                                                                                                                           params
#> 1                                                                                               population, hierarchy, where, data_label, analysis_id, analysis_label, overall, by, data, data_id, sort, heading
#> 2                                                                                                                                            population, data, by, continuous, categorical, labels, cells, stack
#> 3                                                                                                                           population, categorical, cat_id, cat_label, labels, by, cat_statistics, groupn_label
#> 4                                                                                               population, hierarchy, by, data, where, data_id, data_label, analysis_id, analysis_label, overall, sort, heading
#> 5                                                                                                                                                               param, pop, group, time, censor, time_unit, data
#> 6                                                                                                                                               by, by_levels, median_label, time_prefix, time_unit, drop_time_0
#> 7                                                                                                                                        population, visit, params, by, data_id, by_levels, param_labels, levels
#> 8                                                                                                                                                                                     data, columns, where, sort
#> 9                                                                                                                      population, by, by_levels, data, param, category, responders, data_id, ci_method, heading
#> 10                                                                                      population, by, data, where, data_id, analysis_id, param, baseline, post, levels, by_levels, param_labels, cell, heading
#> 11 population, variables, categorical, by, data, where, data_id, data_label, groupn_label, analysis_id, analysis_label, statistics, ci_id, ci_label, ci_method, cat_id, cat_label, cat_statistics, cells, labels
#> 12                                                                                                                                            population, param, visit, param_label, when, visit_label, unit, by
```
