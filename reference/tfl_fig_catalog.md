# Catalogue of clinical figure types

Every figure type and style (subtype) tflspec knows, grouped by clinical
category. `status = "planned"` rows are classified but not generated
yet.

## Usage

``` r
tfl_fig_catalog(status = NULL)
```

## Arguments

- status:

  `"implemented"`, `"planned"` or `NULL` for all.

## Value

A data frame: `category`, `type`, `style`, `default`, `status`, `fun`
(quick function), `engine`, `data` (default input), `subtypes`
(argument-level variants) and `description`.

## Examples

``` r
cat <- tfl_fig_catalog()
table(cat$category, cat$status)
#>                                
#>                                 implemented planned
#>   Distribution / association              2       2
#>   Efficacy: subgroups and rates           6       0
#>   Efficacy: time to event                 4       1
#>   Efficacy: tumour response               7       0
#>   Longitudinal                            8       1
#>   PK / PD                                 3       2
#>   Safety                                  6       2
#>   Treatment patterns                      4       0
```
