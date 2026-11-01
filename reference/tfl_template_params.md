# A template's parameters

The parameters of a registered template, to start a template of one's
own from (a company's `demog` with its own default cells).

## Usage

``` r
tfl_template_params(name)
```

## Arguments

- name:

  The template's name.

## Value

A named list of
[`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md);
change one's `default` and pass them to
[`tfl_template()`](https://ichirio.github.io/tflspec/reference/tfl_template.md).

## Examples

``` r
p <- tfl_template_params("demog")
names(p)
#>  [1] "population"             "subset"                 "data"                  
#>  [4] "where"                  "by"                     "continuous"            
#>  [7] "categorical"            "labels"                 "order"                 
#> [10] "cells"                  "count_cell"             "continuous_statistics" 
#> [13] "categorical_statistics" "total"                  "stack"                 
p$cells$default
#> $n
#> [1] "{N:.0f}"
#> 
#> $`Mean (SD)`
#> [1] "{mean:.1f} ({sd:.2f})"
#> 
#> $Median
#> [1] "{median:.1f}"
#> 
#> $`Min, Max`
#> [1] "{min:.0f}, {max:.0f}"
#> 
```
