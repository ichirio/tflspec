# The statistics an ARD analysis may ask for

Each statistic's `kind` – which methods give it: `continuous`,
`categorical`, `missing`, or `result` (what a confidence interval, a
test or a model gives) –, its label, the format its `stat_fmt` gets
unless the method or the analysis gives another, and, for the continuous
statistics cards does not compute itself (CV, geometric mean,
percentiles ...), the R function written into the ARD program.

## Usage

``` r
tfl_ard_statistics(kind = NULL)
```

## Arguments

- kind:

  Only the statistics of these kinds; `NULL` for all.

## Value

A data frame: `statistic`, `kind`, `group`, `label`, `fmt`, `fun`,
`note`.

## Details

An analysis's `formats` are `statistic=format` pairs, `|` between them
(`mean=xx.xx | sd=xx.xxx`); `VARIABLE:statistic=format` for one variable
only. A format is `xx.x` (as many x after the point as decimals),
`xx.x%` (a proportion as a percent), a number of decimals, or `pvalue`
(`<0.001`, else 3 decimals).

The catalog is the built-in one unless
`options(tflspec.ard_statistics = )` holds another, or a function that
uses it is given `statistics =`.
