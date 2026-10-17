# The days in a week, a month and a year

The figure programs divide a time in days by named constants –
`days_per_week`, `days_per_month`, `days_per_year` – not by numbers
written in each program, so a company sets them once (a study's setup:
tflplanner writes them from its company standards). These are the values
used where none is set: the figure setup
([`tfl_fig_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md))
and a figure script of its own define a constant only when it is not
defined yet. The programs say the name as it is
(`AVAL / days_per_month`): a data column must not be named like one
(tflplanner's review says so).

## Usage

``` r
tfl_time_unit_days()
```

## Value

A named numeric vector: `days_per_week` 7, `days_per_month` 30.4375
(365.25 / 12), `days_per_year` 365.25.

## Examples

``` r
tfl_time_unit_days()
#>  days_per_week days_per_month  days_per_year 
#>         7.0000        30.4375       365.2500 
```
