# Oncology sample study XYZ-ONC-01: vital signs (ADVS)

Weight, blood pressure and pulse of the simulated study XYZ-ONC-01 (see
[adsl](https://ichirio.github.io/tflspec/reference/adsl.md)) on day 1 of
each cycle.

## Usage

``` r
advs
```

## Format

`advs`: a data frame, one row per subject, parameter and visit, with the
subject keys (as in
[adex](https://ichirio.github.io/tflspec/reference/adex.md)) and
`PARAMCD` (`WEIGHT`, `SYSBP`, `DIABP`, `PULSE`), `PARAM`, `AVISIT`,
`AVISITN`, `ADT`, `ADY`, `AVAL`, `BASE`, `CHG`, `ABLFL`, `ONTRTFL`.

## Source

Simulated by `data-raw/gen_onco.R` in the package's source, seed
20261016; not real patient data.

## See also

Other oncology sample data:
[`adae`](https://ichirio.github.io/tflspec/reference/adae.md),
[`adex`](https://ichirio.github.io/tflspec/reference/adex.md),
[`adlb`](https://ichirio.github.io/tflspec/reference/adlb.md),
[`adrs`](https://ichirio.github.io/tflspec/reference/adrs.md),
[`adsl`](https://ichirio.github.io/tflspec/reference/adsl.md),
[`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md),
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md)
