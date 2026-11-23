# Oncology sample study XYZ-ONC-01: exposure (ADEX)

Study drug exposure of the simulated study XYZ-ONC-01 (see
[adsl](https://ichirio.github.io/tflspec/reference/adsl.md)): one dose
every 21 days from the first to the last dose; a reduced dose follows a
related grade \>= 3 adverse event (`AEACN = "DOSE REDUCED"` in
[adae](https://ichirio.github.io/tflspec/reference/adae.md)).

## Usage

``` r
adex
```

## Format

`adex`: a data frame, one row per dose and four summary rows per treated
subject, with the subject keys (`STUDYID`, `USUBJID`, `TRT01P`,
`TRT01PN`, `TRT01A`, `TRT01AN`, `SAFFL`) and

- PARAMCD, PARAM:

  `DOSE` (each administration, mg), `TRTDURD` (days), `NCYCLE`,
  `CUMDOSE` (mg), `RDI` (relative dose intensity, %)

- AVISIT, AVISITN:

  cycle (`OVERALL` for the summaries)

- AVAL, AVALU:

  value and unit

- EXPLDOS, EXDOSRFL:

  planned dose; dose reduced flag

- ASTDT, AENDT:

  dates

## Source

Simulated by `data-raw/gen_onco.R` in the package's source, seed
20261016; not real patient data.

## See also

Other oncology sample data:
[`adae`](https://ichirio.github.io/tflspec/reference/adae.md),
[`adlb`](https://ichirio.github.io/tflspec/reference/adlb.md),
[`adrs`](https://ichirio.github.io/tflspec/reference/adrs.md),
[`adsl`](https://ichirio.github.io/tflspec/reference/adsl.md),
[`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md),
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md),
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)
