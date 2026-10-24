# Oncology sample study XYZ-ONC-01: laboratory (ADLB)

Six laboratory parameters of the simulated study XYZ-ONC-01 (see
[adsl](https://ichirio.github.io/tflspec/reference/adsl.md)) on day 1 of
each cycle (cycle 1 is the baseline): ALT, AST, bilirubin, haemoglobin,
neutrophils, platelets, with normal ranges and CTCAE grades for shift
tables.

## Usage

``` r
adlb
```

## Format

`adlb`: a data frame, one row per subject, parameter and visit, with the
subject keys (as in
[adex](https://ichirio.github.io/tflspec/reference/adex.md)) and

- PARAMCD, PARAM, PARCAT1:

  `ALT`, `AST`, `BILI`, `HGB`, `NEUT`, `PLAT`; chemistry or haematology

- AVISIT, AVISITN, ADT, ADY:

  visit, date, study day

- AVAL, BASE, CHG, PCHG:

  value, baseline, change, percent change

- ANRLO, ANRHI, ANRIND, BNRIND:

  normal range and indicator (and at baseline)

- ATOXGR, BTOXGR:

  CTCAE grade (and at baseline)

- ABLFL, ONTRTFL:

  baseline and on-treatment record flags

## Source

Simulated by `data-raw/gen_onco.R` in the package's source, seed
20261016; not real patient data.

## See also

Other oncology sample data:
[`adae`](https://ichirio.github.io/tflspec/reference/adae.md),
[`adex`](https://ichirio.github.io/tflspec/reference/adex.md),
[`adrs`](https://ichirio.github.io/tflspec/reference/adrs.md),
[`adsl`](https://ichirio.github.io/tflspec/reference/adsl.md),
[`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md),
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md),
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)
