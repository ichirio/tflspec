# Oncology sample study XYZ-ONC-01: tumour sizes (ADTR)

The sum of diameters of target lesions of the simulated study XYZ-ONC-01
(see [adsl](https://ichirio.github.io/tflspec/reference/adsl.md)) at
baseline (at randomisation) and at each assessment, every 6 weeks until
progression, death or the cut-off: the data of a waterfall or spider
plot.

## Usage

``` r
adtr
```

## Format

`adtr`: a data frame with the subject keys (as in
[adrs](https://ichirio.github.io/tflspec/reference/adrs.md)) and

- PARAMCD, PARAM:

  `SUMDIAM`

- AVISIT, AVISITN, ADT, ADY:

  visit, date, study day

- AVAL, BASE, CHG, PCHG:

  sum of diameters (mm), baseline, change, percent change

- NADIR:

  the smallest sum before this assessment

- ABLFL, ANL01FL:

  baseline and analysis flags

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
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md),
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)
