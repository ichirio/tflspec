# Oncology sample study XYZ-ONC-01: tumour response (ADRS)

RECIST 1.1 responses of the simulated study XYZ-ONC-01 (see
[adsl](https://ichirio.github.io/tflspec/reference/adsl.md)): the
investigator's overall response at each assessment (`OVR`: CR when the
sum of diameters is 0 mm, PR at least 30% below baseline, PD at least
20% and 5 mm above the nadir or a new lesion, else SD) and the best
confirmed overall response (`BOR`: CR / PR confirmed by the next
assessment; NE without a post-baseline assessment), one per subject.

## Usage

``` r
adrs
```

## Format

`adrs`: a data frame with the subject keys (`STUDYID`, `USUBJID`,
`TRT01P`, `TRT01PN`, `TRT01A`, `TRT01AN`, `ITTFL`, `SAFFL`, `EFFFL`) and

- PARAMCD, PARAM:

  `OVR` or `BOR`

- AVISIT, AVISITN, ADT, ADY:

  visit (`WEEK 6`, ...; `OVERALL` for BOR), date, study day

- AVALC, AVAL:

  response (CR, PR, SD, PD, NE) and its code 1-5

- ANL01FL:

  analysis flag

## Source

Simulated by `data-raw/gen_onco.R` in the package's source, seed
20261016; not real patient data.

## See also

Other oncology sample data:
[`adae`](https://ichirio.github.io/tflspec/reference/adae.md),
[`adex`](https://ichirio.github.io/tflspec/reference/adex.md),
[`adlb`](https://ichirio.github.io/tflspec/reference/adlb.md),
[`adsl`](https://ichirio.github.io/tflspec/reference/adsl.md),
[`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md),
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md),
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)
