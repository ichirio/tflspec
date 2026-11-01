# Oncology sample study XYZ-ONC-01: adverse events (ADAE)

Adverse events of the simulated study XYZ-ONC-01 (see
[adsl](https://ichirio.github.io/tflspec/reference/adsl.md)), coded with
a small MedDRA-like dictionary (31 preferred terms in 12 system organ
classes) and graded by CTCAE. Treatment-emergent events start from the
first dose to 30 days after the last; a few start between randomisation
and the first dose (`TRTEMFL = "N"`). A death before progression on
treatment is a grade 5 event on the date of death.

## Usage

``` r
adae
```

## Format

`adae`: a data frame, one row per event, with the subject keys (as in
[adex](https://ichirio.github.io/tflspec/reference/adex.md)) and

- AESEQ, AETERM, AEDECOD, AEBODSYS, AESOC:

  the event and its coding

- AETOXGR, ATOXGR, AESEV:

  CTCAE grade (1-5); severity

- AESER, AEREL, AEACN, AEOUT, AESDTH:

  serious, causality, action with the study drug, outcome, results in
  death

- ASTDT, AENDT, ASTDY, AENDY:

  start / end dates and study days

- TRTEMFL:

  treatment-emergent flag

## Source

Simulated by `data-raw/gen_onco.R` in the package's source, seed
20261016; not real patient data.

## See also

Other oncology sample data:
[`adex`](https://ichirio.github.io/tflspec/reference/adex.md),
[`adlb`](https://ichirio.github.io/tflspec/reference/adlb.md),
[`adrs`](https://ichirio.github.io/tflspec/reference/adrs.md),
[`adsl`](https://ichirio.github.io/tflspec/reference/adsl.md),
[`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md),
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md),
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)
