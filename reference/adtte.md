# Oncology sample study XYZ-ONC-01: time to event (ADTTE)

Progression-free survival, overall survival and duration of response of
the simulated study XYZ-ONC-01 (see
[adsl](https://ichirio.github.io/tflspec/reference/adsl.md)), in months;
and a safety endpoint, the time to the first treatment-emergent skin and
subcutaneous tissue disorder, in days. PFS: to progression or death,
censored at the last tumour assessment; OS: to death, censored at the
cut-off; DOR: from the first confirmed CR / PR; TTSKIN: the safety set,
from the first dose, censored at the end of the treatment-emergent
period (30 days after the last dose, death or the cut-off).

## Usage

``` r
adtte
```

## Format

`adtte`: a data frame, one row per subject and parameter, with the
subject keys (as in
[adrs](https://ichirio.github.io/tflspec/reference/adrs.md)), the
subgroups of a forest plot (`AGEGR1`, `SEX`, `RACE`, `ECOGBL`,
`REGION1`, `STRATA1`) and

- PARAMCD, PARAM, PARAMN:

  `PFS`, `OS`, `DOR`, `TTSKIN`

- STARTDT, ADT, ADY:

  origin and event / censoring dates

- AVAL, AVALU:

  time (`MONTHS`; `DAYS` for `TTSKIN`)

- CNSR, EVNTDESC, CNSDTDSC:

  censor (1) or event (0) and why

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
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)

## Examples

``` r
pfs <- adtte[adtte$PARAMCD == "PFS", ]
tapply(pfs$CNSR == 0, pfs$TRT01P, sum)
#>  Drug X Control 
#>      77      78 
```
