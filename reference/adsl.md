# Oncology sample study XYZ-ONC-01: subject level (ADSL)

One of the eight ADaM datasets of a simulated oncology study: Drug X vs
Control, randomised 1:1 (stratified by baseline ECOG and region), RECIST
1.1 every 6 weeks, data cut-off 30 June 2026; 200 subjects screened, 160
randomised. The datasets agree with one another: they are made from one
set of simulated subject histories, so a confirmed CR/PR has its tumour
sizes at least 30% below baseline, nothing is assessed after
progression, the PFS and OS events are the progressions and the deaths,
and doses stop at progression, toxicity, death or the cut-off. `TRT01P`,
`TRT01A` and `ARM` are factors with Drug X first.

## Usage

``` r
adsl
```

## Format

`adsl`: a data frame, one row per screened subject (200 rows), with

- STUDYID, USUBJID, SUBJID, SITEID:

  identifiers

- COUNTRY, REGION1:

  country; region (North America, Europe, Asia)

- AGE, AGEGR1, SEX, RACE, ETHNIC:

  demographics (`AGEGR1`: `<65`, `>=65`)

- ECOGBL, STRATA1:

  baseline ECOG (0, 1; 2 in some screen failures); randomisation stratum

- ARM, TRT01P, TRT01A:

  planned / actual treatment (factors, Drug X first; `TRT01A` is `NA`
  for the subjects never treated, all three for the screen failures)

- ARMCD, ACTARM, ACTARMCD, ARMNRS:

  arm codes; why an arm is null (`SCREEN FAILURE`,
  `ASSIGNED, NOT TREATED`)

- TRT01PN, TRT01AN:

  their numeric codes (1 Drug X, 2 Control)

- RFICDT, RANDDT, TRTSDT, TRTEDT, TRTDURD:

  informed consent, randomisation, first and last dose dates, treatment
  duration (days)

- ENRLFL, ITTFL, SAFFL, EFFFL:

  enrolled (randomised), intent-to-treat, safety and efficacy evaluable
  population flags

- EOTSTT, DCTREAS:

  end of treatment status and reason

- EOSSTT, EOSDT, DCSREAS, DCSREASP:

  end of study status (`ONGOING` in follow-up at the cut-off,
  `DISCONTINUED`), its date, reason (`DEATH`, `SCREEN FAILURE`) and the
  screen failure's reason

- DTHFL, DTHDT, DTHCAUS, LSTALVDT, DCUTDT:

  death; date last known alive; data cut-off date

- BSLD:

  baseline sum of diameters of target lesions (mm)

## Source

Simulated by `data-raw/gen_onco.R` in the package's source, seed
20261016; not real patient data.

## Details

ADSL holds every screened subject: the 160 randomised (`ENRLFL`,
`ITTFL`) and 40 screen failures, who have no arm (`ARM`, `TRT01P` ...
`NA`, `ARMNRS` "SCREEN FAILURE", as SDTMIG 3.3 has it) and no records in
the other datasets. An analysis of the randomised subjects selects them
(`ITTFL == "Y"`, or `SAFFL` / `EFFFL`).

The study's mini SAP (populations, endpoints, a display list) and CRF
outline are installed with the package:
`system.file("sample", "onco", "SAP.md", package = "tflspec")` and
`"CRF.md"`. Variables carry CDISC labels (`attr(x, "label")`).

## See also

The other datasets of the study:
[adex](https://ichirio.github.io/tflspec/reference/adex.md),
[adae](https://ichirio.github.io/tflspec/reference/adae.md),
[adlb](https://ichirio.github.io/tflspec/reference/adlb.md),
[advs](https://ichirio.github.io/tflspec/reference/advs.md),
[adrs](https://ichirio.github.io/tflspec/reference/adrs.md),
[adtr](https://ichirio.github.io/tflspec/reference/adtr.md),
[adtte](https://ichirio.github.io/tflspec/reference/adtte.md); the
study's SAP and CRF outline,
`system.file("sample", "onco", package = "tflspec")`.

Other oncology sample data:
[`adae`](https://ichirio.github.io/tflspec/reference/adae.md),
[`adex`](https://ichirio.github.io/tflspec/reference/adex.md),
[`adlb`](https://ichirio.github.io/tflspec/reference/adlb.md),
[`adrs`](https://ichirio.github.io/tflspec/reference/adrs.md),
[`adtr`](https://ichirio.github.io/tflspec/reference/adtr.md),
[`adtte`](https://ichirio.github.io/tflspec/reference/adtte.md),
[`advs`](https://ichirio.github.io/tflspec/reference/advs.md)

## Examples

``` r
table(adsl$ARMNRS, useNA = "ifany")
#> 
#> ASSIGNED, NOT TREATED        SCREEN FAILURE                  <NA> 
#>                     2                    40                   158 
itt <- adsl[adsl$ITTFL == "Y", ]
table(itt$TRT01P, itt$DTHFL)
#>          
#>            N  Y
#>   Drug X  40 42
#>   Control 27 51
```
