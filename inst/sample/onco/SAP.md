# Study XYZ-ONC-01: statistical analysis plan (sample)

A fictitious study for the samples of the tfl series.  Its ADaM datasets
ship with tflspec as `adsl`, `adex`, `adae`, `adlb`, `advs`, `adrs`, `adtr`
and `adtte` (see `?adsl`); they were simulated by `data-raw/gen_onco.R` in
the package's source (seed 20261016).  Nothing here describes a real drug,
a real study or real patients.

```r
library(tflspec)
table(adsl$TRT01P)                         # Drug X first: a factor
adrs[adrs$PARAMCD == "BOR", c("USUBJID", "TRT01P", "AVALC")]
```

## 1. Design and objectives

A randomised, open-label, two-arm study in adults with advanced solid
tumours and measurable disease (RECIST 1.1).  Subjects are randomised 1:1
to **Drug X** (200 mg every 21 days) or **Control** (100 mg every 21 days),
stratified by baseline ECOG performance status (0, 1) and region (North
America, Europe, Asia), in permuted blocks of 4.  Treatment continues until
progression, unacceptable toxicity or death; subjects are then followed for
survival.  Enrolment ran from July 2024 to June 2025: 200 subjects were
screened, 40 failed screening and 160 were randomised.

- Primary objective: progression-free survival (PFS) of Drug X vs Control.
- Secondary: overall survival (OS), objective response rate (ORR), disease
  control rate (DCR), duration of response (DoR), safety.

## 2. Populations

| Population | Definition | ADSL flag |
|---|---|---|
| Screened | every subject who signed the informed consent (all of ADSL); screen failures have no arm (`ARMNRS` "SCREEN FAILURE") | -- |
| Enrolled | the screened subjects who were randomised | `ENRLFL` |
| Intent-to-treat (ITT) | all randomised subjects, by planned treatment | `ITTFL` |
| Safety (SAF) | subjects who received at least one dose, by actual treatment | `SAFFL` |
| Efficacy evaluable (EE) | SAF subjects with at least one post-baseline tumour assessment | `EFFFL` |

Efficacy is analysed in the ITT population (EE as sensitivity), safety in
the SAF population; disposition and screen failures in the screened subjects.

**Disposition.**  End of treatment (`EOTSTT`, `DCTREAS`): ongoing at the
cut-off, or discontinued for progressive disease, an adverse event or
death.  End of study (`EOSSTT`, `EOSDT`, `DCSREAS`): a randomised subject
is in follow-up (`ONGOING`) until death (`DISCONTINUED`, `DEATH`); a screen
failure ends the study at screening (`SCREEN FAILURE`, its reason in
`DCSREASP`).

## 3. Tumour assessments

RECIST 1.1 by the investigator: a baseline scan at randomisation, then every
6 weeks (± 3 days) from the first dose until progression, death or the
cut-off, also after treatment stops for another reason.  The sum of
diameters of target lesions is `ADTR.PARAMCD = "SUMDIAM"`; the overall
response at each assessment is `ADRS.PARAMCD = "OVR"`:

- CR: sum of diameters 0 mm;
- PR: at least 30% below baseline;
- PD: at least 20% (and 5 mm) above the nadir, or a new lesion;
- SD: otherwise.

The best overall response (`ADRS.PARAMCD = "BOR"`) needs **confirmation**:
CR or PR confirmed by the next assessment (at least 4 weeks later; a PR by a
PR or a CR).  An unconfirmed CR/PR counts as SD; a subject with no
post-baseline assessment is NE.

## 4. Endpoints

| Endpoint | Definition | Data |
|---|---|---|
| ORR | BOR CR or PR | ADRS `BOR` |
| DCR | BOR CR, PR or SD | ADRS `BOR` |
| PFS | randomisation to PD or death, whichever first; censored at the last adequate tumour assessment (at randomisation without one) | ADTTE `PFS` |
| OS | randomisation to death; censored at the cut-off (date last known alive) | ADTTE `OS` |
| DoR | first confirmed CR/PR to PD or death; censored as PFS | ADTTE `DOR` |
| Time to first skin disorder (safety) | first dose to the first treatment-emergent AE of the SOC "Skin and subcutaneous tissue disorders"; censored at the end of the treatment-emergent period (30 days after the last dose, death or the cut-off) | ADTTE `TTSKIN` |

Times are in months (days / 30.4375), the safety endpoint's in days.

## 5. Methods

- ORR and DCR with exact (Clopper-Pearson) 95% CIs by arm; the difference
  with a 95% CI and a Cochran-Mantel-Haenszel test stratified by the
  randomisation strata.
- PFS, OS, DoR: Kaplan-Meier estimates (median with 95% CI, rates at 6 and
  12 months), a stratified log-rank test, the hazard ratio from a stratified
  Cox model.  Subgroups (forest plot): age group (`AGEGR1`), sex, baseline
  ECOG (`ECOGBL`), race, region.
- Safety: treatment-emergent adverse events (onset from the first dose to
  30 days after the last) by SOC and PT, by worst CTCAE grade; exposure;
  laboratory values over time and shifts from baseline in CTCAE grade.
- No multiplicity adjustment or interim analysis (a sample).

## 6. Data cut-off

All data up to **30 June 2026** (`DCUTDT`): nothing after it is in the data.

## 7. Displays

| No. | Title | Population | Data |
|---|---|---|---|
| T-1 | Subject disposition (screened, randomised, treated; end of treatment and of study) | Screened | ADSL |
| T-2 | Demographics and baseline characteristics | ITT | ADSL |
| T-3 | Study drug exposure | SAF | ADEX |
| T-4 | Best overall response, ORR and DCR with 95% CI | ITT | ADRS |
| T-5 | Progression-free survival | ITT | ADTTE |
| T-6 | Overall survival | ITT | ADTTE |
| T-7 | Duration of response | ITT responders | ADTTE |
| T-8 | Treatment-emergent AEs by SOC and PT | SAF | ADAE |
| T-9 | Treatment-emergent AEs by SOC, PT and worst grade | SAF | ADAE |
| T-10 | Serious AEs, AEs leading to discontinuation, deaths | SAF | ADAE, ADSL |
| T-11 | Shift from baseline to worst CTCAE grade (laboratory) | SAF | ADLB |
| T-12 | Time to first treatment-emergent skin disorder (Kaplan-Meier) | SAF | ADTTE |
| F-1 | Kaplan-Meier plot of PFS (with numbers at risk) | ITT | ADTTE |
| F-2 | Kaplan-Meier plot of OS | ITT | ADTTE |
| F-3 | Forest plot of the PFS hazard ratio by subgroup | ITT | ADTTE |
| F-4 | Waterfall plot of the best percent change in tumour size | EE | ADTR, ADRS |
| F-5 | Spider plot of percent change in tumour size over time | EE | ADTR |
| F-6 | Swimmer plot of treatment duration and responses | SAF | ADSL, ADRS |
| F-7 | Laboratory values over time (mean and SE) | SAF | ADLB |
| L-1 | Listing of deaths | ITT | ADSL, ADAE |
