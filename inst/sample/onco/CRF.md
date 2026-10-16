# Study XYZ-ONC-01: CRF outline (sample)

Only what the sample's ADaM data need (see `SAP.md`; the data, `adsl` ...
`adtte` in tflspec, are simulated by `data-raw/gen_onco.R`, not collected).
One line per collected item: the CRF page, the item, and where it ends up
in the ADaM data.

| CRF page | Item | Values | ADaM |
|---|---|---|---|
| Enrolment | Site, subject number | 101-104, 201-205, 301-303; 0001- | ADSL `SITEID`, `SUBJID`, `USUBJID` |
| Demographics | Age, sex, race, country | years; M/F; CDISC race; ISO 3166 alpha-3 | `AGE`, `SEX`, `RACE`, `COUNTRY` (`REGION1`, `AGEGR1` derived) |
| Baseline | ECOG performance status | 0, 1 | `ECOGBL` |
| Randomisation (IWRS) | Date, arm, stratum | Drug X / Control; ECOG x region | `RANDDT`, `ARM`, `TRT01P`, `STRATA1` |
| Study drug | Date, dose given, planned dose, reason for reduction | mg, every 21 days | ADEX `DOSE` (`ASTDT`, `AVAL`, `EXPLDOS`, `EXDOSRFL`); ADSL `TRTSDT`, `TRTEDT` |
| End of treatment | Status, reason | progressive disease, adverse event, death; ongoing | ADSL `EOTSTT`, `DCTREAS` |
| Tumour assessment (target lesions) | Date, sum of diameters | mm, at baseline and every 6 weeks | ADTR `SUMDIAM` |
| Tumour assessment (new lesions) | New lesion | yes / no | (makes the OVR PD) |
| Overall response | Investigator's response per visit | CR, PR, SD, PD, NE | ADRS `OVR` (`BOR` derived) |
| Adverse events | Term, start / end date, CTCAE grade, serious, related, action with study drug, outcome | MedDRA-like PT/SOC; grade 1-5 | ADAE `AETERM`, `AEDECOD`, `AEBODSYS`, `ASTDT`, `AENDT`, `AETOXGR`, `AESER`, `AEREL`, `AEACN`, `AEOUT` |
| Laboratory | Date, ALT, AST, bilirubin, haemoglobin, neutrophils, platelets | units in `PARAM`; on day 1 of each cycle | ADLB (`ANRIND`, `ATOXGR` derived) |
| Vital signs | Date, weight, blood pressure, pulse | kg, mmHg, beats/min; day 1 of each cycle | ADVS |
| Death | Date, cause | progressive disease, adverse event, other | ADSL `DTHDT`, `DTHCAUS`, `DTHFL` |
| Survival follow-up | Date last known alive | up to the cut-off | ADSL `LSTALVDT` |
