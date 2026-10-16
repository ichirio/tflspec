#' Oncology sample study XYZ-ONC-01: subject level (ADSL)
#'
#' One of the eight ADaM datasets of a simulated oncology study: Drug X vs
#' Control, randomised 1:1 (stratified by baseline ECOG and region), RECIST
#' 1.1 every 6 weeks, data cut-off 30 June 2026; 200 subjects screened, 160
#' randomised.  The datasets
#' agree with one another: they are made from one set of simulated subject
#' histories, so a confirmed CR/PR has its tumour sizes at least 30% below
#' baseline, nothing is assessed after progression, the PFS and OS events
#' are the progressions and the deaths, and doses stop at progression,
#' toxicity, death or the cut-off.  `TRT01P`, `TRT01A` and `ARM` are
#' factors with Drug X first.
#'
#' ADSL holds every screened subject: the 160 randomised (`ENRLFL`, `ITTFL`)
#' and 40 screen failures, who have no arm (`ARM`, `TRT01P` ... `NA`,
#' `ARMNRS` "SCREEN FAILURE", as SDTMIG 3.3 has it) and no records in the
#' other datasets.  An analysis of the randomised subjects selects them
#' (`ITTFL == "Y"`, or `SAFFL` / `EFFFL`).
#'
#' The study's mini SAP (populations, endpoints, a display list) and CRF
#' outline are installed with the package:
#' `system.file("sample", "onco", "SAP.md", package = "tflspec")` and
#' `"CRF.md"`.  Variables carry CDISC labels (`attr(x, "label")`).
#'
#' @format `adsl`: a data frame, one row per screened subject (200 rows), with
#'   \describe{
#'     \item{STUDYID, USUBJID, SUBJID, SITEID}{identifiers}
#'     \item{COUNTRY, REGION1}{country; region (North America, Europe, Asia)}
#'     \item{AGE, AGEGR1, SEX, RACE, ETHNIC}{demographics (`AGEGR1`: `<65`,
#'       `>=65`)}
#'     \item{ECOGBL, STRATA1}{baseline ECOG (0, 1; 2 in some screen
#'       failures); randomisation stratum}
#'     \item{ARM, TRT01P, TRT01A}{planned / actual treatment (factors, Drug X
#'       first; `TRT01A` is `NA` for the subjects never treated, all three for
#'       the screen failures)}
#'     \item{ARMCD, ACTARM, ACTARMCD, ARMNRS}{arm codes; why an arm is null
#'       (`SCREEN FAILURE`, `ASSIGNED, NOT TREATED`)}
#'     \item{TRT01PN, TRT01AN}{their numeric codes (1 Drug X, 2 Control)}
#'     \item{RFICDT, RANDDT, TRTSDT, TRTEDT, TRTDURD}{informed consent,
#'       randomisation, first and last dose dates, treatment duration (days)}
#'     \item{ENRLFL, ITTFL, SAFFL, EFFFL}{enrolled (randomised),
#'       intent-to-treat, safety and efficacy evaluable population flags}
#'     \item{EOTSTT, DCTREAS}{end of treatment status and reason}
#'     \item{EOSSTT, EOSDT, DCSREAS, DCSREASP}{end of study status
#'       (`ONGOING` in follow-up at the cut-off, `DISCONTINUED`), its date,
#'       reason (`DEATH`, `SCREEN FAILURE`) and the screen failure's reason}
#'     \item{DTHFL, DTHDT, DTHCAUS, LSTALVDT, DCUTDT}{death; date last known
#'       alive; data cut-off date}
#'     \item{BSLD}{baseline sum of diameters of target lesions (mm)}
#'   }
#' @source Simulated by `data-raw/gen_onco.R` in the package's source,
#'   seed 20261016; not real patient data.
#' @seealso The other datasets of the study: [adex], [adae], [adlb], [advs],
#'   [adrs], [adtr], [adtte]; the study's SAP and CRF outline,
#'   `system.file("sample", "onco", package = "tflspec")`.
#' @family oncology sample data
#' @examples
#' table(adsl$ARMNRS, useNA = "ifany")
#' itt <- adsl[adsl$ITTFL == "Y", ]
#' table(itt$TRT01P, itt$DTHFL)
"adsl"

#' Oncology sample study XYZ-ONC-01: exposure (ADEX)
#'
#' Study drug exposure of the simulated study XYZ-ONC-01 (see [adsl]): one
#' dose every 21 days from the first to the last dose; a reduced dose
#' follows a related grade >= 3 adverse event (`AEACN = "DOSE REDUCED"` in
#' [adae]).
#'
#' @format `adex`: a data frame, one row per dose and four summary rows per
#'   treated subject, with the subject keys (`STUDYID`, `USUBJID`, `TRT01P`,
#'   `TRT01PN`, `TRT01A`, `TRT01AN`, `SAFFL`) and
#'   \describe{
#'     \item{PARAMCD, PARAM}{`DOSE` (each administration, mg), `TRTDURD`
#'       (days), `NCYCLE`, `CUMDOSE` (mg), `RDI` (relative dose intensity, %)}
#'     \item{AVISIT, AVISITN}{cycle (`OVERALL` for the summaries)}
#'     \item{AVAL, AVALU}{value and unit}
#'     \item{EXPLDOS, EXDOSRFL}{planned dose; dose reduced flag}
#'     \item{ASTDT, AENDT}{dates}
#'   }
#' @inherit adsl source
#' @family oncology sample data
"adex"

#' Oncology sample study XYZ-ONC-01: adverse events (ADAE)
#'
#' Adverse events of the simulated study XYZ-ONC-01 (see [adsl]), coded
#' with a small MedDRA-like dictionary (31 preferred terms in 12 system
#' organ classes) and graded by CTCAE.  Treatment-emergent events start
#' from the first dose to 30 days after the last; a few start between
#' randomisation and the first dose (`TRTEMFL = "N"`).  A death before
#' progression on treatment is a grade 5 event on the date of death.
#'
#' @format `adae`: a data frame, one row per event, with the subject keys
#'   (as in [adex]) and
#'   \describe{
#'     \item{AESEQ, AETERM, AEDECOD, AEBODSYS, AESOC}{the event and its coding}
#'     \item{AETOXGR, ATOXGR, AESEV}{CTCAE grade (1-5); severity}
#'     \item{AESER, AEREL, AEACN, AEOUT, AESDTH}{serious, causality, action
#'       with the study drug, outcome, results in death}
#'     \item{ASTDT, AENDT, ASTDY, AENDY}{start / end dates and study days}
#'     \item{TRTEMFL}{treatment-emergent flag}
#'   }
#' @inherit adsl source
#' @family oncology sample data
"adae"

#' Oncology sample study XYZ-ONC-01: laboratory (ADLB)
#'
#' Six laboratory parameters of the simulated study XYZ-ONC-01 (see
#' [adsl]) on day 1 of each cycle (cycle 1 is the baseline): ALT, AST,
#' bilirubin, haemoglobin, neutrophils, platelets, with normal ranges and
#' CTCAE grades for shift tables.
#'
#' @format `adlb`: a data frame, one row per subject, parameter and visit,
#'   with the subject keys (as in [adex]) and
#'   \describe{
#'     \item{PARAMCD, PARAM, PARCAT1}{`ALT`, `AST`, `BILI`, `HGB`, `NEUT`,
#'       `PLAT`; chemistry or haematology}
#'     \item{AVISIT, AVISITN, ADT, ADY}{visit, date, study day}
#'     \item{AVAL, BASE, CHG, PCHG}{value, baseline, change, percent change}
#'     \item{ANRLO, ANRHI, ANRIND, BNRIND}{normal range and indicator (and at
#'       baseline)}
#'     \item{ATOXGR, BTOXGR}{CTCAE grade (and at baseline)}
#'     \item{ABLFL, ONTRTFL}{baseline and on-treatment record flags}
#'   }
#' @inherit adsl source
#' @family oncology sample data
"adlb"

#' Oncology sample study XYZ-ONC-01: vital signs (ADVS)
#'
#' Weight, blood pressure and pulse of the simulated study XYZ-ONC-01 (see
#' [adsl]) on day 1 of each cycle.
#'
#' @format `advs`: a data frame, one row per subject, parameter and visit,
#'   with the subject keys (as in [adex]) and `PARAMCD` (`WEIGHT`, `SYSBP`,
#'   `DIABP`, `PULSE`), `PARAM`, `AVISIT`, `AVISITN`, `ADT`, `ADY`, `AVAL`,
#'   `BASE`, `CHG`, `ABLFL`, `ONTRTFL`.
#' @inherit adsl source
#' @family oncology sample data
"advs"

#' Oncology sample study XYZ-ONC-01: tumour response (ADRS)
#'
#' RECIST 1.1 responses of the simulated study XYZ-ONC-01 (see [adsl]):
#' the investigator's overall response at each assessment (`OVR`: CR when
#' the sum of diameters is 0 mm, PR at least 30% below baseline, PD at
#' least 20% and 5 mm above the nadir or a new lesion, else SD) and the
#' best confirmed overall response (`BOR`: CR / PR confirmed by the next
#' assessment; NE without a post-baseline assessment), one per subject.
#'
#' @format `adrs`: a data frame with the subject keys (`STUDYID`, `USUBJID`,
#'   `TRT01P`, `TRT01PN`, `TRT01A`, `TRT01AN`, `ITTFL`, `SAFFL`, `EFFFL`) and
#'   \describe{
#'     \item{PARAMCD, PARAM}{`OVR` or `BOR`}
#'     \item{AVISIT, AVISITN, ADT, ADY}{visit (`WEEK 6`, ...; `OVERALL` for
#'       BOR), date, study day}
#'     \item{AVALC, AVAL}{response (CR, PR, SD, PD, NE) and its code 1-5}
#'     \item{ANL01FL}{analysis flag}
#'   }
#' @inherit adsl source
#' @family oncology sample data
"adrs"

#' Oncology sample study XYZ-ONC-01: tumour sizes (ADTR)
#'
#' The sum of diameters of target lesions of the simulated study
#' XYZ-ONC-01 (see [adsl]) at baseline (at randomisation) and at each
#' assessment, every 6 weeks until progression, death or the cut-off: the
#' data of a waterfall or spider plot.
#'
#' @format `adtr`: a data frame with the subject keys (as in [adrs]) and
#'   \describe{
#'     \item{PARAMCD, PARAM}{`SUMDIAM`}
#'     \item{AVISIT, AVISITN, ADT, ADY}{visit, date, study day}
#'     \item{AVAL, BASE, CHG, PCHG}{sum of diameters (mm), baseline, change,
#'       percent change}
#'     \item{NADIR}{the smallest sum before this assessment}
#'     \item{ABLFL, ANL01FL}{baseline and analysis flags}
#'   }
#' @inherit adsl source
#' @family oncology sample data
"adtr"

#' Oncology sample study XYZ-ONC-01: time to event (ADTTE)
#'
#' Progression-free survival, overall survival and duration of response of
#' the simulated study XYZ-ONC-01 (see [adsl]), in months; and a safety
#' endpoint, the time to the first treatment-emergent skin and subcutaneous
#' tissue disorder, in days.  PFS: to progression or death, censored at the
#' last tumour assessment; OS: to death, censored at the cut-off; DOR: from
#' the first confirmed CR / PR; TTSKIN: the safety set, from the first dose,
#' censored at the end of the treatment-emergent period (30 days after the
#' last dose, death or the cut-off).
#'
#' @format `adtte`: a data frame, one row per subject and parameter, with
#'   the subject keys (as in [adrs]), the subgroups of a forest plot
#'   (`AGEGR1`, `SEX`, `RACE`, `ECOGBL`, `REGION1`, `STRATA1`) and
#'   \describe{
#'     \item{PARAMCD, PARAM, PARAMN}{`PFS`, `OS`, `DOR`, `TTSKIN`}
#'     \item{STARTDT, ADT, ADY}{origin and event / censoring dates}
#'     \item{AVAL, AVALU}{time (`MONTHS`; `DAYS` for `TTSKIN`)}
#'     \item{CNSR, EVNTDESC, CNSDTDSC}{censor (1) or event (0) and why}
#'   }
#' @inherit adsl source
#' @family oncology sample data
#' @examples
#' pfs <- adtte[adtte$PARAMCD == "PFS", ]
#' tapply(pfs$CNSR == 0, pfs$TRT01P, sum)
"adtte"
