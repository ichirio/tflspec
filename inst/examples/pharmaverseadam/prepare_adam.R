# Data preparation for the pharmaverseadam example -------------------------
#
# tflspec writes the figure part of each program (Step1 selection, Step2
# ggplot2, Step3 ggsave). What it does NOT do is derive analysis variables
# that are missing from the data. For pharmaverseadam that means:
#
#   1. ADSL has no numeric treatment code  -> TRT01PN / TRT01AN (arm order)
#   2. ADTTE / ADRS / ADTR carry no treatment or population flags
#      -> add TRT01P, TRT01A, SAFFL, SEX, AGEGR1 from ADSL
#   3. ADRS has no ADY                       -> ADY = ADT - TRTSDT + 1
#   4. ADTR has no one-row-per-subject best % change for the waterfall
#      -> PARAMCD "BPCHG" = minimum post-baseline PCHG of SDIAM
#   5. Only a few subjects have tumour assessments
#      -> ADSL flag RSFL = "Y" for them (population of the swimmer plot)
#
# The result is a named list `adam`; the generated programs read from it
# through options(tflspec.data_expr = "adam${DS}").

library(dplyr)
library(pharmaverseadam)

arm_order <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

adsl_x <- adsl |>
  mutate(
    TRT01PN = match(TRT01P, arm_order),
    TRT01AN = match(TRT01A, arm_order),
    RSFL    = ifelse(USUBJID %in% adrs_onco$USUBJID[adrs_onco$PARAMCD == "OVR"], "Y", "N")
  )

# ADSL variables copied into the BDS datasets that lack them
adsl_vars <- adsl_x |>
  select(USUBJID, TRT01P, TRT01PN, TRT01A, TRT01AN, SAFFL, SEX, AGEGR1)
add_adsl <- function(d) {
  d |>
    select(-any_of(setdiff(names(adsl_vars), "USUBJID"))) |>
    left_join(adsl_vars, by = "USUBJID")
}

adrs_x <- adrs_onco |>
  mutate(ADY = as.numeric(ADT - TRTSDT) + 1) |>
  add_adsl()

adtr_best <- adtr_onco |>
  filter(PARAMCD == "SDIAM", ADY > 1, !is.na(PCHG)) |>
  group_by(USUBJID) |>
  summarise(AVAL = min(PCHG), .groups = "drop") |>
  mutate(PARAMCD = "BPCHG", PARAM = "Best Percent Change from Baseline in Sum of Diameters")

adtr_x <- bind_rows(adtr_onco, adtr_best) |>
  add_adsl()

adam <- list(
  ADSL  = adsl_x,
  ADTTE = add_adsl(adtte_onco),
  ADRS  = adrs_x,
  ADTR  = adtr_x,
  ADAE  = adae,
  ADLB  = adlb,
  ADPC  = adpc
)
