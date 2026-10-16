# Study XYZ-ONC-01: the analysis datasets ------------------------------------
#
# A study's ADaM come from its own ADaM programs; here they are tflspec's
# sample datasets (see ?adsl, and SAP.md in
# system.file("sample", "onco", package = "tflspec")), saved where the
# specs say: data/adam/<dataset>.rds.  (`tflspec::` in full: other
# packages have datasets of these names too -- ggsurvfit's adtte.)
#
# A few figure variables are not in the ADaM and are derived here, as a
# study would in an analysis-data step:
#
#   ADSL  DTHADY  study day of death (the swimmer plot's death marker)
#         RSPFL   "Y" for a confirmed responder (the swimmer plot's subjects)
#         ARMCMP  the planned arm with Control first: the forest plot's
#                 hazard ratio is of Drug X against this first level
#   ADTR  BPCHG   one record a subject: the best (lowest) percent change
#                 from baseline in the sum of diameters (the waterfall)

bor <- tflspec::adrs[tflspec::adrs$PARAMCD == "BOR", ]

adsl_x <- tflspec::adsl
adsl_x$DTHADY <- as.numeric(adsl_x$DTHDT - adsl_x$TRTSDT) + 1
adsl_x$RSPFL <- ifelse(adsl_x$USUBJID %in% bor$USUBJID[bor$AVALC %in% c("CR", "PR")], "Y", "N")
adsl_x$ARMCMP <- factor(adsl_x$TRT01P, levels = rev(levels(adsl_x$TRT01P)))

adtr <- tflspec::adtr
post <- adtr[adtr$PARAMCD == "SUMDIAM" & adtr$ABLFL != "Y" & !is.na(adtr$PCHG), ]
best <- post[order(post$USUBJID, post$PCHG), ]
best <- best[!duplicated(best$USUBJID), ]
best$PARAMCD <- "BPCHG"
best$PARAM <- "Best Percent Change from Baseline in Sum of Diameters (%)"
best$AVAL <- best$PCHG
adtr_x <- rbind(adtr, best)

dir.create(file.path("data", "adam"), recursive = TRUE, showWarnings = FALSE)
adam <- list(adsl = adsl_x, adae = tflspec::adae, adlb = tflspec::adlb,
             adrs = tflspec::adrs, adtr = adtr_x, adtte = tflspec::adtte)
for (ds in names(adam)) saveRDS(adam[[ds]], file.path("data", "adam", paste0(ds, ".rds")))
