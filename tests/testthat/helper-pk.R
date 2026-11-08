# A fictional ADPC for the template pk_conc (test-tpl-pk.R, and the study
# that builds every template, test-template-to-spec-run.R)

pk_times <- c("Pre-Dose", "0.5 hr", "1 hr", "2 hr", "4 hr", "8 hr", "24 hr")

# A fictional ADPC: the safety subjects of two arms, one analyte, a sample
# at each time point; Pre-Dose and some late samples BLQ (AVAL 0 before the
# peak, missing after it)
pk_adpc <- function(adsl) {
  sl <- adsl[adsl$SAFFL == "Y", ]
  sl <- sl[sl$TRT01A %in% unique(sl$TRT01A)[1:2], ][1:12, ]
  tn <- c(0, 0.5, 1, 2, 4, 8, 24)
  set.seed(565)
  d <- do.call(rbind, lapply(seq_len(nrow(sl)), function(i) {
    v <- round(100 * (exp(-0.2 * tn) - exp(-1.5 * tn)) * exp(stats::rnorm(7, 0, 0.2)), 3)
    blq <- tn == 0 | (tn == 24 & i %% 3 == 0)
    v[blq] <- ifelse(tn[blq] == 0, 0, NA)
    data.frame(STUDYID = sl$STUDYID[i], USUBJID = sl$USUBJID[i], AGE = sl$AGE[i], SEX = sl$SEX[i],
               TRT01A = sl$TRT01A[i], PARAMCD = "DRUGX", PARAM = "Drug X (ng/mL)",
               ATPT = pk_times, ATPTN = tn, AVAL = v, BLQFL = ifelse(blq, "Y", ""),
               stringsAsFactors = FALSE)
  }))
  # (the records in another order than the time points')
  d[order(d$USUBJID, d$ATPT), ]
}
