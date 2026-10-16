# Oncology ADaM sample data for study XYZ-ONC-01 (see SAP.md and CRF.md).
#
#   source(system.file("sample", "onco", "gen_onco.R", package = "tflspec"))
#   onco <- gen_onco()            # a named list: ADSL, ADEX, ADAE, ...
#
# Drug X vs Control, randomised 1:1 (stratified by ECOG and region),
# RECIST 1.1 every 6 weeks, a fixed data cut-off.  Every dataset is made
# from ONE set of latent subject histories (tumour course, new lesion,
# toxicity, death before / after progression), so the datasets agree: the
# best response follows the tumour sizes, there is no assessment after
# progression, the PFS and OS events are the progressions and the deaths,
# doses stop at progression, toxicity, death or the cut-off, and dose
# reductions follow grade >= 3 adverse events.  Base R only; seeded (the
# caller's random number state is left as it was).

gen_onco <- function(n = 160, seed = 20261016, dco = as.Date("2026-06-30")) {
  rng <- ".Random.seed"
  if (exists(rng, envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(rng, envir = globalenv(), inherits = FALSE)
    on.exit(assign(rng, old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(rm(list = rng, envir = globalenv()), add = TRUE)
  }
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  dco <- as.Date(dco)
  study <- "XYZ-ONC-01"
  arms <- c("Drug X", "Control")

  # ---- helpers -------------------------------------------------------------
  pick <- function(x, size, prob = NULL) x[sample.int(length(x), size, TRUE, prob)]
  rbind_all <- function(x) {
    x <- Filter(Negate(is.null), x)
    out <- do.call(rbind, x)
    rownames(out) <- NULL
    out
  }
  study_day <- function(dt, ref) {
    d <- as.numeric(dt - ref)
    ifelse(is.na(d), NA_real_, d + (d >= 0))
  }
  months <- function(from, to) round(as.numeric(to - from + 1) / 30.4375, 2)
  na_date <- as.Date(NA)

  # ---- ADSL: subjects, randomisation ---------------------------------------
  region_lv <- c("NORTH AMERICA", "EUROPE", "ASIA")
  region <- pick(region_lv, n, c(0.3, 0.4, 0.3))
  sites <- list("NORTH AMERICA" = 101:104, "EUROPE" = 201:205, "ASIA" = 301:303)
  country <- list("NORTH AMERICA" = c("USA", "CAN"), "EUROPE" = c("DEU", "FRA", "GBR", "ESP"),
                  "ASIA" = c("JPN", "KOR"))
  race_lv <- c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER")
  race_p <- list("NORTH AMERICA" = c(0.70, 0.20, 0.05, 0.05), "EUROPE" = c(0.90, 0.05, 0.02, 0.03),
                 "ASIA" = c(0, 0, 0.97, 0.03))
  siteid <- vapply(region, function(r) as.character(pick(sites[[r]], 1)), "")
  cntry <- vapply(region, function(r) pick(country[[r]], 1), "")
  race <- vapply(region, function(r) pick(race_lv, 1, race_p[[r]]), "")
  age <- round(pmin(pmax(rnorm(n, 62, 10), 28), 85))
  sex <- pick(c("M", "F"), n, c(0.55, 0.45))
  ecog <- pick(0:1, n, c(0.45, 0.55))
  randdt <- as.Date("2024-07-01") + sort(sample.int(366, n, TRUE) - 1)
  # permuted blocks of 4 within each stratum (ECOG x region)
  stratum <- paste(ecog, region)
  arm <- character(n)
  for (s in sort(unique(stratum))) {
    idx <- which(stratum == s)
    blocks <- unlist(lapply(seq_len(ceiling(length(idx) / 4)), function(j) sample(rep(arms, 2))))
    arm[idx] <- blocks[seq_along(idx)]
  }
  drug <- arm == "Drug X"
  treated <- rep(TRUE, n)
  treated[sample.int(n, round(n * 0.0125))] <- FALSE
  trtsdt <- randdt + sample(0:4, n, TRUE)
  trtsdt[!treated] <- na_date
  ref <- ifelse(treated, trtsdt, randdt)   # day 1 of each subject's history
  ref <- as.Date(ref, origin = "1970-01-01")
  cut_day <- as.numeric(dco - ref) + 1

  # ---- latent histories ----------------------------------------------------
  # tumour: Stein model SLD(t) = b (exp(-d t) + exp(g t) - 1); new lesions,
  # stops for toxicity, death before / after progression (days from day 1)
  poor <- ifelse(ecog == 1, 1.35, 1) * ifelse(age >= 65, 1.1, 1)
  base <- round(exp(runif(n, log(25), log(140))))
  shrink <- rlnorm(n, log(ifelse(drug, 0.0062, 0.0050)), 0.9)
  growth <- rlnorm(n, log(ifelse(drug, 0.0014, 0.0030) * poor), 0.55)
  newles <- ceiling(rexp(n, ifelse(drug, 1 / 1100, 1 / 600) * poor))
  tox <- ceiling(rexp(n, ifelse(drug, 1 / 1400, 1 / 1100)))
  death_pre <- ceiling(rexp(n, 1 / 5500 * poor^2))
  death_post <- ceiling(rexp(n, 1 / 640 * poor))

  # ---- tumour assessments every 6 weeks until progression, death or cut-off
  ass <- vector("list", n)
  hist <- data.frame(pd_day = rep(NA_real_, n), death_day = NA_real_, last_dose = NA_real_,
                     stop = NA_character_)
  for (i in seq_len(n)) {
    resp <- character()
    days <- numeric()
    sld <- numeric()
    pd_day <- NA_real_
    if (treated[i]) {
      k_max <- floor((cut_day[i] + 3) / 42)
      sched <- seq_len(max(k_max, 0))
      days <- 42 * sched + 1 + sample(-3:3, length(sched), TRUE)
      sched <- sched[days <= cut_day[i]]
      days <- days[days <= cut_day[i]]
      tt <- days - 1
      sld <- base[i] * (exp(-shrink[i] * tt) + exp(growth[i] * tt) - 1)
      sld <- round(pmax(sld, 0))
      sld[sld < 10] <- 0
      nadir <- base[i]
      for (k in seq_along(days)) {
        if (days[k] >= death_pre[i]) break
        rise <- sld[k] - nadir
        pd <- (rise >= 0.2 * nadir && rise >= 5) || days[k] >= newles[i]
        r <- if (pd) "PD" else if (sld[k] == 0) "CR" else if (sld[k] <= 0.7 * base[i]) "PR" else "SD"
        resp[k] <- r
        nadir <- min(nadir, sld[k])
        if (pd) {
          pd_day <- days[k]
          break
        }
      }
      m <- length(resp)
      if (m) {
        ass[[i]] <- data.frame(i = i, day = days[seq_len(m)], visit = sched[seq_len(m)],
                               sld = sld[seq_len(m)], newles = days[seq_len(m)] >= newles[i],
                               ovr = resp)
      }
    }
    death <- if (is.na(pd_day)) death_pre[i] else pd_day + death_post[i]
    if (death > cut_day[i]) death <- NA_real_
    hist$pd_day[i] <- pd_day
    hist$death_day[i] <- death
    if (treated[i]) {
      ends <- c(pd_day, tox[i], death, cut_day[i] + 1)
      why <- c("PROGRESSIVE DISEASE", "ADVERSE EVENT", "DEATH", "ONGOING")
      j <- which.min(ifelse(is.na(ends), Inf, ends))
      doses <- seq(1, by = 21, length.out = ceiling(ends[j] / 21))
      doses <- doses[doses < ends[j]]
      hist$last_dose[i] <- max(c(1, doses))
      hist$stop[i] <- why[j]
    }
  }
  ass <- rbind_all(ass)
  ass$date <- ref[ass$i] + ass$day - 1

  # ---- dates per subject ---------------------------------------------------
  dthdt <- ref + hist$death_day - 1
  trtedt <- ref + hist$last_dose - 1
  pddt <- ref + hist$pd_day - 1
  dthfl <- ifelse(is.na(dthdt), "N", "Y")
  teae_end <- pmin(trtedt + 30, dthdt, dco, na.rm = TRUE)

  # ---- ADAE ----------------------------------------------------------------
  # a small MedDRA-like dictionary: incidence per arm (for ~4 months of
  # treatment) and grade 1-4 distribution
  dict <- data.frame(
    soc = c(rep("GASTROINTESTINAL DISORDERS", 5),
            rep("GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", 3),
            rep("SKIN AND SUBCUTANEOUS TISSUE DISORDERS", 3),
            rep("METABOLISM AND NUTRITION DISORDERS", 2),
            rep("BLOOD AND LYMPHATIC SYSTEM DISORDERS", 3),
            rep("INVESTIGATIONS", 3),
            rep("NERVOUS SYSTEM DISORDERS", 3),
            rep("RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS", 3),
            rep("INFECTIONS AND INFESTATIONS", 2),
            rep("MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS", 2),
            "VASCULAR DISORDERS", "PSYCHIATRIC DISORDERS"),
    pt = c("Nausea", "Diarrhoea", "Vomiting", "Constipation", "Stomatitis",
           "Fatigue", "Pyrexia", "Oedema peripheral",
           "Rash", "Pruritus", "Alopecia",
           "Decreased appetite", "Hypokalaemia",
           "Anaemia", "Neutropenia", "Thrombocytopenia",
           "Alanine aminotransferase increased", "Aspartate aminotransferase increased", "Weight decreased",
           "Headache", "Dysgeusia", "Peripheral sensory neuropathy",
           "Cough", "Dyspnoea", "Pneumonitis",
           "Pneumonia", "Urinary tract infection",
           "Arthralgia", "Back pain",
           "Hypertension", "Insomnia"),
    p_x = c(.30, .35, .15, .12, .14, .35, .12, .08, .28, .16, .04, .22, .07, .15, .09, .05,
            .13, .11, .07, .11, .09, .04, .11, .08, .05, .05, .05, .09, .07, .11, .07),
    p_c = c(.28, .18, .14, .18, .07, .32, .10, .07, .07, .05, .22, .20, .05, .27, .25, .14,
            .05, .05, .07, .09, .05, .16, .09, .10, .01, .06, .05, .07, .07, .04, .07),
    severe = c(.08, .12, .08, .03, .10, .10, .05, .03, .12, .03, 0, .05, .15, .20, .45, .30,
               .25, .20, .03, .03, 0, .10, .02, .15, .30, .60, .25, .03, .05, .20, .02)
  )
  grade_of <- function(sev) {
    if (runif(1) < sev) pick(3:4, 1, c(0.8, 0.2)) else pick(1:2, 1, c(0.65, 0.35))
  }
  ae <- vector("list", n)
  for (i in which(treated)) {
    exposure <- hist$last_dose[i] + 20
    window <- as.numeric(teae_end[i] - trtsdt[i]) + 1
    p <- if (drug[i]) dict$p_x else dict$p_c
    p <- 1 - (1 - p)^(min(exposure, 360) / 120)
    hit <- which(runif(nrow(dict)) < p)
    rows <- lapply(hit, function(k) {
      day <- max(1, ceiling(window * rbeta(1, 0.8, 2)))
      data.frame(i = i, k = k, day = day, grade = grade_of(dict$severe[k]),
                 rel = if (runif(1) < 0.6) "Y" else "N", acn = NA_character_, fatal = FALSE)
    })
    if (hist$stop[i] == "ADVERSE EVENT") {
      k <- pick(which(dict$severe >= 0.1), 1)
      rows[[length(rows) + 1]] <- data.frame(
        i = i, k = k, day = tox[i], grade = pick(3:4, 1, c(0.75, 0.25)), rel = "Y",
        acn = "DRUG WITHDRAWN", fatal = FALSE)
    }
    if (dthfl[i] == "Y" && is.na(hist$pd_day[i]) && dthdt[i] <= trtedt[i] + 30) {
      k <- pick(match(c("Pneumonia", "Pneumonitis", "Dyspnoea"), dict$pt), 1)
      rows[[length(rows) + 1]] <- data.frame(
        i = i, k = k, day = hist$death_day[i], grade = 5, rel = "N", acn = NA_character_, fatal = TRUE)
    }
    # a few events between randomisation and the first dose (not emergent)
    gap <- as.numeric(trtsdt[i] - randdt[i])
    if (gap > 0 && runif(1) < 0.15) {
      rows[[length(rows) + 1]] <- data.frame(
        i = i, k = pick(match(c("Headache", "Back pain", "Insomnia", "Cough"), dict$pt), 1),
        day = 1 - sample.int(gap, 1), grade = 1, rel = "N", acn = "NOT APPLICABLE", fatal = FALSE)
    }
    ae[[i]] <- rbind_all(rows)
  }
  ae <- rbind_all(ae)
  ae <- ae[order(ae$i, ae$day, ae$k), ]
  ae$astdt <- trtsdt[ae$i] + ae$day - 1
  ae$teae <- ae$astdt >= trtsdt[ae$i]
  dur <- ceiling(rexp(nrow(ae), 1 / 14))
  ae$aendt <- ae$astdt + dur
  end_cap <- pmin(dco, dthdt[ae$i], na.rm = TRUE)
  ongoing <- ae$aendt > end_cap
  ae$aendt[ongoing] <- na_date
  ae$aendt[ae$fatal] <- ae$astdt[ae$fatal]
  ae$out <- ifelse(ae$fatal, "FATAL", ifelse(ongoing, "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED"))
  # action with the study drug: a related grade >= 3 event during treatment
  # reduces the dose (at most twice); one after the last dose changes nothing
  on_trt <- ae$teae & ae$astdt <= trtedt[ae$i]
  ae$acn[is.na(ae$acn) & !on_trt] <- "NOT APPLICABLE"
  ae$acn[is.na(ae$acn) & ae$fatal] <- "NOT APPLICABLE"
  red <- is.na(ae$acn) & ae$grade >= 3 & ae$rel == "Y" & ae$astdt < trtedt[ae$i]
  red[red] <- ave(seq_along(ae$i[red]), ae$i[red], FUN = seq_along) <= 2
  ae$acn[red] <- "DOSE REDUCED"
  ae$acn[is.na(ae$acn)] <- "DOSE NOT CHANGED"
  ser <- ae$grade >= 4 | (ae$grade == 3 & runif(nrow(ae)) < 0.4)

  adae <- data.frame(
    STUDYID = study, USUBJID = NA_character_, ae_i = ae$i,
    AESEQ = ave(ae$i, ae$i, FUN = seq_along),
    AETERM = toupper(dict$pt[ae$k]), AEDECOD = dict$pt[ae$k], AEBODSYS = dict$soc[ae$k],
    AESOC = dict$soc[ae$k],
    AETOXGR = as.character(ae$grade), ATOXGR = as.character(ae$grade),
    AESEV = c("MILD", "MODERATE", "SEVERE", "SEVERE", "SEVERE")[ae$grade],
    AESER = ifelse(ser, "Y", "N"), AEREL = ifelse(ae$rel == "Y", "RELATED", "NOT RELATED"),
    AEACN = ae$acn, AEOUT = ae$out,
    ASTDT = ae$astdt, AENDT = ae$aendt,
    ASTDY = study_day(ae$astdt, trtsdt[ae$i]), AENDY = study_day(ae$aendt, trtsdt[ae$i]),
    TRTEMFL = ifelse(ae$teae, "Y", "N"), AESDTH = ifelse(ae$fatal, "Y", "N")
  )

  # ---- ADEX: one administration per 21-day cycle ---------------------------
  plan <- ifelse(drug, 200, 100)
  step <- ifelse(drug, 50, 25)
  ex <- vector("list", n)
  for (i in which(treated)) {
    day <- seq(1, hist$last_dose[i], by = 21)
    dt <- trtsdt[i] + day - 1
    red_dt <- sort(ae$astdt[ae$i == i & ae$acn == "DOSE REDUCED"])
    nred <- vapply(dt, function(d) sum(red_dt < d), 0)
    ex[[i]] <- data.frame(i = i, cycle = seq_along(day), date = dt,
                          dose = plan[i] - step[i] * nred, planned = plan[i])
  }
  ex <- rbind_all(ex)
  ex_sum <- data.frame(
    i = which(treated),
    ncyc = as.numeric(table(factor(ex$i, levels = which(treated)))),
    cum = as.numeric(tapply(ex$dose, factor(ex$i, levels = which(treated)), sum))
  )
  ex_sum$dur <- as.numeric(trtedt[ex_sum$i] - trtsdt[ex_sum$i]) + 1
  ex_sum$rdi <- round(100 * ex_sum$cum / (plan[ex_sum$i] * ex_sum$ncyc), 1)
  adex <- rbind(
    data.frame(ex_i = ex$i, PARAMCD = "DOSE", PARAM = "Dose Administered (mg)",
               AVISIT = paste("CYCLE", ex$cycle), AVISITN = ex$cycle, AVAL = ex$dose, AVALU = "mg",
               EXPLDOS = ex$planned, EXDOSRFL = ifelse(ex$dose < ex$planned, "Y", "N"),
               ASTDT = ex$date, AENDT = ex$date),
    data.frame(ex_i = rep(ex_sum$i, 4),
               PARAMCD = rep(c("TRTDURD", "NCYCLE", "CUMDOSE", "RDI"), each = nrow(ex_sum)),
               PARAM = rep(c("Treatment Duration (days)", "Number of Cycles", "Cumulative Dose (mg)",
                             "Relative Dose Intensity (%)"), each = nrow(ex_sum)),
               AVISIT = "OVERALL", AVISITN = NA_real_,
               AVAL = c(ex_sum$dur, ex_sum$ncyc, ex_sum$cum, ex_sum$rdi),
               AVALU = rep(c("DAYS", "CYCLES", "mg", "%"), each = nrow(ex_sum)),
               EXPLDOS = rep(plan[ex_sum$i], 4), EXDOSRFL = NA_character_,
               ASTDT = rep(trtsdt[ex_sum$i], 4), AENDT = rep(trtedt[ex_sum$i], 4))
  )

  # ---- ADLB / ADVS: at every cycle's day 1 (cycle 1 = baseline) ------------
  lb_par <- data.frame(
    PARAMCD = c("ALT", "AST", "BILI", "HGB", "NEUT", "PLAT"),
    PARAM = c("Alanine Aminotransferase (U/L)", "Aspartate Aminotransferase (U/L)",
              "Bilirubin (umol/L)", "Hemoglobin (g/L)", "Neutrophils (10^9/L)", "Platelets (10^9/L)"),
    PARCAT1 = c("CHEMISTRY", "CHEMISTRY", "CHEMISTRY", "HEMATOLOGY", "HEMATOLOGY", "HEMATOLOGY"),
    ANRLO = c(7, 8, 3, 120, 2.0, 150), ANRHI = c(40, 40, 21, 160, 7.5, 400),
    digits = c(0, 0, 1, 0, 2, 0)
  )
  ctc <- function(code, x, lo, hi) {
    g <- switch(code,
      ALT = , AST = cut(x / hi, c(-Inf, 1, 3, 5, 20, Inf), labels = FALSE, right = TRUE) - 1,
      BILI = cut(x / hi, c(-Inf, 1, 1.5, 3, 10, Inf), labels = FALSE, right = TRUE) - 1,
      HGB = 3 - cut(x, c(-Inf, 80, 100, lo, Inf), labels = FALSE, right = FALSE) + 1,
      NEUT = 4 - cut(x, c(-Inf, 0.5, 1.0, 1.5, lo, Inf), labels = FALSE, right = FALSE) + 1,
      PLAT = 4 - cut(x, c(-Inf, 25, 50, 75, lo, Inf), labels = FALSE, right = FALSE) + 1)
    as.character(g)
  }
  liver <- ifelse(drug, rexp(n, 1 / 0.5), rexp(n, 1 / 0.08))
  marrow <- ifelse(drug, rnorm(n, -0.12, 0.10), rnorm(n, -0.55, 0.25))
  hgb_drop <- ifelse(drug, rnorm(n, 6, 5), rnorm(n, 16, 7))
  b_lb <- cbind(ALT = rlnorm(n, log(22), 0.4), AST = rlnorm(n, log(24), 0.35),
                BILI = rlnorm(n, log(9), 0.4), HGB = rnorm(n, 132, 13),
                NEUT = rlnorm(n, log(4.2), 0.35), PLAT = rlnorm(n, log(260), 0.3))
  b_vs <- cbind(WEIGHT = rnorm(n, ifelse(sex == "M", 78, 64), 12), SYSBP = rnorm(n, 128, 14),
                DIABP = rnorm(n, 79, 9), PULSE = rnorm(n, 76, 10))
  lb <- vector("list", n)
  vs <- vector("list", n)
  for (i in which(treated)) {
    cyc <- ex$cycle[ex$i == i]
    dt <- ex$date[ex$i == i]
    m <- length(cyc)
    on <- c(0, rep(1, m - 1))
    ramp <- pmin(cyc - 1, 4) / 4
    v <- cbind(
      ALT = b_lb[i, "ALT"] * exp(on * liver[i] * runif(m, 0.3, 1.6) + rnorm(m, 0, 0.15)),
      AST = b_lb[i, "AST"] * exp(on * 0.8 * liver[i] * runif(m, 0.3, 1.6) + rnorm(m, 0, 0.12)),
      BILI = b_lb[i, "BILI"] * exp(on * 0.3 * liver[i] + rnorm(m, 0, 0.15)),
      HGB = b_lb[i, "HGB"] - hgb_drop[i] * ramp + rnorm(m, 0, 4),
      NEUT = b_lb[i, "NEUT"] * exp(on * (marrow[i] + rnorm(m, 0, 0.3))),
      PLAT = b_lb[i, "PLAT"] * exp(on * (0.6 * marrow[i] + rnorm(m, 0, 0.15)))
    )
    lb[[i]] <- data.frame(i = i, cycle = rep(cyc, 6), date = rep(dt, 6),
                          PARAMCD = rep(lb_par$PARAMCD, each = m), AVAL = as.vector(v))
    w <- cbind(
      WEIGHT = b_vs[i, "WEIGHT"] * (1 - on * ramp * ifelse(drug[i], 0.03, 0.04)) + rnorm(m, 0, 0.8),
      SYSBP = b_vs[i, "SYSBP"] + on * ifelse(drug[i], 6, 0) + rnorm(m, 0, 7),
      DIABP = b_vs[i, "DIABP"] + on * ifelse(drug[i], 3, 0) + rnorm(m, 0, 5),
      PULSE = b_vs[i, "PULSE"] + rnorm(m, 0, 6)
    )
    vs[[i]] <- data.frame(i = i, cycle = rep(cyc, 4), date = rep(dt, 4),
                          PARAMCD = rep(colnames(w), each = m), AVAL = as.vector(w))
  }
  lb <- rbind_all(lb)
  p <- match(lb$PARAMCD, lb_par$PARAMCD)
  lb$AVAL <- round(pmax(lb$AVAL, 0.01), lb_par$digits[p])
  lb$ANRLO <- lb_par$ANRLO[p]
  lb$ANRHI <- lb_par$ANRHI[p]
  lb$ANRIND <- ifelse(lb$AVAL < lb$ANRLO, "LOW", ifelse(lb$AVAL > lb$ANRHI, "HIGH", "NORMAL"))
  lb$ATOXGR <- NA_character_
  for (pc in lb_par$PARAMCD) {
    j <- lb$PARAMCD == pc
    lb$ATOXGR[j] <- ctc(pc, lb$AVAL[j], lb_par$ANRLO[lb_par$PARAMCD == pc], lb_par$ANRHI[lb_par$PARAMCD == pc])
  }
  bl <- lb$cycle == 1
  key <- paste(lb$i, lb$PARAMCD)
  b <- match(key, key[bl])
  adlb <- data.frame(
    lb_i = lb$i, PARAMCD = lb$PARAMCD, PARAM = lb_par$PARAM[p], PARCAT1 = lb_par$PARCAT1[p],
    AVISIT = ifelse(bl, "BASELINE", paste("CYCLE", lb$cycle, "DAY 1")), AVISITN = lb$cycle - 1,
    ADT = lb$date, AVAL = lb$AVAL, BASE = lb$AVAL[bl][b],
    CHG = NA_real_, PCHG = NA_real_,
    ANRLO = lb$ANRLO, ANRHI = lb$ANRHI, ANRIND = lb$ANRIND, BNRIND = lb$ANRIND[bl][b],
    ATOXGR = lb$ATOXGR, BTOXGR = lb$ATOXGR[bl][b],
    ABLFL = ifelse(bl, "Y", ""), ONTRTFL = ifelse(bl, "", "Y")
  )
  adlb$CHG <- ifelse(bl, NA_real_, round(adlb$AVAL - adlb$BASE, lb_par$digits[p]))
  adlb$PCHG <- ifelse(bl, NA_real_, round(100 * (adlb$AVAL - adlb$BASE) / adlb$BASE, 1))

  vs <- rbind_all(vs)
  vs_par <- c(WEIGHT = "Weight (kg)", SYSBP = "Systolic Blood Pressure (mmHg)",
              DIABP = "Diastolic Blood Pressure (mmHg)", PULSE = "Pulse Rate (beats/min)")
  vs$AVAL <- round(vs$AVAL, ifelse(vs$PARAMCD == "WEIGHT", 1, 0))
  bl <- vs$cycle == 1
  key <- paste(vs$i, vs$PARAMCD)
  b <- match(key, key[bl])
  advs <- data.frame(
    vs_i = vs$i, PARAMCD = vs$PARAMCD, PARAM = unname(vs_par[vs$PARAMCD]),
    AVISIT = ifelse(bl, "BASELINE", paste("CYCLE", vs$cycle, "DAY 1")), AVISITN = vs$cycle - 1,
    ADT = vs$date, AVAL = vs$AVAL, BASE = vs$AVAL[bl][b],
    CHG = ifelse(bl, NA_real_, round(vs$AVAL - vs$AVAL[bl][b], 1)),
    ABLFL = ifelse(bl, "Y", ""), ONTRTFL = ifelse(bl, "", "Y")
  )

  # ---- ADTR: sum of longest diameters; baseline scan at randomisation ------
  tr_bl <- data.frame(tr_i = seq_len(n), AVISIT = "BASELINE", AVISITN = 0, ADT = randdt,
                      AVAL = base, ABLFL = "Y")
  tr_po <- data.frame(tr_i = ass$i, AVISIT = paste("WEEK", 6 * ass$visit), AVISITN = 6 * ass$visit,
                      ADT = ass$date, AVAL = ass$sld, ABLFL = "")
  adtr <- rbind(tr_bl, tr_po)
  adtr <- adtr[order(adtr$tr_i, adtr$ADT), ]
  adtr$PARAMCD <- "SUMDIAM"
  adtr$PARAM <- "Sum of Diameters of Target Lesions (mm)"
  adtr$BASE <- base[adtr$tr_i]
  adtr$CHG <- ifelse(adtr$ABLFL == "Y", NA_real_, adtr$AVAL - adtr$BASE)
  adtr$PCHG <- round(100 * adtr$CHG / adtr$BASE, 1)
  adtr$NADIR <- ave(adtr$AVAL, adtr$tr_i, FUN = function(x) c(NA, cummin(x)[-length(x)]))
  adtr$ANL01FL <- ifelse(adtr$ABLFL == "Y", "", "Y")

  # ---- ADRS: overall response per visit, best confirmed overall response --
  code <- c(CR = 1, PR = 2, SD = 3, PD = 4, NE = 5)
  bor <- data.frame(i = seq_len(n), AVALC = "NE", ADT = na_date, rsp_start = na_date)
  for (i in unique(ass$i)) {
    a <- ass[ass$i == i, ]
    r <- a$ovr
    nx <- c(r[-1], NA)
    cr_ok <- r == "CR" & nx %in% "CR"
    pr_ok <- r %in% c("CR", "PR") & nx %in% c("CR", "PR")
    if (any(cr_ok)) {
      bor$AVALC[i] <- "CR"
      bor$ADT[i] <- a$date[which(cr_ok)[1]]
    } else if (any(pr_ok)) {
      bor$AVALC[i] <- "PR"
      bor$ADT[i] <- a$date[which(pr_ok)[1]]
    } else if (any(r %in% c("CR", "PR", "SD"))) {
      bor$AVALC[i] <- "SD"
      bor$ADT[i] <- a$date[which(r %in% c("CR", "PR", "SD"))[1]]
    } else {
      bor$AVALC[i] <- "PD"
      bor$ADT[i] <- a$date[1]
    }
    if (any(pr_ok)) bor$rsp_start[i] <- a$date[which(pr_ok)[1]]
  }
  adrs <- rbind(
    data.frame(rs_i = ass$i, PARAMCD = "OVR", PARAM = "Overall Response by Investigator",
               AVISIT = paste("WEEK", 6 * ass$visit), AVISITN = 6 * ass$visit,
               ADT = ass$date, AVALC = ass$ovr, AVAL = unname(code[ass$ovr])),
    data.frame(rs_i = bor$i, PARAMCD = "BOR", PARAM = "Best Confirmed Overall Response by Investigator",
               AVISIT = "OVERALL", AVISITN = NA_real_, ADT = bor$ADT, AVALC = bor$AVALC,
               AVAL = unname(code[bor$AVALC]))
  )
  adrs <- adrs[order(adrs$rs_i, adrs$PARAMCD == "BOR", adrs$ADT), ]
  adrs$ANL01FL <- "Y"

  # ---- ADTTE: PFS, OS, duration of response --------------------------------
  last_ass <- as.Date(tapply(ass$date, factor(ass$i, levels = seq_len(n)), max), origin = "1970-01-01")
  pfs_ev <- pmin(pddt, dthdt, na.rm = TRUE)
  pfs_cn <- ifelse(is.na(last_ass), randdt, last_ass)
  pfs_cn <- as.Date(pfs_cn, origin = "1970-01-01")
  pfs <- data.frame(
    tte_i = seq_len(n), PARAMCD = "PFS", PARAM = "Progression-Free Survival (months)", PARAMN = 1,
    STARTDT = randdt, ADT = ifelse(is.na(pfs_ev), pfs_cn, pfs_ev), CNSR = ifelse(is.na(pfs_ev), 1L, 0L),
    EVNTDESC = ifelse(is.na(pfs_ev), NA_character_,
                      ifelse(!is.na(pddt) & pddt == pfs_ev, "DISEASE PROGRESSION", "DEATH")),
    CNSDTDSC = ifelse(!is.na(pfs_ev), NA_character_,
                      ifelse(is.na(last_ass), "RANDOMIZATION", "LAST ADEQUATE TUMOR ASSESSMENT"))
  )
  lstalvdt <- ifelse(is.na(dthdt), dco, dthdt)
  lstalvdt <- as.Date(lstalvdt, origin = "1970-01-01")
  os <- data.frame(
    tte_i = seq_len(n), PARAMCD = "OS", PARAM = "Overall Survival (months)", PARAMN = 2,
    STARTDT = randdt, ADT = lstalvdt, CNSR = ifelse(is.na(dthdt), 1L, 0L),
    EVNTDESC = ifelse(is.na(dthdt), NA_character_, "DEATH"),
    CNSDTDSC = ifelse(is.na(dthdt), "LAST KNOWN ALIVE DATE", NA_character_)
  )
  rsp <- which(!is.na(bor$rsp_start))
  dor_ev <- pfs_ev[rsp]
  dor <- data.frame(
    tte_i = rsp, PARAMCD = "DOR", PARAM = "Duration of Response (months)", PARAMN = 3,
    STARTDT = bor$rsp_start[rsp], ADT = ifelse(is.na(dor_ev), last_ass[rsp], dor_ev),
    CNSR = ifelse(is.na(dor_ev), 1L, 0L),
    EVNTDESC = ifelse(is.na(dor_ev), NA_character_, pfs$EVNTDESC[rsp]),
    CNSDTDSC = ifelse(is.na(dor_ev), "LAST ADEQUATE TUMOR ASSESSMENT", NA_character_)
  )
  adtte <- rbind(pfs, os, dor)
  adtte$ADT <- as.Date(adtte$ADT, origin = "1970-01-01")
  adtte$AVAL <- months(adtte$STARTDT, adtte$ADT)
  adtte$AVALU <- "MONTHS"
  adtte <- adtte[order(adtte$tte_i, adtte$PARAMN), ]

  # ---- ADSL ------------------------------------------------------------------
  subjid <- sprintf("%04d", seq_len(n))
  usubjid <- sprintf("%s-%s-%s", study, siteid, subjid)
  eff <- treated & seq_len(n) %in% ass$i
  dthcaus <- ifelse(dthfl == "N", NA_character_,
                    ifelse(!is.na(hist$pd_day), "PROGRESSIVE DISEASE",
                           ifelse(seq_len(n) %in% ae$i[ae$fatal], "ADVERSE EVENT", "OTHER")))
  armcd <- ifelse(drug, "DRUGX", "CTRL")
  adsl <- data.frame(
    STUDYID = study, USUBJID = usubjid, SUBJID = subjid, SITEID = siteid, COUNTRY = unname(cntry),
    REGION1 = unname(region), AGE = age, AGEU = "YEARS", AGEGR1 = ifelse(age < 65, "<65", ">=65"),
    AGEGR1N = ifelse(age < 65, 1, 2), SEX = sex, RACE = unname(race),
    ECOGBL = as.character(ecog), STRATA1 = paste0("ECOG ", ecog, " / ", region),
    ARM = arm, ARMCD = armcd, ACTARM = ifelse(treated, arm, "Not Treated"),
    ACTARMCD = ifelse(treated, armcd, "NOTTRT"),
    TRT01P = arm, TRT01PN = ifelse(drug, 1, 2),
    TRT01A = ifelse(treated, arm, NA_character_), TRT01AN = ifelse(treated, ifelse(drug, 1, 2), NA_real_),
    RANDDT = randdt, TRTSDT = trtsdt, TRTEDT = trtedt,
    TRTDURD = as.numeric(trtedt - trtsdt) + 1,
    ITTFL = "Y", SAFFL = ifelse(treated, "Y", "N"), EFFFL = ifelse(eff, "Y", "N"),
    EOTSTT = ifelse(!treated, "NOT STARTED", ifelse(hist$stop == "ONGOING", "ONGOING", "DISCONTINUED")),
    DCTREAS = ifelse(!treated | hist$stop == "ONGOING", NA_character_, hist$stop),
    DTHFL = dthfl, DTHDT = dthdt, DTHCAUS = dthcaus, LSTALVDT = lstalvdt, DCUTDT = dco,
    BSLD = base
  )

  # ---- subject keys and labels -----------------------------------------------
  keys <- c("STUDYID", "USUBJID", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN")
  add_keys <- function(d, idx, extra = character()) {
    s <- adsl[idx, c(keys, extra), drop = FALSE]
    rownames(s) <- NULL
    rownames(d) <- NULL
    out <- cbind(s, d[, setdiff(names(d), c(keys, "ae_i", "ex_i", "lb_i", "vs_i", "tr_i", "rs_i", "tte_i")),
                      drop = FALSE])
    if ("ADT" %in% names(out)) out$ADY <- study_day(out$ADT, adsl$TRTSDT[idx])
    out
  }
  pop <- c("ITTFL", "SAFFL", "EFFFL")
  covs <- c("AGEGR1", "SEX", "RACE", "ECOGBL", "REGION1", "STRATA1")
  out <- list(
    ADSL = adsl,
    ADEX = add_keys(adex, adex$ex_i, "SAFFL"),
    ADAE = add_keys(adae, adae$ae_i, "SAFFL"),
    ADLB = add_keys(adlb, adlb$lb_i, "SAFFL"),
    ADVS = add_keys(advs, advs$vs_i, "SAFFL"),
    ADRS = add_keys(adrs, adrs$rs_i, pop),
    ADTR = add_keys(adtr, adtr$tr_i, pop),
    ADTTE = add_keys(adtte, adtte$tte_i, c(pop, covs))
  )
  labels <- c(
    STUDYID = "Study Identifier", USUBJID = "Unique Subject Identifier", SUBJID = "Subject Identifier for the Study",
    SITEID = "Study Site Identifier", COUNTRY = "Country", REGION1 = "Geographic Region 1",
    AGE = "Age", AGEU = "Age Units", AGEGR1 = "Pooled Age Group 1", AGEGR1N = "Pooled Age Group 1 (N)",
    SEX = "Sex", RACE = "Race", ECOGBL = "Baseline ECOG Performance Status",
    STRATA1 = "Randomization Stratum", ARM = "Description of Planned Arm", ARMCD = "Planned Arm Code",
    ACTARM = "Description of Actual Arm", ACTARMCD = "Actual Arm Code",
    TRT01P = "Planned Treatment for Period 01", TRT01PN = "Planned Treatment for Period 01 (N)",
    TRT01A = "Actual Treatment for Period 01", TRT01AN = "Actual Treatment for Period 01 (N)",
    RANDDT = "Date of Randomization", TRTSDT = "Date of First Exposure to Treatment",
    TRTEDT = "Date of Last Exposure to Treatment", TRTDURD = "Total Treatment Duration (Days)",
    ITTFL = "Intent-To-Treat Population Flag", SAFFL = "Safety Population Flag",
    EFFFL = "Efficacy Evaluable Population Flag", EOTSTT = "End of Treatment Status",
    DCTREAS = "Reason for Discontinuation of Treatment", DTHFL = "Subject Death Flag",
    DTHDT = "Date of Death", DTHCAUS = "Cause of Death", LSTALVDT = "Date Last Known Alive",
    DCUTDT = "Data Cutoff Date", BSLD = "Baseline Sum of Diameters (mm)",
    PARAMCD = "Parameter Code", PARAM = "Parameter", PARAMN = "Parameter (N)", PARCAT1 = "Parameter Category 1",
    AVISIT = "Analysis Visit", AVISITN = "Analysis Visit (N)", ADT = "Analysis Date", ADY = "Analysis Relative Day",
    AVAL = "Analysis Value", AVALC = "Analysis Value (C)", AVALU = "Analysis Value Unit",
    BASE = "Baseline Value", CHG = "Change from Baseline", PCHG = "Percent Change from Baseline",
    NADIR = "Nadir Before This Assessment", ABLFL = "Baseline Record Flag", ONTRTFL = "On Treatment Record Flag",
    ANL01FL = "Analysis Flag 01", ANRLO = "Analysis Normal Range Lower Limit",
    ANRHI = "Analysis Normal Range Upper Limit", ANRIND = "Analysis Reference Range Indicator",
    BNRIND = "Baseline Reference Range Indicator", ATOXGR = "Analysis Toxicity Grade",
    BTOXGR = "Baseline Toxicity Grade", EXPLDOS = "Planned Dose (mg)", EXDOSRFL = "Dose Reduced Flag",
    ASTDT = "Analysis Start Date", AENDT = "Analysis End Date", ASTDY = "Analysis Start Relative Day",
    AENDY = "Analysis End Relative Day", AESEQ = "Sequence Number", AETERM = "Reported Term for the Adverse Event",
    AEDECOD = "Dictionary-Derived Term", AEBODSYS = "Body System or Organ Class",
    AESOC = "Primary System Organ Class", AETOXGR = "Standard Toxicity Grade", AESEV = "Severity/Intensity",
    AESER = "Serious Event", AEREL = "Causality", AEACN = "Action Taken with Study Treatment",
    AEOUT = "Outcome of Adverse Event", AESDTH = "Results in Death",
    TRTEMFL = "Treatment Emergent Analysis Flag", STARTDT = "Time-to-Event Origin Date for Subject",
    CNSR = "Censor", EVNTDESC = "Event or Censoring Description", CNSDTDSC = "Censor Date Description"
  )
  lapply(out, function(d) {
    for (v in intersect(names(d), names(labels))) attr(d[[v]], "label") <- unname(labels[v])
    d
  })
}
