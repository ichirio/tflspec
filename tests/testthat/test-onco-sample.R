# The oncology sample study XYZ-ONC-01 shipped in data/ (adsl ... adtte):
# consistent across datasets, plausible numbers; and its recipe,
# data-raw/gen_onco.R, seeded and current (where the source tree is here).

# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

onco <- list(ADSL = tflspec::adsl, ADEX = tflspec::adex, ADAE = tflspec::adae,
             ADLB = tflspec::adlb, ADVS = tflspec::advs, ADRS = tflspec::adrs,
             ADTR = tflspec::adtr, ADTTE = tflspec::adtte)
dco <- as.Date("2026-06-30")

test_that("the datasets carry labels, and the treatment is a factor with Drug X first", {
  for (nm in names(onco)) {
    d <- onco[[nm]]
    expect_s3_class(d, "data.frame")
    lab <- vapply(d, function(x) if (is.null(attr(x, "label"))) "" else attr(x, "label"), "")
    expect_true(all(nzchar(lab)), label = paste(nm, paste(names(lab)[!nzchar(lab)], collapse = ", ")))
    for (v in intersect(c("TRT01P", "TRT01A", "ARM"), names(d))) {
      expect_s3_class(d[[v]], "factor")
      expect_identical(levels(d[[v]]), c("Drug X", "Control"), label = paste(nm, v))
    }
    expect_true("TRT01PN" %in% names(d), label = nm)
  }
  expect_identical(as.integer(onco$ADSL$TRT01P), as.integer(onco$ADSL$TRT01PN))
})

test_that("data-raw/gen_onco.R: the same seed gives the shipped data; the caller's random numbers are untouched", {
  gen <- test_path("..", "..", "data-raw", "gen_onco.R")
  skip_if_not(file.exists(gen), "data-raw/ is not installed (only in the source tree)")
  env <- new.env()
  sys.source(gen, envir = env)
  set.seed(1)
  before <- .Random.seed
  again <- env$gen_onco()
  expect_identical(.Random.seed, before)
  expect_named(again, names(onco))
  expect_identical(again, onco)
  expect_false(identical(env$gen_onco(seed = 1)$ADSL, onco$ADSL))
  skip_on_cran()
  expect_lt(system.time(env$gen_onco())[["elapsed"]], 5)
})

test_that("every subject is in ADSL and every date is within randomisation .. cut-off", {
  adsl <- onco$ADSL
  expect_equal(nrow(adsl), 200)                                  # screened
  expect_equal(sum(adsl$ITTFL == "Y"), 160)                       # randomised
  expect_false(anyDuplicated(adsl$USUBJID) > 0)
  m <- function(d) match(d$USUBJID, adsl$USUBJID)
  for (nm in setdiff(names(onco), "ADSL")) {
    d <- onco[[nm]]
    expect_false(anyNA(m(d)), label = nm)
    for (v in names(d)[vapply(d, inherits, NA, "Date")]) {
      x <- d[[v]]
      ok <- is.na(x) | (x >= adsl$RANDDT[m(d)] & x <= dco)
      expect_true(all(ok), label = paste(nm, v))
    }
  }
  itt <- adsl[adsl$ITTFL == "Y", ]
  for (v in c("TRTSDT", "TRTEDT", "DTHDT", "LSTALVDT", "EOSDT")) {
    x <- itt[[v]]
    expect_true(all(is.na(x) | (x >= itt$RANDDT & x <= dco)), label = v)
  }
  expect_true(all(itt$RFICDT < itt$RANDDT))
  expect_true(all(adsl$TRTEDT >= adsl$TRTSDT, na.rm = TRUE))
  expect_identical(is.na(adsl$TRTSDT), adsl$SAFFL == "N")

  ex <- onco$ADEX
  i <- m(ex)
  expect_true(all(ex$ASTDT >= adsl$TRTSDT[i] & ex$AENDT <= adsl$TRTEDT[i]))
  last <- tapply(ex$ASTDT[ex$PARAMCD == "DOSE"], ex$USUBJID[ex$PARAMCD == "DOSE"], max)
  expect_equal(as.numeric(last), as.numeric(adsl$TRTEDT[match(names(last), adsl$USUBJID)]))

  ae <- onco$ADAE
  i <- m(ae)
  te <- ae$TRTEMFL == "Y"
  expect_true(all(ae$ASTDT[te] >= adsl$TRTSDT[i][te] & ae$ASTDT[te] <= adsl$TRTEDT[i][te] + 30))
  expect_true(all(ae$ASTDT[!te] < adsl$TRTSDT[i][!te]))
  expect_true(all(ae$AENDT >= ae$ASTDT, na.rm = TRUE))
  # deaths: a fatal event is a death on that day
  fatal <- ae$AEOUT == "FATAL"
  expect_true(all(adsl$DTHDT[i][fatal] == ae$ASTDT[fatal]))
  expect_true(all(ae$AETOXGR[fatal] == "5"))
  # a dose reduced in ADEX follows a "DOSE REDUCED" adverse event
  red <- unique(ex$USUBJID[ex$EXDOSRFL %in% "Y"])
  expect_true(all(red %in% ae$USUBJID[ae$AEACN == "DOSE REDUCED"]))
  disc <- adsl$USUBJID[adsl$DCTREAS %in% "ADVERSE EVENT"]
  expect_true(all(disc %in% ae$USUBJID[ae$AEACN == "DRUG WITHDRAWN"]))
})

test_that("responses follow the tumour sizes; nothing after progression; PFS and OS follow the events", {
  adsl <- onco$ADSL
  rs <- onco$ADRS
  tr <- onco$ADTR
  ovr <- rs[rs$PARAMCD == "OVR", ]
  bor <- rs[rs$PARAMCD == "BOR", ]
  expect_setequal(bor$USUBJID, adsl$USUBJID[adsl$ITTFL == "Y"])
  post <- tr[tr$ABLFL != "Y", ]
  # the same assessments in ADRS and ADTR
  expect_identical(paste(ovr$USUBJID, ovr$ADT), paste(post$USUBJID, post$ADT))

  for (u in bor$USUBJID[bor$AVALC %in% c("CR", "PR")]) {
    p <- post$PCHG[post$USUBJID == u]
    a <- post$AVAL[post$USUBJID == u]
    k <- seq_len(length(p) - 1)
    # confirmed: two consecutive assessments at or below -30% (CR: 0 mm)
    expect_true(any(p[k] <= -30 & p[k + 1] <= -30), label = u)
    if (bor$AVALC[bor$USUBJID == u] == "CR") expect_true(any(a[k] == 0 & a[k + 1] == 0), label = u)
  }
  expect_true(all(post$AVAL[paste(post$USUBJID, post$ADT) %in%
                              paste(ovr$USUBJID, ovr$ADT)[ovr$AVALC == "CR"]] == 0))
  expect_true(all(post$PCHG[paste(post$USUBJID, post$ADT) %in%
                              paste(ovr$USUBJID, ovr$ADT)[ovr$AVALC == "PR"]] <= -30))

  # no assessment after progression (PD is each subject's last)
  pd <- ovr[ovr$AVALC == "PD", ]
  expect_false(anyDuplicated(pd$USUBJID) > 0)
  last <- tapply(ovr$ADT, ovr$USUBJID, max)
  expect_equal(as.numeric(pd$ADT), as.numeric(last[pd$USUBJID]))
  expect_true(all(tapply(tr$ADT, tr$USUBJID, max)[pd$USUBJID] == as.numeric(pd$ADT)))

  tte <- onco$ADTTE
  pfs <- tte[tte$PARAMCD == "PFS", ]
  os <- tte[tte$PARAMCD == "OS", ]
  i <- match(pfs$USUBJID, adsl$USUBJID)
  pd_dt <- pd$ADT[match(pfs$USUBJID, pd$USUBJID)]
  ev <- pmin(pd_dt, adsl$DTHDT[i], na.rm = TRUE)
  last_ass <- as.Date(as.numeric(last[pfs$USUBJID]), origin = "1970-01-01")
  expect_identical(pfs$CNSR == 0, !is.na(ev))
  expect_equal(pfs$ADT[!is.na(ev)], ev[!is.na(ev)])
  cn <- is.na(ev)
  expect_equal(pfs$ADT[cn], ifelse(is.na(last_ass[cn]), adsl$RANDDT[i][cn], last_ass[cn]),
               ignore_attr = TRUE)
  j <- match(os$USUBJID, adsl$USUBJID)
  expect_identical(os$CNSR == 0, adsl$DTHFL[j] == "Y")
  expect_equal(os$ADT[os$CNSR == 0], adsl$DTHDT[j][os$CNSR == 0])
  expect_true(all(os$ADT[os$CNSR == 1] == dco))
  expect_identical(is.na(adsl$DTHDT), adsl$DTHFL == "N")
  # duration of response: the confirmed responders
  dor <- tte[tte$PARAMCD == "DOR", ]
  expect_setequal(dor$USUBJID, bor$USUBJID[bor$AVALC %in% c("CR", "PR")])
})

test_that("the numbers are those of a plausible phase 2/3 study", {
  skip_if_not_installed("survival")
  adsl <- onco$ADSL
  bor <- onco$ADRS[onco$ADRS$PARAMCD == "BOR", ]
  orr <- tapply(bor$AVALC %in% c("CR", "PR"), bor$TRT01P, mean)
  expect_true(orr[["Drug X"]] >= 0.30 && orr[["Drug X"]] <= 0.50)
  expect_true(orr[["Control"]] >= 0.10 && orr[["Control"]] <= 0.20)

  tte <- onco$ADTTE
  med <- function(cd) {
    d <- tte[tte$PARAMCD == cd, ]
    f <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01P, data = d)
    m <- summary(f)$table[, "median"]
    stats::setNames(m, sub("TRT01P=", "", names(m), fixed = TRUE))
  }
  pfs <- med("PFS")
  expect_true(pfs[["Drug X"]] >= 6 && pfs[["Drug X"]] <= 8)
  expect_true(pfs[["Control"]] >= 3 && pfs[["Control"]] <= 5)
  os <- tte[tte$PARAMCD == "OS", ]
  os$DRUG <- os$TRT01P == "Drug X"
  hr <- exp(stats::coef(survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ DRUG, data = os)))
  expect_true(hr >= 0.65 && hr <= 0.85)
  deaths <- mean(adsl$DTHFL[adsl$ITTFL == "Y"] == "Y")
  expect_true(deaths >= 0.40 && deaths <= 0.60)
})

test_that("ADSL holds the screened subjects: the screen failures have no arm and no other records", {
  adsl <- onco$ADSL
  sf <- adsl[adsl$ENRLFL == "N", ]
  expect_equal(nrow(sf), 40)
  expect_identical(as.vector(adsl$ENRLFL), as.vector(adsl$ITTFL))
  expect_true(all(is.na(sf$ARM) & is.na(sf$ARMCD) & is.na(sf$ACTARM) & is.na(sf$TRT01P) &
                    is.na(sf$TRT01A) & is.na(sf$RANDDT) & is.na(sf$TRTSDT)))
  expect_true(all(sf$ARMNRS == "SCREEN FAILURE"))
  expect_identical(levels(sf$ARM), c("Drug X", "Control"))
  expect_true(all(sf$ITTFL == "N" & sf$SAFFL == "N" & sf$EFFFL == "N"))
  expect_true(all(sf$EOSSTT == "DISCONTINUED" & sf$DCSREAS == "SCREEN FAILURE" & !is.na(sf$DCSREASP)))
  expect_true(all(sf$RFICDT < sf$EOSDT & sf$EOSDT == sf$LSTALVDT & sf$DTHFL == "N"))
  for (nm in setdiff(names(onco), "ADSL")) {
    expect_false(any(onco[[nm]]$USUBJID %in% sf$USUBJID), label = nm)
  }
  # the randomised subjects' study status: in follow-up until death
  itt <- adsl[adsl$ITTFL == "Y", ]
  expect_identical(itt$EOSSTT == "DISCONTINUED", itt$DTHFL == "Y")
  expect_true(all(itt$DCSREAS[itt$DTHFL == "Y"] == "DEATH"))
  expect_true(all(is.na(itt$DCSREAS[itt$DTHFL == "N"])))
  expect_equal(itt$EOSDT, itt$DTHDT)
  expect_identical(is.na(itt$ARMNRS), itt$SAFFL == "Y")
  expect_true(all(itt$ARMNRS[itt$SAFFL == "N"] == "ASSIGNED, NOT TREATED"))
  # ethnicity for every subject, CDISC's terms
  expect_true(all(adsl$ETHNIC %in% c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "NOT REPORTED")))
})

test_that("TTSKIN: the time to the first treatment-emergent skin disorder of the safety set", {
  adsl <- onco$ADSL
  ae <- onco$ADAE
  t <- onco$ADTTE[onco$ADTTE$PARAMCD == "TTSKIN", ]
  expect_setequal(t$USUBJID, adsl$USUBJID[adsl$SAFFL == "Y"])
  expect_true(all(t$AVALU == "DAYS"))
  i <- match(t$USUBJID, adsl$USUBJID)
  expect_equal(t$STARTDT, adsl$TRTSDT[i])
  skin <- ae[ae$TRTEMFL == "Y" & ae$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", ]
  first <- tapply(skin$ASTDT, skin$USUBJID, min)
  ev <- t$CNSR == 0
  expect_identical(ev, t$USUBJID %in% skin$USUBJID)
  expect_equal(as.numeric(t$ADT[ev]), as.numeric(first[t$USUBJID[ev]]))
  end <- pmin(adsl$TRTEDT[i] + 30, adsl$DTHDT[i], dco, na.rm = TRUE)
  expect_equal(t$ADT[!ev], end[!ev])
  expect_equal(t$AVAL, as.numeric(t$ADT - t$STARTDT) + 1)
  # Drug X has more skin events than Control
  rate <- tapply(ev, t$TRT01A, mean)
  expect_gt(rate[["Drug X"]], rate[["Control"]])
})

test_that("200 subjects make well under 1 MB", {
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(onco, f)
  expect_lt(file.size(f), 1e6)
})
