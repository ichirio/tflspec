# Blank text values (an .xpt holds "" for a missing value), subjects with no
# record, a missing event day and a stub folded first: the templates on
# invented data (the package's datasets, with the cases put in)

bv_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
}

# a study of these datasets (a list of name = data frame)
bv_study <- function(root, data) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (n in names(data)) {
    haven::write_xpt(tflspec:::.ai_chr(data[[n]]), file.path(d, "adam", paste0(n, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "BV-1"))
  d
}

bv_put <- function(d, id, name, args, population = "SAF") {
  tfl_write_output(tfl_output(id, "table", population = population, extends = "defaults",
                              template = list(name = name, args = args)),
                   file.path(d, "spec", "outputs", paste0(id, ".yml")))
}

# the output's ARD as a data frame, its list columns as text
bv_ard <- function(d, id) {
  a <- suppressMessages(tfl_build_ard(tfl_read_study_spec(d), output_id = id, dir = d, save = FALSE))
  a <- as.data.frame(a)
  for (v in intersect(c("group1_level", "variable_level", "stat"), names(a))) {
    a[[v]] <- vapply(a[[v]], function(x) if (length(x)) as.character(x[[1L]]) else NA_character_, "")
  }
  a[, intersect(c("analysis_id", "group1", "group1_level", "variable", "variable_level", "stat_name",
                  "stat"), names(a))]
}

# one statistic of a variable's level in an arm (group1 = the arm)
bv_stat <- function(a, variable, level, arm, stat, analysis = NULL) {
  k <- a$variable == variable & a$variable_level %in% level & a$group1_level %in% arm &
    a$stat_name == stat
  if (!is.null(analysis)) k <- k & a$analysis_id == analysis
  as.numeric(a$stat[k])
}

test_that("demog, summary and disposition: a blank is missing, not a category", {
  bv_skip()
  adsl <- tflspec::adsl
  saf <- which(adsl$SAFFL == "Y")
  # one blank sex and one blank end-of-study status in each arm
  for (arm in c("Drug X", "Control")) {
    i <- saf[adsl$TRT01A[saf] == arm][1:2]
    adsl$SEX[i[1L]] <- ""
    adsl$EOSSTT[i[2L]] <- ""
  }
  d <- bv_study(withr::local_tempdir(), list(adsl = adsl))
  bv_put(d, "T-D", "demog", list(by = "TRT01A", continuous = "AGE", categorical = c("SEX", "AGEGR1")))
  bv_put(d, "T-S", "summary", list(by = "TRT01A", continuous = "AGE",
                                   categorical = c("SEX", "EOSSTT"), total = TRUE))
  bv_put(d, "T-DI", "disposition", list(by = "TRT01A", categorical = "EOSSTT"))
  s <- adsl[adsl$SAFFL == "Y", ]
  for (id in c("T-D", "T-S", "T-DI")) {
    a <- bv_ard(d, id)
    for (v in intersect(c("SEX", "EOSSTT"), unique(a$variable))) {
      lv <- a$variable_level[a$variable == v & a$stat_name == "n"]
      expect_false(any(lv %in% ""), info = paste(id, v))
      for (arm in c("Drug X", "Control")) {
        x <- s[[v]][s$TRT01A == arm]
        # the percent is of the whole analysis set's arm (blanks counted in the
        # denominator, the header N), and the blanks are a Missing row, last
        den <- length(x)
        expect_equal(bv_stat(a, v, "Missing", arm, "n"), sum(x == ""), info = paste(id, v, arm))
        expect_equal(bv_stat(a, v, "Missing", arm, "p"), sum(x == "") / den, info = paste(id, v, arm))
        x <- x[x != ""]
        for (l in unique(x)) {
          expect_equal(bv_stat(a, v, l, arm, "n"), sum(x == l), info = paste(id, v, l, arm))
          expect_equal(bv_stat(a, v, l, arm, "p"), sum(x == l) / den, info = paste(id, v, l, arm))
        }
      }
    }
  }
  # the Total column too
  a <- bv_ard(d, "T-S")
  tot <- a$variable == "SEX" & is.na(a$group1_level) & a$stat_name == "n"
  expect_setequal(a$variable_level[tot], c("F", "M", "Missing"))
  expect_equal(sum(as.numeric(a$stat[tot])), nrow(s))
  expect_equal(bv_stat(a, "SEX", "Missing", NA, "n"), sum(s$SEX == ""))
  # the program says it: the count reads the data with its blank text missing
  txt <- paste(tfl_ard_code(tfl_read_study_spec(d), output_id = "T-D", save = FALSE, dir = d),
               collapse = "\n")
  # (dplyr and cards attached in the program: no `pkg::`)
  expect_match(txt, "mutate(across(where(is.character), ~ na_if(.x, \"\"))) |>\n  ard_tabulate(",
               fixed = TRUE)
})

test_that("lab_abnormal: a blank category is missing, not a category", {
  bv_skip()
  lb <- tflspec::adlb
  j <- which(lb$PARAMCD == "ALT" & lb$ONTRTFL == "Y" & lb$SAFFL == "Y")
  lb$ANRIND[j[c(1, 5, 9)]] <- ""
  d <- bv_study(withr::local_tempdir(), list(adsl = tflspec::adsl, adlb = lb))
  bv_put(d, "T-L", "lab_abnormal", list(population = "SAF", where = "ONTRTFL == \"Y\"",
                                        params = "ALT"))
  a <- bv_ard(d, "T-L")
  lv <- a$variable_level[a$analysis_id == "ABNORMAL" & a$variable == "ANRIND"]
  expect_false(any(lv %in% ""))
  expect_true(all(c("HIGH", "LOW", "NORMAL") %in% lv))
  # the counts are those of the records with a category
  r <- lb[lb$PARAMCD == "ALT" & lb$ONTRTFL == "Y" & lb$SAFFL == "Y" & lb$ANRIND != "", ]
  for (arm in c("Drug X", "Control")) {
    ra <- r[r$TRT01A == arm, ]
    n <- tapply(ra$USUBJID, ra$ANRIND, function(u) length(unique(u)))
    for (l in names(n)) {
      expect_equal(bv_stat(a, "ANRIND", l, arm, "n"), unname(n[l]), info = paste(arm, l))
    }
  }
})

test_that("orr: a subject with no BOR record is a non-responder in N; a blank response is missing", {
  bv_skip()
  rs <- tflspec::adrs
  itt <- tflspec::adsl[tflspec::adsl$ITTFL == "Y", ]
  b <- rs[rs$PARAMCD == "BOR" & rs$USUBJID %in% itt$USUBJID, ]
  # two subjects of Drug X: one with no BOR record, one with a blank response
  dx <- b$USUBJID[b$TRT01P == "Drug X"]
  none <- dx[1L]
  blank <- dx[2L]
  rs <- rs[!(rs$PARAMCD == "BOR" & rs$USUBJID == none), ]
  rs$AVALC[rs$PARAMCD == "BOR" & rs$USUBJID == blank] <- ""
  d <- bv_study(withr::local_tempdir(), list(adsl = tflspec::adsl, adrs = rs))
  bv_put(d, "T-O", "orr", list(population = "ITT"), population = "ITT")
  a <- bv_ard(d, "T-O")
  b <- rs[rs$PARAMCD == "BOR" & rs$USUBJID %in% itt$USUBJID, ]
  for (arm in c("Drug X", "Control")) {
    n_itt <- sum(itt$TRT01P == arm)
    # the arm's N is its subjects of the analysis set
    expect_equal(bv_stat(a, "RESP", c("TRUE", "FALSE"), arm, "N", "ORR")[1L], n_itt, info = arm)
    resp <- sum(b$AVALC[b$TRT01P == arm] %in% c("CR", "PR"))
    expect_equal(bv_stat(a, "RESP", "TRUE", arm, "n", "ORR"), resp, info = arm)
  }
  expect_equal(bv_stat(a, "AVALC", "CR", "Drug X", "N", "BOR"), sum(itt$TRT01P == "Drug X"))
  lv <- a$variable_level[a$analysis_id == "BOR" & a$variable == "AVALC"]
  expect_false(any(lv %in% ""))
  for (l in c("CR", "PR", "SD", "PD")) {
    expect_equal(bv_stat(a, "AVALC", l, "Drug X", "n", "BOR"),
                 sum(b$AVALC[b$TRT01P == "Drug X"] == l), info = l)
  }
  # the same for the columns layout
  bv_put(d, "T-O2", "orr", list(population = "ITT", stat_layout = "cols"), population = "ITT")
  a2 <- bv_ard(d, "T-O2")
  expect_equal(bv_stat(a2, "RESP", c("TRUE", "FALSE"), "Drug X", "N", "ORR")[1L],
               sum(itt$TRT01P == "Drug X"))
})

test_that("forest_hr: a blank subgroup value is no subgroup", {
  bv_skip()
  skip_if_not_installed("survival")
  # the subgroups are the population's (ADSL's) columns
  tt <- tflspec::adtte
  ad <- tflspec::adsl
  pfs <- tt$USUBJID[tt$PARAMCD == "PFS" & tt$SAFFL == "Y"]
  ad$SEX[ad$USUBJID %in% pfs[c(2, 30, 60)]] <- ""
  d <- bv_study(withr::local_tempdir(), list(adsl = ad, adtte = tt))
  tfl_write_output(tfl_output("F-1", "figure", population = "SAF", extends = "defaults",
                              template = list(name = "forest_hr",
                                              args = list(param = "PFS", population = "SAF"))),
                   file.path(d, "spec", "outputs", "F-1.yml"))
  a <- suppressWarnings(bv_ard(d, "F-1"))
  lv <- unique(a$group1_level[a$analysis_id == "HR_SEX"])
  expect_setequal(lv, c("F", "M"))
  # the other subgroup as it was
  expect_true(all(c("<65", ">=65") %in% a$group1_level[a$analysis_id == "HR_AGEGR1"]))
  p <- tt[tt$PARAMCD == "PFS" & tt$SAFFL == "Y", ]
  p$SEX <- ad$SEX[match(p$USUBJID, ad$USUBJID)]
  n <- bv_stat(a, "TRT01A", "Control", "F", "n_obs", "HR_SEX")
  expect_equal(n, sum(p$TRT01A == "Control" & p$SEX == "F"))
})

test_that("ae_eair: an event with no day uses the subject's other events; none: counted, warned", {
  bv_skip()
  ae <- tflspec::adae
  ad <- tflspec::adsl
  te <- ae[ae$TRTEMFL == "Y" & ae$SAFFL == "Y", ]
  # a subject with two events of one term: the first by day has no day
  pair <- names(which(table(paste(te$USUBJID, te$AEDECOD, sep = "|")) >= 2L))
  expect_true(length(pair) > 0L)
  one <- strsplit(pair[1L], "|", fixed = TRUE)[[1L]][1L]
  i <- which(paste(ae$USUBJID, ae$AEDECOD, sep = "|") == pair[1L] & ae$TRTEMFL == "Y")
  ae$ASTDY[i[which.min(ae$ASTDY[i])]] <- NA
  d <- bv_study(withr::local_tempdir(), list(adsl = ad, adae = ae))
  bv_put(d, "T-E", "ae_eair", list(population = "SAF"))
  expect_no_warning(a <- bv_ard(d, "T-E"))
  s <- ad[ad$SAFFL == "Y", ]
  first_day <- function(aes) {
    t2 <- aes[aes$TRTEMFL == "Y" & aes$SAFFL == "Y", ]
    tapply(t2$ASTDY, t2$USUBJID, function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE))
  }
  years <- function(sa, day) {
    sum(ifelse(sa$USUBJID %in% names(day) & !is.na(day[sa$USUBJID]), day[sa$USUBJID], sa$TRTDURD)) / 365.25
  }
  day <- first_day(ae)
  for (arm in c("Drug X", "Control")) {
    sa <- s[s$TRT01A == arm, ]
    expect_equal(bv_stat(a, "AEBODSYS", "Any TEAE", arm, "n_events", "EAIR"),
                 sum(sa$USUBJID %in% names(day)), info = arm)
    expect_equal(bv_stat(a, "AEBODSYS", "Any TEAE", arm, "tot_person_time", "EAIR"),
                 years(sa, day), info = arm)
  }
  # a subject whose events all have no day: counted, the whole exposure, one warning
  ae2 <- ae
  ae2$ASTDY[ae2$USUBJID == one & ae2$TRTEMFL == "Y"] <- NA
  d2 <- bv_study(withr::local_tempdir(), list(adsl = ad, adae = ae2))
  bv_put(d2, "T-E", "ae_eair", list(population = "SAF"))
  w <- character()
  a2 <- withCallingHandlers(bv_ard(d2, "T-E"), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  expect_match(w, "1 subject(s) have an event with no start day", fixed = TRUE, all = FALSE)
  arm <- ad$TRT01A[ad$USUBJID == one]
  sa <- s[s$TRT01A == arm, ]
  day2 <- first_day(ae2)
  expect_equal(bv_stat(a2, "AEBODSYS", "Any TEAE", arm, "n_events", "EAIR"),
               sum(sa$USUBJID %in% names(day2)))
  expect_equal(bv_stat(a2, "AEBODSYS", "Any TEAE", arm, "tot_person_time", "EAIR"), years(sa, day2))
  # no missing day: no warning
  d3 <- bv_study(withr::local_tempdir(), list(adsl = ad, adae = tflspec::adae))
  bv_put(d3, "T-E", "ae_eair", list(population = "SAF"))
  expect_no_warning(bv_ard(d3, "T-E"))
})

test_that("stat_block: the study's stub folded first, built to RTF, the numbers as without it", {
  bv_skip()
  d <- bv_study(withr::local_tempdir(), list(adsl = tflspec::adsl, adrs = tflspec::adrs))
  cells <- list(N = "{N}", "ORR(%) 95%CI" = c("{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}"))
  for (ext in c("defaults", "none")) {
    id <- paste0("T-SB-", ext)
    o <- tfl_output(id, "table", population = "ITT", extends = ext,
      ard = list(
        analysis_data = list(list(id = "adrs_bor", from = "ADRS", population = "ITT",
                                  where = tfl_r("PARAMCD == \"BOR\""),
                                  derive = tfl_r("RESP = AVALC %in% c(\"CR\", \"PR\")"))),
        analyses = list(list(id = "ORR", method = "proportion_ci", data = "adrs_bor", by = "TRT01P",
                             variables = "RESP"))),
      template = list(name = "stat_block", args = list(by = "TRT01P", variable = "RESP", cells = cells)))
    tfl_write_output(o, file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  f1 <- suppressMessages(tfl_run_output("T-SB-defaults", dir = d))
  f2 <- suppressMessages(tfl_run_output("T-SB-none", dir = d))
  for (f in c(f1, f2)) {
    rtf <- paste(readLines(f, warn = FALSE), collapse = " ")
    expect_true(grepl("32 (39.0)", rtf, fixed = TRUE), info = f)
    expect_true(grepl("27.9, 50.2", rtf, fixed = TRUE), info = f)
    expect_true(grepl("14 (17.9)", rtf, fixed = TRUE), info = f)
  }
  # the study's stub: the keys' name is the stub's column from then on
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-SB-defaults"]], st)
  g <- Filter(function(it) it$fn == "plan_row_group", ro$content)[[1L]]
  expect_identical(unlist(g$args$collapse), "row_label")
  h <- Filter(function(it) it$fn == "plan_col_header", ro$content)[[1L]]
  expect_true("row_label" %in% unlist(lapply(h$args$header, `[[`, "cols")))
  ro0 <- tfl_resolve_output(tfl_read_outputs(d)[["T-SB-none"]], st)
  g0 <- Filter(function(it) it$fn == "plan_row_group", ro0$content)[[1L]]
  expect_identical(unlist(g0$args$collapse), "group")
})

test_that(".tpl_stub_first(): only the keys the study's stub folds first are renamed", {
  item <- tfl_item
  items <- list(item("table_plan", list(cols = "ARM", rows = list(group = "variable", sub = "SUB"))),
                item("plan_row_group", list(mode = "value", collapse = list("group", "sub"))),
                item("plan_col_header", list(header = list(
                  list(line = "2", cols = "group", text = "x"),
                  list(line = "2", cols = ".values", span = "each", text = "{col}")))))
  stub <- list(item("plan_stub", list(name = "row_label", before = TRUE)))
  r <- .tpl_stub_first(items, stub)
  expect_identical(unlist(r[[2L]]$args$collapse), "row_label")
  expect_identical(r[[3L]]$args$header[[1L]]$cols, "row_label")
  expect_identical(r[[3L]]$args$header[[2L]]$cols, ".values")
  # folded last (the default), or no stub: as it was
  expect_identical(.tpl_stub_first(items, list(item("plan_stub", list(name = "row_label")))), items)
  expect_identical(.tpl_stub_first(items, list()), items)
  # the template's own vars say which keys fold
  own <- c(items, list(item("plan_stub", list(vars = "group", name = "row_label"))))
  r2 <- .tpl_stub_first(own, stub)
  expect_identical(unlist(r2[[2L]]$args$collapse), c("row_label", "sub"))
})

test_that("tfl_check_spec() accepts orr's columns layout and stat_block's cells", {
  bv_skip()
  d <- bv_study(withr::local_tempdir(), list(adsl = tflspec::adsl, adrs = tflspec::adrs))
  bv_put(d, "T-O4", "orr", list(population = "ITT", stat_layout = "cols"), population = "ITT")
  bv_put(d, "T-O5", "orr", list(population = "ITT", stat_layout = "cols", conf_level = 0.9),
         population = "ITT")
  cells <- list(N = "{N}", "ORR(%) 95%CI" = c("{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}"))
  tfl_write_output(tfl_output("T-SB", "table", population = "ITT", extends = "defaults",
    ard = list(
      analysis_data = list(list(id = "adrs_bor", from = "ADRS", population = "ITT",
                                where = tfl_r("PARAMCD == \"BOR\""),
                                derive = tfl_r("RESP = AVALC %in% c(\"CR\", \"PR\")"))),
      analyses = list(list(id = "ORR", method = "proportion_ci", data = "adrs_bor", by = "TRT01P",
                           variables = "RESP"))),
    template = list(name = "stat_block", args = list(by = "TRT01P", variable = "RESP", cells = cells))),
    file.path(d, "spec", "outputs", "T-SB.yml"))
  p <- suppressMessages(tfl_check_spec(d))
  expect_identical(nrow(p[p$level == "error", ]), 0L)
  # a row label that no plan_stat_groups() names is still wrong
  p2 <- tfl_schema_check(tfl_item("plan_cells", list("Mean (SD)" = "{mean}")))
  expect_match(p2$message, "is a row's label")
})
