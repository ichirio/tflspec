# The stack templates' `denominator`: NULL, "data", a number, numbers named by
# the values of `by`, or an analysis data's id (D2a).  Templates: hierarchical,
# ae_soc_pt, ae_ser_soc_pt, ae_disc_soc_pt, ae_pt_freq, ae_max_grade.

dv_args <- stats::setNames(
  lapply(c("hierarchical", "ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt", "ae_pt_freq", "ae_max_grade"),
         stack_req),
  c("hierarchical", "ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt", "ae_pt_freq", "ae_max_grade"))
dv_codelists <- list(TRT01A = c("Drug X", "Control"))
dv_pops <- list(list(id = "SAF", label = "Safety Analysis Set"),
                list(id = "ITT", label = "Intent-To-Treat Analysis Set"))

dv_expand <- function(name, args = list(), ...) {
  tfl_template_expand(name, c(list(population = "SAF"), dv_args[[name]], args, list(...)),
                      codelists = dv_codelists, population = dv_pops)
}
# (the template with the arguments it needs and has no default for: the variable set and `by`)
dv_expand_with <- function(tpl, args, codelists = NULL) {
  tfl_template_expand(tpl, c(list(population = "SAF"), dv_args[[tpl]], args), codelists = codelists)
}
dv_dens <- function(x) lapply(x$ard$analyses, function(a) a$denominator)
dv_ids <- function(x) vapply(x$ard$analysis_data, function(d) d$id, "")
dv_msg <- function(tpl, text) sprintf("%s: %s", tpl, text)
dv_all_msg <- function(tpl) {
  sprintf(paste0("%s: denominator takes NULL (the analysis set), \"data\" (the records' subjects), ",
                 "a number, numbers named by the values of TRT01A, or an analysis data's id."), tpl)
}

# ---- expansions ----

test_that("the default is the analysis set; the set's own id is the same expansion", {
  for (tpl in names(dv_args)) {
    x <- dv_expand(tpl)
    expect_true(all(vapply(dv_dens(x), identical, NA, "adsl_saf")), info = tpl)
    expect_identical(dv_expand(tpl, denominator = "adsl_saf"), x, info = tpl)
    expect_identical(dv_expand(tpl, denominator = NULL), x, info = tpl)
  }
})

test_that("a number is every column's N; a text of numbers is read as the numbers", {
  for (tpl in names(dv_args)) {
    x <- dv_expand(tpl, denominator = 20)
    expect_true(all(vapply(dv_dens(x), identical, NA, 20L)), info = tpl)
    expect_identical(dv_ids(x), dv_ids(dv_expand(tpl)), info = tpl)
    expect_identical(dv_expand(tpl, denominator = 20L), x, info = tpl)
    expect_identical(dv_expand(tpl, denominator = "20"), x, info = tpl)
  }
})

test_that("numbers named by the values of by: a vector, a YAML map, or its text", {
  v <- c("Drug X" = 10, Control = 8)
  w <- c("Drug X" = 10L, Control = 8L)
  for (tpl in names(dv_args)) {
    x <- dv_expand(tpl, denominator = v)
    expect_true(all(vapply(dv_dens(x), identical, NA, w)), info = tpl)
    expect_identical(dv_expand(tpl, denominator = as.list(v)), x, info = tpl)
    expect_identical(dv_expand(tpl, denominator = "c(\"Drug X\" = 10, \"Control\" = 8)"), x,
                     info = tpl)
    # (the order of the names is the user's)
    y <- dv_expand(tpl, denominator = v[2:1])
    expect_true(all(vapply(dv_dens(y), identical, NA, w[2:1])), info = tpl)
  }
})

test_that("\"data\" is the records' subjects: one analysis data of the set's subjects and arms", {
  for (tpl in names(dv_args)) {
    x <- dv_expand(tpl, denominator = "data")
    ids <- dv_ids(x)
    den <- grep("_den$", ids, value = TRUE)
    expect_length(den, 1L)
    d <- x$ard$analysis_data[[match(den, ids)]]
    expect_identical(as.character(d$code), sprintf("dplyr::distinct(%s, USUBJID, TRT01A)", d$from),
                     info = tpl)
    expect_true(all(vapply(dv_dens(x), identical, NA, den)), info = tpl)
  }
})

test_that("an analysis set's data id: the template makes it from ADSL, narrowed by subset", {
  for (tpl in names(dv_args)) {
    x <- dv_expand(tpl, denominator = "adsl_itt")
    ids <- dv_ids(x)
    expect_identical(ids[1:2], c("adsl_saf", "adsl_itt"), info = tpl)
    d <- x$ard$analysis_data[[2L]]
    expect_identical(d$from, "ADSL", info = tpl)
    expect_identical(d$population, "ITT", info = tpl)
    expect_identical(d$label, "Intent-To-Treat set", info = tpl)
    expect_true(all(vapply(dv_dens(x), identical, NA, "adsl_itt")), info = tpl)
    # (the subset narrows both sets)
    s <- dv_expand(tpl, denominator = "adsl_itt", subset = "SEX == \"F\"")
    for (k in 1:2) {
      expect_match(as.character(s$ard$analysis_data[[k]]$where), "SEX", fixed = TRUE, info = tpl)
    }
  }
  # (another report's own data: the report's `ard:` makes it, so it is not added)
  x <- dv_expand("ae_soc_pt", denominator = "adsl_part")
  expect_identical(dv_ids(x), dv_ids(dv_expand("ae_soc_pt")))
  expect_identical(dv_dens(x)[[1L]], "adsl_part")
})

test_that("ae_pt_freq and ae_max_grade take denominator, with its documentation", {
  doc <- function(nm) {
    a <- tfl_tpl_args(nm)
    a$doc[a$param == "denominator"]
  }
  for (tpl in names(dv_args)) {
    expect_true("denominator" %in% tfl_tpl_args(tpl)$param, info = tpl)
    expect_match(doc(tpl), "analysis data", fixed = TRUE, info = tpl)
    expect_match(doc(tpl), "\"data\"", fixed = TRUE, info = tpl)
  }
  expect_identical(doc("ae_pt_freq"), doc("ae_soc_pt"))
  expect_identical(doc("ae_max_grade"), doc("hierarchical"))
})

# ---- the refusals ----

test_that("a number that is not a positive whole number stops", {
  for (tpl in names(dv_args)) {
    for (v in list(0, -3, 2.5, NA_real_)) {
      expect_error(dv_expand(tpl, denominator = v),
                   dv_msg(tpl, sprintf(paste0("denominator = %s: a number is the subjects of every ",
                                              "column: a positive whole number."), format(v))),
                   fixed = TRUE, info = paste(tpl, v))
    }
  }
  expect_error(dv_expand("ae_soc_pt", denominator = "0"),
               "ae_soc_pt: denominator = 0: a number is the subjects of every column: a positive whole number.",
               fixed = TRUE)
})

test_that("numbers named by the values of by: the names, the numbers, the arms", {
  tpl <- "ae_soc_pt"
  # no names, or a blank one
  expect_error(dv_expand(tpl, denominator = c(10, 8)),
               dv_msg(tpl, paste0("denominator: several numbers are named by the values of TRT01A, ",
                                  "c(\"Drug X\" = 60, \"Control\" = 55).")), fixed = TRUE)
  expect_error(dv_expand(tpl, denominator = stats::setNames(c(10, 8), c("Drug X", ""))),
               "several numbers are named by the values of TRT01A", fixed = TRUE)
  # a name twice
  expect_error(dv_expand(tpl, denominator = c("Drug X" = 10, "Drug X" = 8)),
               dv_msg(tpl, "denominator names \"Drug X\" twice."), fixed = TRUE)
  # the numbers
  expect_error(dv_expand(tpl, denominator = c("Drug X" = 10, Control = 0)),
               dv_msg(tpl, paste0("denominator: the numbers are the subjects of each column: ",
                                  "positive whole numbers.")), fixed = TRUE)
  expect_error(dv_expand(tpl, denominator = c("Drug X" = 10, Control = 7.5)),
               "positive whole numbers", fixed = TRUE)
  expect_error(dv_expand(tpl, denominator = "c(\"Drug X\" = 10, \"Control\" = 7.5)"),
               dv_msg(tpl, paste0("denominator \"c(\"Drug X\" = 10, \"Control\" = 7.5)\": numbers are ",
                                  "whole and each named by a value of TRT01A: ",
                                  "c(\"Drug X\" = 60, \"Control\" = 55).")), fixed = TRUE)
  # an arm missing, a name that is no arm (the code list of `by`)
  expect_error(dv_expand(tpl, denominator = c("Drug X" = 10)),
               dv_msg(tpl, paste0("denominator has no number for \"Control\", a value of TRT01A: ",
                                  "give one for each (Drug X, Control).")), fixed = TRUE)
  expect_error(dv_expand(tpl, denominator = c("Drug X" = 10, Control = 8, Placebo = 9)),
               dv_msg(tpl, "denominator names \"Placebo\", not a value of TRT01A (Drug X, Control)."),
               fixed = TRUE)
  # (a code list as the YAML holds it: rows of value / label)
  cl <- list(TRT01A = list(list(value = "Drug X", label = "Drug X (mg)"),
                           list(value = "Control", label = "Control")))
  expect_error(dv_expand_with(tpl, list(denominator = c("Drug X (mg)" = 10,
                                                                                  Control = 8)),
                                   codelists = cl),
               "denominator names \"Drug X (mg)\", not a value of TRT01A (Drug X, Control).",
               fixed = TRUE)
  expect_silent(dv_expand_with(tpl, list(denominator = c("Drug X" = 10, Control = 8)),
                               codelists = cl))
  # (no code list: the arms are the data's, which a template cannot see)
  expect_silent(dv_expand_with(tpl, list(denominator = c("Drug X" = 10))))
})

test_that("anything else stops with the list of what denominator takes", {
  for (tpl in names(dv_args)) {
    for (v in list(TRUE, "Adsl", "adsl saf", character(), c("a", "b"), list(1, 2))) {
      expect_error(dv_expand(tpl, denominator = v), dv_all_msg(tpl), fixed = TRUE,
                   info = paste(tpl, deparse(v)))
    }
  }
  expect_error(dv_expand("ae_soc_pt", denominator = "-5"), dv_all_msg("ae_soc_pt"), fixed = TRUE)
})

# ---- header_n follows the denominator ----

dv_hdr <- function(x) {
  it <- Filter(function(i) identical(i$fn, "plan_col_header"), x$content)
  unlist(lapply(it[[1L]]$args$header, function(h) unlist(h$text)))
}
dv_why <- paste0("differs from the percentages' denominator; a header N apart from it ",
                 "is not supported yet: use header_n = \"denominator\" or \"none\".")

test_that("header_n: \"denominator\" is the denominator's N, \"none\" takes it away", {
  for (tpl in names(dv_args)) {
    for (v in list(20, c("Drug X" = 10, Control = 8), "adsl_itt", "adsl_part", "data")) {
      x <- dv_expand(tpl, denominator = v)
      expect_identical(dv_expand(tpl, denominator = v, header_n = "denominator"), x, info = tpl)
      expect_true(any(grepl("(N={n", dv_hdr(x), fixed = TRUE)), info = tpl)
      y <- dv_expand(tpl, denominator = v, header_n = "none")
      expect_false(any(grepl("N=", dv_hdr(y), fixed = TRUE)), info = tpl)
      expect_identical(y$ard, x$ard, info = tpl)
      # (the set and the tested subjects: only when they are the denominator)
      if (!identical(v, "data")) {
        expect_error(dv_expand(tpl, denominator = v, header_n = "tested"),
                     sprintf("%s: header_n = \"tested\" %s", tpl, dv_why), fixed = TRUE)
      }
      if (!identical(v, "adsl_saf")) {
        expect_error(dv_expand(tpl, denominator = v, header_n = "set"),
                     sprintf("%s: header_n = \"set\" %s", tpl, dv_why), fixed = TRUE)
      }
    }
    # the analysis set's own id is the set
    x <- dv_expand(tpl, denominator = "adsl_saf", header_n = "set")
    expect_identical(x, dv_expand(tpl), info = tpl)
    # "data" is the tested subjects
    x <- dv_expand(tpl, denominator = "data", header_n = "tested")
    expect_identical(x, dv_expand(tpl, denominator = "data"), info = tpl)
  }
  ch <- function(nm) tfl_tpl_args(nm)$doc[tfl_tpl_args(nm)$param == "header_n"]
  expect_identical(ch("ae_pt_freq"), ch("ae_soc_pt"))
  expect_identical(ch("ae_max_grade"), ch("ae_soc_pt"))
})

# ---- a report: the unknown data id ----

dv_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  # 12 subjects of DMY-001: 6 a arm; the safety set leaves out 6 and 12
  id <- sprintf("DMY-001-%03d", 1:12)
  sl0 <- tflspec::adsl
  sl <- rbind(sl0[sl0$TRT01A == "Drug X", ][1:6, ], sl0[sl0$TRT01A == "Control", ][1:6, ])
  sl$USUBJID <- id
  sl$TRT01A <- rep(c("Drug X", "Control"), each = 6)
  sl$SAFFL <- ifelse(seq_len(12) %in% c(6, 12), "N", "Y")
  sl$ITTFL <- "Y"
  sl$SEX <- ifelse(seq_len(12) %in% c(1, 2, 4, 7, 9, 10, 11), "F", "M")
  # treatment-emergent events (subjects 6 and 12 are outside the safety set):
  # 1 headache, nausea; 2 headache (mild, then severe); 3 nausea; 6 headache;
  # 7 headache; 8 nausea, vomiting; 12 nausea
  ae <- data.frame(
    n = c(1, 1, 2, 2, 3, 6, 7, 8, 8, 12),
    soc = c("Nervous system disorders", "Gastrointestinal disorders",
            "Nervous system disorders", "Nervous system disorders", "Gastrointestinal disorders",
            "Nervous system disorders", "Nervous system disorders", "Gastrointestinal disorders",
            "Gastrointestinal disorders", "Gastrointestinal disorders"),
    pt = c("Headache", "Nausea", "Headache", "Headache", "Nausea", "Headache", "Headache",
           "Nausea", "Vomiting", "Nausea"),
    sev = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MILD", "MODERATE", "SEVERE", "MILD",
            "MILD"),
    stringsAsFactors = FALSE)
  a0 <- tflspec::adae[1L, ]
  a0 <- a0[rep(1L, nrow(ae)), ]
  a0$USUBJID <- id[ae$n]
  a0$TRT01A <- sl$TRT01A[ae$n]
  a0$AEBODSYS <- ae$soc
  a0$AEDECOD <- ae$pt
  a0$AESEV <- ae$sev
  a0$TRTEMFL <- "Y"
  a0$SAFFL <- sl$SAFFL[ae$n]
  a0 <- a0[setdiff(names(a0), "TRT01AN")]
  for (x in list(list("adsl", sl), list("adae", a0))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "DMY-001")
  list(dir = d, adsl = sl, adae = a0)
}

# a report of a template: its ARD (as a data frame) and the header's text
dv_report <- function(s, name, args, own = NULL) {
  f <- file.path(s$dir, "spec", "outputs", "T-DV.yml")
  tfl_write_output(tfl_output("T-DV", "table", population = "SAF", extends = "defaults",
                              template = list(name = name, args = utils::modifyList(dv_args[[name]], as.list(args))),
                              ard = own), f)
  st <- tfl_read_study_spec(s$dir)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-DV", dir = s$dir, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  pg <- suppressMessages(rtfreporter::plan_apply(p, "pages"))
  hdr <- vapply(unlist(pg[[1L]]$col_header, recursive = FALSE), function(z) {
    paste(as.character(unlist(z)), collapse = "")
  }, "")
  list(ard = as.data.frame(ard), header = gsub("\n", " ", hdr, fixed = TRUE))
}

# the counts the ARD gives a preferred term: n, N and p of the arm
dv_cells <- function(ard, var, level, arm, analysis = NULL, pt = NULL) {
  chr1 <- function(z) vapply(z, function(v) paste(as.character(unlist(v)), collapse = "|"), "")
  k <- ard$variable == var & chr1(ard$variable_level) == level & chr1(ard$group1_level) == arm
  if (!is.null(analysis)) k <- k & ard$analysis_id == analysis
  # (the term in an outer level of the row's keys)
  if (!is.null(pt)) {
    g <- setdiff(grep("^group[0-9]+_level$", names(ard), value = TRUE), "group1_level")
    k <- k & Reduce(`|`, lapply(g, function(col) chr1(ard[[col]]) == pt))
  }
  a <- ard[k, ]
  stats::setNames(as.numeric(unlist(a$stat[match(c("n", "N", "p"), a$stat_name)])), c("n", "N", "p"))
}
dv_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
}

test_that("an analysis data the report does not have stops, naming what it has", {
  dv_skip()
  s <- dv_study(withr::local_tempdir())
  o <- tfl_output("T-DV", "table", population = "SAF", extends = "defaults",
                  template = list(name = "ae_soc_pt", args = c(stack_req("ae_soc_pt"),
                                                               list(denominator = "adsl_zzz"))))
  st <- tfl_read_study_spec(s$dir)
  expect_error(tfl_resolve_output(o, st),
                 paste0("T-DV: template ae_soc_pt: denominator \"adsl_zzz\" is not an analysis data ",
                        "of the report (adsl_saf, adae_saf). The template makes adsl_saf and the ",
                        "other analysis sets' adsl_<id>; the report's own come from its `ard:`."),
                 fixed = TRUE)
  # (a number, "data", an analysis set of the study: no complaint)
  for (v in list(5, "data", "adsl_itt")) {
    o$template$args$denominator <- v
    expect_no_warning(tfl_resolve_output(o, st))
  }
})

# ---- the numbers, by hand, from the data ----

# subjects of the safety set with a treatment-emergent event, by hand
dv_by_hand <- function(s) {
  sl <- s$adsl
  ae <- s$adae
  saf <- sl[sl$SAFFL == "Y", ]
  ae <- ae[ae$USUBJID %in% saf$USUBJID, ]
  arms <- c("Drug X", "Control")
  cnt <- function(pt = NULL, soc = NULL, sev = NULL, arm) {
    z <- ae[ae$TRT01A == arm, ]
    if (!is.null(pt)) z <- z[z$AEDECOD == pt, ]
    if (!is.null(soc)) z <- z[z$AEBODSYS == soc, ]
    length(unique(z$USUBJID))
  }
  list(arms = arms, cnt = cnt,
       set = vapply(arms, function(a) sum(saf$TRT01A == a), 0L),
       itt = vapply(arms, function(a) sum(sl$TRT01A == a), 0L),
       tested = vapply(arms, function(a) cnt(arm = a), 0L))
}

test_that("hand counts of the fixture: the checks below rest on them", {
  dv_skip()
  s <- dv_study(withr::local_tempdir())
  h <- dv_by_hand(s)
  expect_identical(unname(h$set), c(5L, 5L))
  expect_identical(unname(h$itt), c(6L, 6L))
  expect_identical(unname(h$tested), c(3L, 2L))
  expect_identical(c(h$cnt(pt = "Headache", arm = "Drug X"), h$cnt(pt = "Headache", arm = "Control")),
                   c(2L, 1L))
  expect_identical(c(h$cnt(pt = "Nausea", arm = "Drug X"), h$cnt(pt = "Nausea", arm = "Control")),
                   c(2L, 1L))
})

test_that("ae_soc_pt, ae_pt_freq: n, N and % of each denominator, and the header's N", {
  dv_skip()
  s <- dv_study(withr::local_tempdir())
  h <- dv_by_hand(s)
  own <- list(analysis_data = list(list(
    id = "adsl_female", label = "Safety set, women", from = "ADSL", population = "SAF",
    where = tfl_r("SEX == \"F\""))))
  # women of the safety set: Drug X 1, 2, 4; Control 7, 9, 10, 11
  fem <- c("Drug X" = 3L, Control = 4L)
  forms <- list(
    set = list(den = NULL, N = h$set),
    data = list(den = "data", N = h$tested),
    one = list(den = 20, N = c("Drug X" = 20L, Control = 20L)),
    vec = list(den = c("Drug X" = 10, Control = 8), N = c("Drug X" = 10L, Control = 8L)),
    itt = list(den = "adsl_itt", N = h$itt),
    own = list(den = "adsl_female", N = fem))
  for (tpl in c("ae_soc_pt", "ae_pt_freq")) {
    for (nm in names(forms)) {
      f <- forms[[nm]]
      a <- c(if (!is.null(f$den)) list(denominator = f$den), if (tpl == "ae_pt_freq") list(overall = "Any TEAE"))
      r <- dv_report(s, tpl, a, own = if (nm == "own") own)
      info <- paste(tpl, nm)
      # the header: each arm's N
      for (arm in h$arms) {
        expect_true(any(grepl(sprintf("%s (N=%d)", arm, f$N[[arm]]), r$header, fixed = TRUE)),
                    info = paste(info, arm, paste(r$header, collapse = " | ")))
      }
      for (arm in h$arms) {
        # a preferred term, and the any-event row (the sentinel)
        for (pt in c("Headache", "Nausea")) {
          c1 <- dv_cells(r$ard, "AEDECOD", pt, arm)
          expect_equal(c1[["n"]], h$cnt(pt = pt, arm = arm), info = paste(info, arm, pt))
          expect_equal(c1[["N"]], f$N[[arm]], info = paste(info, arm, pt))
          expect_equal(c1[["p"]], h$cnt(pt = pt, arm = arm) / f$N[[arm]], info = paste(info, arm, pt))
        }
        c2 <- dv_cells(r$ard, "..ard_hierarchical_overall..", "TRUE", arm)
        expect_equal(c2[["n"]], h$tested[[arm]], info = paste(info, arm, "any"))
        expect_equal(c2[["N"]], f$N[[arm]], info = paste(info, arm, "any"))
        expect_equal(c2[["p"]], h$tested[[arm]] / f$N[[arm]], info = paste(info, arm, "any"))
      }
    }
  }
})

test_that("ae_soc_pt: a subset narrows the analysis sets the denominator reads", {
  dv_skip()
  s <- dv_study(withr::local_tempdir())
  h <- dv_by_hand(s)
  # the women are subjects 1, 2, 4 (Drug X) and 7, 9, 10, 11 (Control), all in the
  # safety set and in the ITT set
  sl <- s$adsl
  f <- sl[sl$SEX == "F", ]
  itt_f <- vapply(h$arms, function(a) sum(f$TRT01A == a), 0L)
  r <- dv_report(s, "ae_soc_pt", list(denominator = "adsl_itt", subset = "SEX == \"F\""))
  for (arm in h$arms) {
    expect_true(any(grepl(sprintf("%s (N=%d)", arm, itt_f[[arm]]), r$header, fixed = TRUE)),
                info = paste(arm, paste(r$header, collapse = " | ")))
    c1 <- dv_cells(r$ard, "AEDECOD", "Headache", arm)
    expect_equal(c1[["N"]], itt_f[[arm]], info = arm)
    n <- length(unique(s$adae$USUBJID[s$adae$AEDECOD == "Headache" & s$adae$TRT01A == arm &
                                       s$adae$USUBJID %in% f$USUBJID & s$adae$SAFFL == "Y"]))
    expect_equal(c1[["n"]], n, info = arm)
  }
})

test_that("ae_max_grade: the subjects' highest grade over each denominator, and the header's N", {
  dv_skip()
  s <- dv_study(withr::local_tempdir())
  h <- dv_by_hand(s)
  forms <- list(
    set = list(den = NULL, N = h$set),
    data = list(den = "data", N = h$tested),
    one = list(den = 20, N = c("Drug X" = 20L, Control = 20L)),
    vec = list(den = c("Drug X" = 10, Control = 8), N = c("Drug X" = 10L, Control = 8L)),
    itt = list(den = "adsl_itt", N = h$itt))
  # by hand: a subject's highest severity of Headache: subject 2 SEVERE (mild, then
  # severe); subject 1 MILD (Drug X); subject 7 MODERATE (Control)
  top <- list("Drug X" = c(MILD = 1L, MODERATE = 0L, SEVERE = 1L),
              Control = c(MILD = 0L, MODERATE = 1L, SEVERE = 0L))
  for (nm in names(forms)) {
    f <- forms[[nm]]
    r <- dv_report(s, "ae_max_grade", c(if (!is.null(f$den)) list(denominator = f$den),
                                         list(grade_levels = c("MILD", "MODERATE", "SEVERE"))))
    for (arm in h$arms) {
      expect_true(any(grepl(sprintf("%s (N=%d)", arm, f$N[[arm]]), r$header, fixed = TRUE)),
                  info = paste(nm, arm, paste(r$header, collapse = " | ")))
      for (g in names(top[[arm]])) {
        if (top[[arm]][[g]] == 0L) next
        c1 <- dv_cells(r$ard, "AESEV", g, arm, analysis = "AE_MAX_PT", pt = "Headache")
        expect_equal(c1[["n"]], top[[arm]][[g]], info = paste(nm, arm, g))
        expect_equal(c1[["N"]], f$N[[arm]], info = paste(nm, arm, g))
        expect_equal(c1[["p"]], top[[arm]][[g]] / f$N[[arm]], info = paste(nm, arm, g))
      }
    }
  }
})

test_that("a named vector is a small data frame in the program; a number is a number", {
  dv_skip()
  s <- dv_study(withr::local_tempdir())
  f <- file.path(s$dir, "spec", "outputs", "T-DV.yml")
  code <- function(den) {
    tfl_write_output(tfl_output("T-DV", "table", population = "SAF", extends = "defaults",
                                template = list(name = "ae_soc_pt",
                                                args = c(stack_req("ae_soc_pt"), list(denominator = den)))), f)
    st <- tfl_read_study_spec(s$dir)
    paste(suppressMessages(tfl_ard_code(st, output_id = "T-DV")), collapse = "\n")
  }
  expect_match(code(c("Drug X" = 10, Control = 8)),
               paste0("denominator = data.frame(TRT01A = rep(c(\"Drug X\", \"Control\"), c(10L, 8L)), ",
                      "USUBJID = seq_len(18L))"), fixed = TRUE)
  expect_match(code(20), "denominator = 20L", fixed = TRUE)
  expect_match(code("adsl_itt"), "denominator = adsl_itt", fixed = TRUE)
  expect_match(code("adsl_itt"), "adsl_itt <- filter(adsl, ITTFL == \"Y\")", fixed = TRUE)
})
