# `denominator` on the summary family (summary, demog, disposition), ae_overview and
# orr (D2b): the percentages' base as NULL, a number, numbers named by the values of
# `by`, an analysis data's id, or "data" (the records' subjects).  The column
# headings' N is not moved, except for header_n = "denominator" on ae_overview.

d2_pops <- list(list(id = "SAF", label = "Safety Analysis Set"),
                list(id = "ITT", label = "Intent-To-Treat Analysis Set"))
d2_codelists <- list(TRT01A = c("Drug X", "Control"), TRT01P = c("Drug X", "Control"))

d2_expand <- function(name, args = list(), ...) {
  base <- switch(name,
    summary = list(population = "SAF", by = "TRT01A", categorical = "SEX"),
    demog = list(population = "SAF", by = "TRT01A", categorical = "SEX"),
    disposition = list(population = "SAF", by = "TRT01A", categorical = "EOSSTT"),
    ae_overview = list(population = "SAF", by = "TRT01A"),
    orr = list(population = "ITT"))
  tfl_template_expand(name, c(base, args, list(...)), codelists = d2_codelists, population = d2_pops)
}
d2_an <- function(x, id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]
d2_ids <- function(x) vapply(x$ard$analysis_data, function(d) d$id, "")
d2_flag_dens <- function(x) lapply(x$ard$analyses[-1L], function(a) a$denominator)
d2_all_msg <- function(tpl, by = "TRT01A") {
  sprintf(paste0("%s: denominator takes NULL (the analysis set), \"data\" (the records' subjects), ",
                 "a number, numbers named by the values of %s, or an analysis data's id."), tpl, by)
}

# ---- summary, demog, disposition: expansions ----

test_that("summary family: no denominator is the analysis set, passed explicitly; the parameter has its documentation", {
  for (tpl in c("summary", "disposition")) {
    x <- d2_expand(tpl)
    cat <- d2_an(x, if (tpl == "disposition") "DISP" else "CAT")
    # (canon 1.38: the analysis set, not cards' "column")
    expect_identical(cat$denominator, "adsl_saf", info = tpl)
    expect_identical(d2_expand(tpl, denominator = NULL), x, info = tpl)
    a <- tfl_tpl_args(tpl)
    expect_true("denominator" %in% a$param, info = tpl)
    expect_match(a$doc[a$param == "denominator"], "analysis data", fixed = TRUE, info = tpl)
    expect_match(a$doc[a$param == "denominator"], "not moved", fixed = TRUE, info = tpl)
  }
  doc <- function(nm) {
    a <- tfl_tpl_args(nm)
    a$doc[a$param == "denominator"]
  }
  # (demog and stf_t02_demog have none: their percentages are over the analysis set)
  expect_false("denominator" %in% tfl_tpl_args("demog")$param)
  expect_false("denominator" %in% tfl_tpl_args("stf_t02_demog")$param)
  expect_error(d2_expand("demog", denominator = 20), "there is no `denominator` parameter")
  # ("data" is for ae_overview and the stack templates, not here)
  expect_false(grepl("\"data\"", doc("summary"), fixed = TRUE))
  expect_false(grepl("\"data\"", doc("disposition"), fixed = TRUE))
  expect_match(doc("summary"), "not for `total` = TRUE", fixed = TRUE)
})

test_that("summary family: a number, numbers by arm, an analysis data's id", {
  for (tpl in c("summary", "disposition")) {
    cat <- if (tpl == "disposition") "DISP" else "CAT"
    den <- function(...) d2_an(d2_expand(tpl, ...), cat)$denominator
    expect_identical(den(denominator = 20), 20L, info = tpl)
    expect_identical(den(denominator = "20"), 20L, info = tpl)
    expect_identical(den(denominator = c("Drug X" = 10, Control = 8)),
                     c("Drug X" = 10L, Control = 8L), info = tpl)
    expect_identical(den(denominator = list("Drug X" = 10, Control = 8)),
                     c("Drug X" = 10L, Control = 8L), info = tpl)
    expect_identical(den(denominator = "c(\"Drug X\" = 10, \"Control\" = 8)"),
                     c("Drug X" = 10L, Control = 8L), info = tpl)
    # another analysis set: the template makes its adsl_<id>, narrowed by subset
    x <- d2_expand(tpl, denominator = "adsl_itt", subset = "SEX == \"F\"")
    expect_identical(d2_ids(x)[1:2], c("adsl_saf", "adsl_itt"), info = tpl)
    expect_identical(d2_an(x, cat)$denominator, "adsl_itt", info = tpl)
    expect_identical(x$ard$analysis_data[[2L]]$population, "ITT", info = tpl)
    for (k in 1:2) {
      expect_match(as.character(x$ard$analysis_data[[k]]$where), "SEX", fixed = TRUE, info = tpl)
    }
    # (the set's own id is kept: it counts every subject of the set, cards' column
    # only those with a value)
    y <- d2_expand(tpl, denominator = "adsl_saf")
    expect_identical(d2_an(y, cat)$denominator, "adsl_saf", info = tpl)
    expect_identical(d2_ids(y), d2_ids(d2_expand(tpl)), info = tpl)
    # (a report's own analysis data: its `ard:` makes it)
    z <- d2_expand(tpl, denominator = "adsl_part")
    expect_identical(d2_ids(z), d2_ids(d2_expand(tpl)), info = tpl)
    expect_identical(d2_an(z, cat)$denominator, "adsl_part", info = tpl)
    # the header N is the study standard's: the same GROUPN
    expect_identical(d2_an(x, "GROUPN"), d2_an(d2_expand(tpl), "GROUPN"), info = tpl)
  }
})

test_that("summary on records: \"data\" stops until id exists; the default is the records", {
  expect_error(d2_expand("summary", data = "ADAE", categorical = "AESEV", denominator = "data"),
               paste0("summary: denominator = \"data\" on records (ADAE) is not supported yet: the counts ",
                      "are of records and \"data\" is a base of subjects, so a percentage could pass ",
                      "100%; leave it out (the records kept), or give a number, numbers named by the ",
                      "values of TRT01A, or an analysis data's id."), fixed = TRUE)
  # (the default is the records kept, passed explicitly: canon 1.38)
  r <- d2_expand("summary", data = "ADAE", categorical = "AESEV")
  expect_identical(d2_an(r, "CAT")$denominator, "adae_saf")
  expect_true(startsWith(d2_an(r, "CAT")$data, "adae_saf"))
})

# ---- summary family: refusals ----

test_that("summary family: \"data\" on ADSL, a stack by arm, no categorical", {
  for (tpl in c("summary", "disposition")) {
    expect_error(d2_expand(tpl, denominator = "data"),
                 sprintf(paste0("%s: denominator = \"data\" is for a `data` other than ADSL (the ",
                                "subjects with a record kept): ADSL is already one row a subject, the ",
                                "analysis set. Leave it out, or give a number, numbers named by the ",
                                "values of TRT01A, or an analysis data's id (adsl_saf counts every ",
                                "subject of the set)."), tpl), fixed = TRUE, info = tpl)
  }
  expect_error(tfl_template_expand("summary", list(population = "SAF", by = "TRT01A", continuous = "AGE",
                                                    denominator = 20)),
               paste0("summary: denominator is the base of the categorical variables' percentages, ",
                      "and the table has none: give `categorical`, or leave denominator out."),
               fixed = TRUE)
})

test_that("summary family: the checks of D2a's denominator", {
  for (tpl in c("summary", "disposition")) {
    expect_error(d2_expand(tpl, denominator = 0),
                 sprintf("%s: denominator = 0: a number is the subjects of every column: a positive whole number.",
                         tpl), fixed = TRUE, info = tpl)
    expect_error(d2_expand(tpl, denominator = c(10, 8)),
                 "several numbers are named by the values of TRT01A", fixed = TRUE, info = tpl)
    expect_error(d2_expand(tpl, denominator = c("Drug X" = 10)),
                 sprintf(paste0("%s: denominator has no number for \"Control\", a value of TRT01A: ",
                                "give one for each (Drug X, Control)."), tpl), fixed = TRUE, info = tpl)
    expect_error(d2_expand(tpl, denominator = c("Drug X" = 10, Control = 8, Placebo = 9)),
                 sprintf("%s: denominator names \"Placebo\", not a value of TRT01A (Drug X, Control).",
                         tpl), fixed = TRUE, info = tpl)
    expect_error(d2_expand(tpl, denominator = TRUE), d2_all_msg(tpl), fixed = TRUE, info = tpl)
    expect_error(d2_expand(tpl, denominator = "Adsl saf"), d2_all_msg(tpl), fixed = TRUE, info = tpl)
  }
})

# ---- ae_overview: expansions ----

test_that("ae_overview: the default is the analysis set; each form goes to every flag", {
  x <- d2_expand("ae_overview")
  # (passed explicitly, canon 1.38)
  expect_true(all(vapply(d2_flag_dens(x), identical, NA, "adsl_saf")))
  expect_identical(d2_expand("ae_overview", denominator = "adsl_saf"), x)
  expect_identical(d2_expand("ae_overview", denominator = NULL), x)
  a <- tfl_tpl_args("ae_overview")
  expect_identical(a$doc[a$param == "denominator"], tfl_tpl_args("ae_soc_pt")$doc[
    tfl_tpl_args("ae_soc_pt")$param == "denominator"])
  one <- d2_expand("ae_overview", denominator = 20)
  expect_true(all(vapply(d2_flag_dens(one), identical, NA, 20L)))
  expect_identical(d2_ids(one), d2_ids(x))
  expect_identical(d2_expand("ae_overview", denominator = "20"), one)
  v <- d2_expand("ae_overview", denominator = c("Drug X" = 10, Control = 8))
  expect_true(all(vapply(d2_flag_dens(v), identical, NA, c("Drug X" = 10L, Control = 8L))))
  # (the header N is the same GROUPN on the set)
  expect_identical(d2_an(one, "GROUPN"), d2_an(x, "GROUPN"))
  expect_identical(d2_an(v, "GROUPN"), d2_an(x, "GROUPN"))
  # another analysis set, narrowed by subset
  i <- d2_expand("ae_overview", denominator = "adsl_itt", subset = "SEX == \"F\"")
  expect_identical(d2_ids(i)[1:3], c("adsl_saf", "adsl_itt", "adae_saf"))
  expect_true(all(vapply(d2_flag_dens(i), identical, NA, "adsl_itt")))
  expect_identical(i$ard$analysis_data[[2L]]$population, "ITT")
  for (k in 1:2) expect_match(as.character(i$ard$analysis_data[[k]]$where), "SEX", fixed = TRUE)
  # a report's own analysis data
  p <- d2_expand("ae_overview", denominator = "adsl_part")
  expect_identical(d2_ids(p), d2_ids(x))
  expect_true(all(vapply(d2_flag_dens(p), identical, NA, "adsl_part")))
  # "data": the subjects with an event kept
  d <- d2_expand("ae_overview", denominator = "data")
  expect_identical(d2_ids(d), c("adsl_saf", "adae_saf", "adae_saf_den"))
  expect_identical(as.character(d$ard$analysis_data[[3L]]$code),
                   "dplyr::distinct(adae_saf, USUBJID, TRT01A)")
  expect_true(all(vapply(d2_flag_dens(d), identical, NA, "adae_saf_den")))
})

test_that("ae_overview: header_n \"denominator\" counts the arms' N from the data of the percentages", {
  gn <- function(...) d2_an(d2_expand("ae_overview", ...), "GROUPN")$data
  # (header_n \"set\", the default: the analysis set whatever the denominator is)
  for (v in list(NULL, 20, c("Drug X" = 10, Control = 8), "adsl_itt", "data")) {
    expect_identical(gn(denominator = v), "adsl_saf")
    expect_identical(gn(denominator = v, header_n = "set"), "adsl_saf")
  }
  expect_identical(gn(header_n = "denominator"), "adsl_saf")
  expect_identical(gn(denominator = "adsl_saf", header_n = "denominator"), "adsl_saf")
  expect_identical(gn(denominator = "adsl_itt", header_n = "denominator"), "adsl_itt")
  expect_identical(gn(denominator = "adsl_part", header_n = "denominator"), "adsl_part")
  expect_identical(gn(denominator = "data", header_n = "denominator"), "adae_saf_den")
  # "none": no N, the denominator as it is
  n <- d2_expand("ae_overview", denominator = "data", header_n = "none")
  expect_false("GROUPN" %in% vapply(n$ard$analyses, function(a) a$id, ""))
  expect_true(all(vapply(d2_flag_dens(d2_expand("ae_overview", denominator = 20, header_n = "none")),
                         identical, NA, 20L)))
  # a number has no data to count from
  for (v in list(20, c("Drug X" = 10, Control = 8))) {
    expect_error(d2_expand("ae_overview", denominator = v, header_n = "denominator"),
                 paste0("ae_overview: header_n = \"denominator\" with a denominator that is a number ",
                        "has no data to count the arms' N from: use header_n = \"set\" or \"none\", ",
                        "or give the denominator as an analysis data."), fixed = TRUE)
  }
  ch <- tfl_tpl_args("ae_overview")
  expect_match(ch$doc[ch$param == "header_n"], "not with a number", fixed = TRUE)
})

test_that("ae_overview: the checks of D2a's denominator", {
  expect_error(d2_expand("ae_overview", denominator = -2),
               "ae_overview: denominator = -2: a number is the subjects of every column: a positive whole number.",
               fixed = TRUE)
  expect_error(d2_expand("ae_overview", denominator = c("Drug X" = 10)),
               paste0("ae_overview: denominator has no number for \"Control\", a value of TRT01A: ",
                      "give one for each (Drug X, Control)."), fixed = TRUE)
  expect_error(d2_expand("ae_overview", denominator = FALSE), d2_all_msg("ae_overview"), fixed = TRUE)
})

# ---- orr: no denominator (the rate row has none) ----

test_that("orr has no denominator parameter", {
  expect_false("denominator" %in% tfl_tpl_args("orr")$param)
  expect_error(d2_expand("orr", denominator = "adsl_itt"), "no parameter 'denominator'", fixed = TRUE)
})

# ---- a number with a Total column ----

test_that("a single number with total = TRUE stops; numbers by arm and a data id do not", {
  hier <- list(hierarchy = c("AEBODSYS", "AEDECOD"))
  for (tpl in c("summary", "hierarchical", "ae_soc_pt", "ae_ser_soc_pt",
                "ae_disc_soc_pt")) {
    a <- c(list(population = "SAF", total = TRUE),
           if (tpl == "summary") list(by = "TRT01A", categorical = "SEX"),
           if (tpl == "hierarchical") hier)
    ex <- function(den) tfl_template_expand(tpl, c(a, list(denominator = den)), codelists = d2_codelists,
                                            population = d2_pops)
    for (v in list(20, "20")) {
      expect_error(ex(v), sprintf(paste0("%s: denominator = 20 with total = TRUE: one number is each ",
                                         "arm's N and would be the Total column's too; give numbers ",
                                         "named by the values of TRT01A (the Total column is their sum), ",
                                         "or an analysis data's id."), tpl), fixed = TRUE, info = tpl)
    }
    expect_silent(ex(c("Drug X" = 10, Control = 8)))
    expect_silent(ex("adsl_itt"))
    # (without a Total column the number is fine)
    if (tpl %in% c("summary", "hierarchical", "ae_soc_pt")) {
      a$total <- FALSE
      expect_silent(ex(20))
    }
  }
})

# ---- a report: the unknown data id ----

d2_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  # 12 subjects of DMY-001: 6 an arm; the safety set leaves out 6 and 12
  id <- sprintf("DMY-001-%03d", 1:12)
  sl0 <- tflspec::adsl
  sl <- rbind(sl0[sl0$TRT01A == "Drug X", ][1:6, ], sl0[sl0$TRT01A == "Control", ][1:6, ])
  sl$USUBJID <- id
  sl$TRT01A <- sl$TRT01P <- rep(c("Drug X", "Control"), each = 6)
  sl$SAFFL <- ifelse(seq_len(12) %in% c(6, 12), "N", "Y")
  sl$ITTFL <- "Y"
  sl$SEX <- ifelse(seq_len(12) %in% c(1, 2, 4, 7, 9, 10, 11), "F", "M")
  # the status at the end of the study: subjects 3 and 8 have none
  sl$EOSSTT <- c("COMPLETED", "COMPLETED", NA, "DISCONTINUED", "COMPLETED", "COMPLETED",
                 "DISCONTINUED", NA, "COMPLETED", "COMPLETED", "DISCONTINUED", "COMPLETED")
  # treatment-emergent events (subjects 6 and 12 are outside the safety set):
  # 1 headache (mild), nausea (moderate, serious); 2 headache (mild, then severe);
  # 3 nausea (mild); 6 headache; 7 headache (moderate, serious); 8 nausea (severe),
  # vomiting (mild); 12 nausea
  ae <- data.frame(
    n = c(1, 1, 2, 2, 3, 6, 7, 8, 8, 12),
    pt = c("Headache", "Nausea", "Headache", "Headache", "Nausea", "Headache", "Headache",
           "Nausea", "Vomiting", "Nausea"),
    sev = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD", "MILD", "MODERATE", "SEVERE", "MILD",
            "MILD"),
    ser = c("N", "Y", "N", "N", "N", "N", "Y", "N", "N", "N"),
    stringsAsFactors = FALSE)
  a0 <- tflspec::adae[1L, ]
  a0 <- a0[rep(1L, nrow(ae)), ]
  a0$USUBJID <- id[ae$n]
  a0$TRT01A <- sl$TRT01A[ae$n]
  a0$AEDECOD <- ae$pt
  a0$AESEV <- ae$sev
  a0$AESER <- ae$ser
  a0$TRTEMFL <- "Y"
  a0$SAFFL <- sl$SAFFL[ae$n]
  a0 <- a0[setdiff(names(a0), "TRT01AN")]
  # the best overall response (ITT set; subjects 5, 6 and 12 have no record)
  bor <- data.frame(n = c(1:4, 7:11),
                    resp = c("CR", "PR", "SD", "PD", "PR", "CR", "PD", "PR", "PD"),
                    stringsAsFactors = FALSE)
  r0 <- tflspec::adrs[tflspec::adrs$PARAMCD == "BOR", ][1L, ]
  r0 <- r0[rep(1L, nrow(bor)), ]
  r0$USUBJID <- id[bor$n]
  r0$TRT01A <- r0$TRT01P <- sl$TRT01A[bor$n]
  r0$ITTFL <- "Y"
  r0$SAFFL <- sl$SAFFL[bor$n]
  r0$AVALC <- bor$resp
  r0 <- r0[setdiff(names(r0), c("TRT01AN", "TRT01PN"))]
  for (x in list(list("adsl", sl), list("adae", a0), list("adrs", r0))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "DMY-001")
  list(dir = d, adsl = sl, adae = a0, adrs = r0)
}
d2_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
}

# a report of a template: its ARD (as a data frame), the header's text and the program
d2_report <- function(s, name, args, own = NULL) {
  f <- file.path(s$dir, "spec", "outputs", "T-DV.yml")
  pop <- args$population %||% "SAF"
  args$population <- NULL
  # (`by` is required, no default; disposition's status variable too)
  if (name %in% c("summary", "disposition", "ae_overview") && is.null(args$by)) args$by <- "TRT01A"
  if (name == "disposition" && is.null(args$categorical)) args$categorical <- "EOSSTT"
  tfl_write_output(tfl_output("T-DV", "table", population = pop, extends = "defaults",
                              template = list(name = name, args = c(
                                # (demog, summary and disposition take the analysis set from the
                                # report's `population:`)
                                if (!name %in% c("demog", "stf_t02_demog", "summary", "disposition")) {
                                  list(population = pop)
                                }, args)),
                              ard = own), f)
  st <- tfl_read_study_spec(s$dir)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-DV", dir = s$dir, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  pg <- suppressMessages(rtfreporter::plan_apply(p, "pages"))
  hdr <- vapply(unlist(pg[[1L]]$col_header, recursive = FALSE), function(z) {
    paste(as.character(unlist(z)), collapse = "")
  }, "")
  list(ard = as.data.frame(ard), header = gsub("\n", " ", hdr, fixed = TRUE),
       code = paste(suppressMessages(tfl_ard_code(st, output_id = "T-DV")), collapse = "\n"))
}

# the n, N and p the ARD gives a level of a variable in an arm (NA: not a statistic of it)
d2_cells <- function(ard, var, level, arm, analysis = NULL) {
  chr1 <- function(z) vapply(z, function(v) paste(as.character(unlist(v)), collapse = "|"), "")
  k <- ard$variable == var & chr1(ard$variable_level) == level & chr1(ard$group1_level) == arm
  if (!is.null(analysis)) k <- k & ard$analysis_id == analysis
  a <- ard[k, ]
  vapply(c(n = "n", N = "N", p = "p"), function(s) {
    i <- match(s, a$stat_name)
    if (is.na(i)) NA_real_ else as.numeric(unlist(a$stat[[i]]))
  }, 0)
}

test_that("an analysis data the report does not have stops, naming what it has", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  st <- tfl_read_study_spec(s$dir)
  have <- list(summary = "adsl_saf, adsl_saf_m", ae_overview = "adsl_saf, adae_saf")
  for (tpl in names(have)) {
    o <- tfl_output("T-DV", "table", population = "SAF", extends = "defaults",
                    template = list(name = tpl, args = c(
                      if (tpl != "summary") list(population = "SAF"),
                      list(by = "TRT01A", denominator = "adsl_zzz"),
                      if (tpl == "summary") list(categorical = "SEX"))))
    expect_error(tfl_resolve_output(o, st),
                 sprintf(paste0("T-DV: template %s: denominator \"adsl_zzz\" is not an analysis data ",
                                "of the report (%s). The template makes adsl_saf and the other ",
                                "analysis sets' adsl_<id>; the report's own come from its `ard:`."),
                         tpl, have[[tpl]]), fixed = TRUE, info = tpl)
    # (a number, an analysis set of the study, the records' subjects: no complaint)
    for (v in list(5, "adsl_itt", if (tpl == "ae_overview") "data")) {
      if (is.null(v)) next
      o$template$args$denominator <- v
      expect_no_warning(tfl_resolve_output(o, st))
    }
  }
})

# ---- the numbers, by hand, from the data ----

d2_by_hand <- function(s) {
  sl <- s$adsl
  saf <- sl[sl$SAFFL == "Y", ]
  arms <- c("Drug X", "Control")
  ev <- s$adae[s$adae$USUBJID %in% saf$USUBJID, ]
  list(arms = arms, saf = saf, ev = ev,
       set = vapply(arms, function(a) sum(saf$TRT01A == a), 0L),
       itt = vapply(arms, function(a) sum(sl$TRT01A == a), 0L),
       tested = vapply(arms, function(a) length(unique(ev$USUBJID[ev$TRT01A == a])), 0L),
       # women of the safety set, a report's own analysis data (adsl_female)
       fem = vapply(arms, function(a) sum(saf$TRT01A == a & saf$SEX == "F"), 0L))
}
d2_own <- list(analysis_data = list(list(
  id = "adsl_female", label = "Safety set, women", from = "ADSL", population = "SAF",
  where = tfl_r("SEX == \"F\""))))

test_that("hand counts of the fixture: the checks below rest on them", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  h <- d2_by_hand(s)
  expect_identical(unname(h$set), c(5L, 5L))
  expect_identical(unname(h$itt), c(6L, 6L))
  expect_identical(unname(h$tested), c(3L, 2L))
  expect_identical(unname(h$fem), c(3L, 4L))
  # the status at the end of the study: a value for 4 of the 5 in each arm of the safety set
  expect_identical(unname(vapply(h$arms, function(a) sum(!is.na(h$saf$EOSSTT[h$saf$TRT01A == a])), 0L)),
                   c(4L, 4L))
  # the events kept, a record each: 5 and 3
  expect_identical(unname(vapply(h$arms, function(a) sum(h$ev$TRT01A == a), 0L)), c(5L, 3L))
})

# the denominators of each form: the N of an arm
d2_forms <- function(h, records = FALSE) {
  c(list(
    set = list(den = NULL, N = h$set),
    one = list(den = 20, N = c("Drug X" = 20L, Control = 20L)),
    vec = list(den = c("Drug X" = 7, Control = 8), N = c("Drug X" = 7L, Control = 8L)),
    itt = list(den = "adsl_itt", N = h$itt),
    own = list(den = "adsl_female", N = h$fem)),
    if (records) list(data = list(den = "data", N = h$tested)))
}

test_that("summary, demog on ADSL: n (%) of each denominator, the header's N not moved", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  h <- d2_by_hand(s)
  forms <- d2_forms(h)
  forms$set <- NULL
  nsex <- function(a, v) sum(h$saf$TRT01A == a & h$saf$SEX == v)
  for (tpl in "summary") {
    for (nm in names(forms)) {
      f <- forms[[nm]]
      a <- c(list(denominator = f$den), if (tpl == "summary") list(categorical = "SEX"),
             if (tpl == "demog") list(by = "TRT01A", categorical = "SEX", continuous = "AGE"))
      r <- d2_report(s, tpl, a, own = if (nm == "own") d2_own)
      info <- paste(tpl, nm)
      for (arm in h$arms) {
        # (the header's N: the set's, whatever the denominator)
        expect_equal(d2_cells(r$ard, "TRT01A", arm, "", analysis = "GROUPN")[["n"]], h$set[[arm]],
                     info = paste(info, arm))
        for (v in c("F", "M")) {
          c1 <- d2_cells(r$ard, "SEX", v, arm)
          expect_equal(c1[["n"]], nsex(arm, v), info = paste(info, arm, v))
          expect_equal(c1[["p"]], nsex(arm, v) / f$N[[arm]], info = paste(info, arm, v))
        }
      }
    }
  }
})

test_that("summary, disposition on ADSL: the analysis set counts the subjects with no value (a Missing row)", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  h <- d2_by_hand(s)
  nst <- function(a, v) sum(h$saf$TRT01A == a & h$saf$EOSSTT %in% v)
  # (Drug X 3 COMPLETED of 4 with a value, of 5 in the set; Control 2 of 4, of 5)
  for (tpl in c("disposition", "summary")) {
    base <- if (tpl == "summary") list(categorical = "EOSSTT") else list()
    cat <- if (tpl == "summary") "CAT" else "DISP"
    # (the default: the analysis set, canon 1.38 -- no longer cards' column)
    forms <- c(list(default = list(den = NULL, N = h$set)),
               d2_forms(h)[c("one", "vec", "itt", "own")],
               list(setid = list(den = "adsl_saf", N = h$set)))
    for (nm in names(forms)) {
      f <- forms[[nm]]
      r <- d2_report(s, tpl, c(base, if (!is.null(f$den)) list(denominator = f$den)),
                     own = if (nm == "own") d2_own)
      for (arm in h$arms) {
        for (v in c("COMPLETED", "DISCONTINUED")) {
          c1 <- d2_cells(r$ard, "EOSSTT", v, arm, analysis = cat)
          expect_equal(c1[["n"]], nst(arm, v), info = paste(tpl, nm, arm, v))
          expect_equal(c1[["p"]], nst(arm, v) / f$N[[arm]], info = paste(tpl, nm, arm, v))
        }
      }
    }
  }
  # the default is the set's own id: the subject with no status is in a Missing row
  # (missing_row, canon 1.31, row 361), so the percentages add up to 100
  r1 <- d2_report(s, "disposition", list())
  r2 <- d2_report(s, "disposition", list(denominator = "adsl_saf"))
  expect_equal(d2_cells(r1$ard, "EOSSTT", "COMPLETED", "Drug X")[["p"]], 3 / 5)
  expect_equal(d2_cells(r2$ard, "EOSSTT", "COMPLETED", "Drug X")[["p"]], 3 / 5)
  expect_equal(d2_cells(r1$ard, "EOSSTT", "Missing", "Drug X")[["n"]], 1)
})

test_that("summary on records: n of the records over each denominator", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  h <- d2_by_hand(s)
  forms <- d2_forms(h)
  # (the default: the records, 5 and 3)
  forms$set <- list(den = NULL, N = c("Drug X" = 5L, Control = 3L))
  nsev <- function(a, v) sum(h$ev$TRT01A == a & h$ev$AESEV == v)
  for (nm in names(forms)) {
    f <- forms[[nm]]
    r <- d2_report(s, "summary", c(list(data = "ADAE", categorical = "AESEV", where = "TRTEMFL == \"Y\""),
                                   if (!is.null(f$den)) list(denominator = f$den)),
                   own = if (nm == "own") d2_own)
    for (arm in h$arms) {
      expect_equal(d2_cells(r$ard, "TRT01A", arm, "", analysis = "GROUPN")[["n"]], h$set[[arm]],
                   info = paste(nm, arm))
      for (v in c("MILD", "MODERATE", "SEVERE")) {
        c1 <- d2_cells(r$ard, "AESEV", v, arm)
        expect_equal(c1[["n"]], nsev(arm, v), info = paste(nm, arm, v))
        expect_equal(c1[["p"]], nsev(arm, v) / f$N[[arm]], info = paste(nm, arm, v))
      }
    }
  }
})

test_that("a Total column takes a number and an analysis data", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  h <- d2_by_hand(s)
  r <- d2_report(s, "summary", list(categorical = "SEX", denominator = "adsl_itt"))
  for (arm in h$arms) {
    c1 <- d2_cells(r$ard, "SEX", "F", arm)
    n <- sum(h$saf$TRT01A == arm & h$saf$SEX == "F")
    expect_equal(c(c1[["n"]], c1[["p"]]), c(n, n / 6), info = arm)
  }
  r <- d2_report(s, "summary", list(categorical = "SEX", denominator = 20))
  expect_equal(d2_cells(r$ard, "SEX", "M", "Control")[["p"]], 1 / 20)
  # a Total column: the total's base is the numbers' sum (a vector), as cards counts it
  r <- d2_report(s, "summary", list(categorical = "SEX", total = TRUE,
                                    denominator = c("Drug X" = 7, Control = 8)))
  expect_equal(d2_cells(r$ard, "SEX", "F", "Drug X")[["p"]], 3 / 7)
  tot <- r$ard[r$ard$variable == "SEX" & r$ard$stat_name == "p" &
                 vapply(r$ard$variable_level, function(z) identical(unlist(z), "F"), NA) &
                 vapply(r$ard$group1_level, is.null, NA), ]
  expect_equal(as.numeric(unlist(tot$stat)), 7 / 15)
})

test_that("ae_overview: n (%) of each denominator, and the header's N", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  h <- d2_by_hand(s)
  forms <- d2_forms(h, records = TRUE)
  ser <- vapply(h$arms, function(a) length(unique(h$ev$USUBJID[h$ev$TRT01A == a & h$ev$AESER == "Y"])), 0L)
  expect_identical(unname(ser), c(1L, 1L))
  for (nm in names(forms)) {
    f <- forms[[nm]]
    # (header_n "set", the default: the analysis set's N)
    r <- d2_report(s, "ae_overview", if (!is.null(f$den)) list(denominator = f$den),
                   own = if (nm == "own") d2_own)
    for (arm in h$arms) {
      expect_true(any(grepl(sprintf("(N=%d)", h$set[[arm]]), r$header, fixed = TRUE)),
                  info = paste(nm, arm, paste(r$header, collapse = " | ")))
      c1 <- d2_cells(r$ard, "ANY", "TRUE", arm)
      expect_equal(c1[["n"]], h$tested[[arm]], info = paste(nm, arm, "any"))
      expect_equal(c1[["N"]], f$N[[arm]], info = paste(nm, arm, "any"))
      expect_equal(c1[["p"]], h$tested[[arm]] / f$N[[arm]], info = paste(nm, arm, "any"))
      c2 <- d2_cells(r$ard, "SER", "TRUE", arm)
      expect_equal(c2[["n"]], ser[[arm]], info = paste(nm, arm, "serious"))
      expect_equal(c2[["p"]], ser[[arm]] / f$N[[arm]], info = paste(nm, arm, "serious"))
    }
    # header_n = "denominator": the N of the data the percentages are of (not a number)
    if (is.numeric(f$den)) next
    r <- d2_report(s, "ae_overview", c(if (!is.null(f$den)) list(denominator = f$den),
                                       list(header_n = "denominator")),
                   own = if (nm == "own") d2_own)
    for (arm in h$arms) {
      expect_true(any(grepl(sprintf("(N=%d)", f$N[[arm]]), r$header, fixed = TRUE)),
                  info = paste(nm, arm, "denominator", paste(r$header, collapse = " | ")))
    }
    # (the header's N is the percentages' N)
    for (arm in h$arms) {
      expect_equal(d2_cells(r$ard, "ANY", "TRUE", arm)[["N"]],
                   d2_cells(r$ard, "TRT01A", arm, "", analysis = "GROUPN")[["n"]],
                   info = paste(nm, arm))
    }
  }
  # a subset narrows the analysis sets the denominator reads
  r <- d2_report(s, "ae_overview", list(denominator = "adsl_itt", subset = "SEX == \"F\"",
                                        header_n = "denominator"))
  # (women of the ITT set: Drug X 1, 2, 4; Control 7, 9, 10, 11)
  expect_true(any(grepl("(N=3)", r$header, fixed = TRUE)))
  expect_true(any(grepl("(N=4)", r$header, fixed = TRUE)))
  expect_equal(d2_cells(r$ard, "ANY", "TRUE", "Drug X")[["n"]], 2)
  expect_equal(d2_cells(r$ard, "ANY", "TRUE", "Drug X")[["p"]], 2 / 3)
  expect_equal(d2_cells(r$ard, "ANY", "TRUE", "Control")[["p"]], 1 / 4)
})

test_that("the program: a number, a named vector, a data id, the records' subjects", {
  d2_skip()
  s <- d2_study(withr::local_tempdir())
  vec <- c("Drug X" = 7, Control = 8)
  code <- function(tpl, den, ...) d2_report(s, tpl, list(denominator = den, ...))$code
  for (tpl in list(list("summary", list(categorical = "SEX")), list("ae_overview", list()))) {
    c1 <- function(den) do.call(code, c(list(tpl[[1L]], den), tpl[[2L]]))
    expect_match(c1(20), "denominator = 20L", fixed = TRUE, info = tpl[[1L]])
    expect_match(c1(vec), paste0("denominator = data.frame(TRT01A = rep(c(\"Drug X\", \"Control\"), ",
                                 "c(7L, 8L)), USUBJID = seq_len(15L))"), fixed = TRUE, info = tpl[[1L]])
    expect_match(c1("adsl_itt"), "denominator = adsl_itt", fixed = TRUE, info = tpl[[1L]])
    expect_match(c1("adsl_itt"), "adsl_itt <- filter(adsl, ITTFL == \"Y\")", fixed = TRUE,
                 info = tpl[[1L]])
  }
  expect_match(code("ae_overview", "data"), "adae_saf_den <- distinct(adae_saf, USUBJID, TRT01A)",
               fixed = TRUE)
  expect_match(code("ae_overview", "data"), "denominator = adae_saf_den", fixed = TRUE)
  # (no denominator: the analysis set's, written in the program all the same, canon 1.38)
  body <- function(x) substring(x, regexpr("# ---- analysis data ----", x, fixed = TRUE))
  expect_match(body(d2_report(s, "ae_overview", list())$code), "denominator = adsl_saf", fixed = TRUE)
  expect_match(body(d2_report(s, "summary", list(categorical = "SEX"))$code), "denominator = adsl_saf",
               fixed = TRUE)
})
