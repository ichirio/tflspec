# header_n on the counting templates: the arm headings' N, set apart from the
# percentages' denominator (D1: where each value already works)

# the template's items: the header texts, the analyses' ids
hn_texts <- function(x) {
  it <- Filter(function(i) identical(i$fn, "plan_col_header"), x$content)
  lapply(it[[1L]]$args$header, function(h) unlist(h$text))
}
hn_all <- function(x) unlist(hn_texts(x))
hn_ids <- function(x) vapply(x$ard$analyses, function(a) a$id, "")
hn_expand <- function(name, args = list(), ...) {
  # (ae_overview's `by` is required, no default: canon 1.14)
  if (name == "ae_overview" && is.null(args$by)) args$by <- "TRT01A"
  tfl_template_expand(name, c(list(population = "SAF"), args, list(...)))
}
hn_msg <- function(tpl, x, why) {
  sprintf("%s: header_n = \"%s\" %s", tpl, x, why)
}
hn_den_why <- paste0("differs from the percentages' denominator; a header N apart from it ",
                     "is not supported yet: use header_n = \"denominator\" or \"none\".")

# ---- the stack templates: the denominator is the header N ----

hn_stack <- list(
  hierarchical = list(hierarchy = c("AEBODSYS", "AEDECOD")),
  ae_soc_pt = list(), ae_ser_soc_pt = list(), ae_disc_soc_pt = list(),
  ae_pt_freq = list(), ae_max_grade = list(),
  ae_max_grade_cols = list(grade_in = "cols"))

test_that("stack templates: the default is today's expansion; \"denominator\" and \"set\" are it", {
  for (nm in names(hn_stack)) {
    tpl <- sub("_cols$", "", nm)
    x <- hn_expand(tpl, hn_stack[[nm]])
    expect_match(paste(hn_all(x), collapse = "|"), "(N={n", fixed = TRUE, info = nm)
    expect_identical(hn_expand(tpl, hn_stack[[nm]], header_n = "denominator"), x, info = nm)
    expect_identical(hn_expand(tpl, hn_stack[[nm]], header_n = "set"), x, info = nm)
  }
  # (the text as it was)
  expect_identical(hn_texts(hn_expand("ae_soc_pt"))[[2L]], "{col}\n(N={n})\nn (%)")
  expect_identical(hn_texts(hn_expand("ae_pt_freq"))[[2L]], "{col}\n(N={n})\nn (%)")
  expect_identical(hn_texts(hn_expand("ae_max_grade"))[[2L]], "{col}\n(N={n})\nn (%)")
  expect_identical(hn_texts(hn_expand("ae_max_grade", grade_in = "cols"))[[2L]],
                   "{col}\n(N={n1})\nn (%)")
})

test_that("stack templates: \"none\" is the header text without the N, the ARD as it was", {
  for (nm in names(hn_stack)) {
    tpl <- sub("_cols$", "", nm)
    x <- hn_expand(tpl, hn_stack[[nm]])
    y <- hn_expand(tpl, hn_stack[[nm]], header_n = "none")
    expect_identical(y$ard, x$ard, info = nm)
    tx <- hn_texts(y)
    expect_false(any(grepl("(N=", unlist(tx), fixed = TRUE)), info = nm)
    expect_identical(tx[[2L]], "{col}\nn (%)", info = nm)
    # (the rest of the header, and the other items)
    expect_identical(tx[-2L], hn_texts(x)[-2L], info = nm)
    keep <- function(z) Filter(function(i) !identical(i$fn, "plan_col_header"), z$content)
    expect_identical(keep(y), keep(x), info = nm)
  }
})

test_that("stack templates: \"tested\" is the denominator only with denominator = \"data\"", {
  for (tpl in c("hierarchical", "ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt")) {
    a <- if (tpl == "hierarchical") hn_stack$hierarchical else list()
    x <- hn_expand(tpl, a, denominator = "data")
    expect_identical(hn_expand(tpl, a, denominator = "data", header_n = "tested"), x, info = tpl)
    expect_identical(hn_expand(tpl, a, denominator = "data", header_n = "denominator"), x)
    # the analysis set is not the denominator then, nor a number or a vector
    expect_error(hn_expand(tpl, a, denominator = "data", header_n = "set"),
                 hn_msg(tpl, "set", hn_den_why), fixed = TRUE)
    expect_error(hn_expand(tpl, a, header_n = "tested"),
                 hn_msg(tpl, "tested", hn_den_why), fixed = TRUE)
    expect_error(hn_expand(tpl, a, denominator = "50", header_n = "set"),
                 hn_msg(tpl, "set", hn_den_why), fixed = TRUE)
    expect_error(hn_expand(tpl, a, denominator = "50", header_n = "tested"),
                 hn_msg(tpl, "tested", hn_den_why), fixed = TRUE)
    expect_identical(hn_texts(hn_expand(tpl, a, denominator = "data", header_n = "none"))[[2L]],
                     "{col}\nn (%)")
  }
  # (without a denominator parameter, the set is the denominator)
  expect_error(hn_expand("ae_pt_freq", header_n = "tested"),
               hn_msg("ae_pt_freq", "tested", hn_den_why), fixed = TRUE)
  expect_error(hn_expand("ae_max_grade", header_n = "tested"),
               hn_msg("ae_max_grade", "tested", hn_den_why), fixed = TRUE)
  expect_error(hn_expand("ae_max_grade", grade_in = "cols", header_n = "tested"),
               hn_msg("ae_max_grade", "tested", hn_den_why), fixed = TRUE)
  expect_error(hn_expand("ae_soc_pt", header_n = "nothing"), "header_n takes", fixed = TRUE)
})

test_that("stack templates: header_n is a parameter with its choices", {
  ch <- function(nm) tfl_tpl_args(nm)$doc[tfl_tpl_args(nm)$param == "header_n"]
  for (nm in c("hierarchical", "ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt", "ae_pt_freq",
               "ae_max_grade", "lab_abnormal", "orr", "ae_overview")) {
    expect_true("header_n" %in% tfl_tpl_args(nm)$param, info = nm)
    expect_match(ch(nm), "none", info = nm)
  }
})

# ---- lab_abnormal ----

test_that("lab_abnormal: header_n \"set\" is today's; \"tested\" counts the records' subjects; \"none\"", {
  a <- list(levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\"", param_order = NULL)
  x <- hn_expand("lab_abnormal", a)
  expect_identical(hn_expand("lab_abnormal", a, header_n = "set"), x)
  expect_identical(hn_ids(x), c("GROUPN", "ABNORMAL"))
  expect_identical(x$ard$analyses[[1L]]$data, "adsl_sub")
  expect_identical(hn_texts(x)[[2L]], "{col}\n(N={n})")
  # tested: a GROUPN on the distinct subjects of the records kept
  y <- hn_expand("lab_abnormal", a, header_n = "tested")
  ids <- vapply(y$ard$analysis_data, function(d) d$id, "")
  expect_identical(ids, c("adsl_sub", "adlb_sub", "adlb_n", "adlb_tested", "adlb_cat"))
  d <- y$ard$analysis_data[[4L]]
  expect_identical(d$from, "adlb_sub")
  expect_identical(d$code, "dplyr::distinct(adlb_sub, USUBJID, TRT01A)")
  expect_identical(hn_ids(y), c("GROUPN", "ABNORMAL"))
  expect_identical(y$ard$analyses[[1L]]$data, "adlb_tested")
  expect_identical(y$ard$analyses[[2L]], x$ard$analyses[[2L]])
  expect_identical(hn_texts(y), hn_texts(x))
  # none
  z <- hn_expand("lab_abnormal", a, header_n = "none")
  expect_identical(hn_ids(z), "ABNORMAL")
  expect_identical(hn_texts(z)[[2L]], "{col}")
  expect_identical(z$ard$analysis_data, x$ard$analysis_data)
  # the denominator differs by parameter: not one N
  expect_error(hn_expand("lab_abnormal", a, header_n = "denominator"),
               hn_msg("lab_abnormal", "denominator",
                      paste0("is not one N: the percentages' denominator differs by parameter; ",
                             "use header_n = \"set\", \"tested\" or \"none\".")), fixed = TRUE)
})

# a study of 25 Drug X subjects: ALT for subjects 1-20, HGB for 16-23, and an
# ALT record before treatment (ONTRTFL "N") for subject 24: 23 subjects have an
# on-treatment record, 24 a record of either parameter
hn_lab_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- tflspec::adsl
  sl <- sl[sl$SAFFL == "Y" & sl$TRT01A == "Drug X", ][1:25, ]
  lb0 <- tflspec::adlb
  alt <- lb0[lb0$PARAMCD == "ALT", ][1L, ]
  hgb <- lb0[lb0$PARAMCD == "HGB", ][1L, ]
  alt <- alt[rep(1L, 21L), ]
  alt$USUBJID <- sl$USUBJID[c(1:20, 24)]
  alt$ONTRTFL <- c(rep("Y", 20), "N")
  hgb <- hgb[rep(1L, 8L), ]
  hgb$USUBJID <- sl$USUBJID[16:23]
  hgb$ONTRTFL <- "Y"
  lb <- rbind(alt, hgb)
  lb$ANRIND <- rep(c("LOW", "NORMAL", "HIGH"), length.out = nrow(lb))
  for (x in list(list("adsl", sl), list("adlb", lb))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "HN-1")
  d
}

# a study's report from a template: its ARD, the header's text
hn_report <- function(d, name, args) {
  f <- file.path(d, "spec", "outputs", "T-HN.yml")
  tfl_write_output(tfl_output("T-HN", "table", common = list(population = "SAF"), extends = "defaults",
                              template = list(name = name, args = args)), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-HN", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  pg <- suppressMessages(rtfreporter::plan_apply(p, "pages"))
  hdr <- as.character(unlist(pg[[1L]]$col_header))
  list(ard = as.data.frame(ard), header = hdr, plan = p)
}
hn_groupn <- function(ard) {
  g <- ard[ard$analysis_id == "GROUPN" & ard$stat_name == "n", ]
  stats::setNames(as.numeric(unlist(g$stat)), unlist(g$group1_level %||% g$variable_level))
}
hn_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
}

test_that("lab_abnormal: the header N is the set's, or the records' subjects (\"tested\"), or none", {
  hn_skip()
  d <- hn_lab_study(withr::local_tempdir())
  a <- list(population = "SAF", levels = c("LOW", "HIGH"), param_order = NULL,
            params = c("ALT", "HGB"))
  has_n <- function(h, n) any(grepl(sprintf("(N=%d)", n), h, fixed = TRUE))
  # set: the arm's 25 subjects
  s <- hn_report(d, "lab_abnormal", a)
  expect_true(has_n(s$header, 25L))
  expect_equal(unname(hn_groupn(s$ard)), 25)
  # tested: the 24 subjects with a record of ALT or HGB
  t <- hn_report(d, "lab_abnormal", c(a, list(header_n = "tested")))
  expect_true(has_n(t$header, 24L))
  expect_equal(unname(hn_groupn(t$ard)), 24)
  # (the records kept: the ones on treatment, after `where`)
  w <- hn_report(d, "lab_abnormal", c(a, list(header_n = "tested", where = "ONTRTFL == \"Y\"")))
  expect_true(has_n(w$header, 23L))
  expect_equal(unname(hn_groupn(w$ard)), 23)
  p <- hn_report(d, "lab_abnormal", c(a[names(a) != "params"], list(header_n = "tested", params = "HGB")))
  expect_true(has_n(p$header, 8L))
  expect_equal(unname(hn_groupn(p$ard)), 8)
  # none
  n <- hn_report(d, "lab_abnormal", c(a, list(header_n = "none")))
  expect_false(any(grepl("N=", n$header, fixed = TRUE)))
  expect_false("GROUPN" %in% n$ard$analysis_id)
  # the cells are the same whatever the header
  cell <- function(r) {
    x <- r$ard[r$ard$analysis_id == "ABNORMAL", c("stat_name", "stat")]
    rownames(x) <- NULL
    x
  }
  expect_identical(cell(t), cell(s))
  expect_identical(cell(n), cell(s))
})

# ---- orr ----

test_that("orr: an explicit GROUPN on the analysis set; \"none\"; \"tested\" stops", {
  for (lay in c("rows", "cols")) {
    x <- hn_expand("orr", list(), stat_layout = lay)
    expect_identical(hn_ids(x), c("BOR", "ORR", "GROUPN"), info = lay)
    g <- x$ard$analyses[[3L]]
    expect_identical(g$data, "adsl_sub", info = lay)
    expect_identical(g$variables, "TRT01P", info = lay)
    expect_identical(hn_expand("orr", list(), stat_layout = lay, header_n = "set"), x)
    expect_identical(hn_expand("orr", list(), stat_layout = lay, header_n = "denominator"), x)
    n <- hn_expand("orr", list(), stat_layout = lay, header_n = "none")
    expect_identical(hn_ids(n), c("BOR", "ORR"), info = lay)
    expect_false(any(grepl("N=", hn_all(n), fixed = TRUE)), info = lay)
    expect_error(hn_expand("orr", list(), stat_layout = lay, header_n = "tested"),
                 hn_msg("orr", "tested", paste0("is not supported: the rate row's base is the ",
                          "analysis set; use header_n = \"set\" or \"none\".")), fixed = TRUE)
  }
  expect_identical(hn_texts(hn_expand("orr"))[[3L]], "(N={n})")
  expect_identical(hn_texts(hn_expand("orr", stat_layout = "cols"))[[1L]], "{col1} (N={n1})")
  expect_identical(hn_texts(hn_expand("orr", stat_layout = "cols", header_n = "none"))[[1L]], "{col1}")
  # (the subset narrows the GROUPN's data, as the other analyses')
  s <- hn_expand("orr", subset = "SEX == \"F\"")
  expect_identical(s$ard$analyses[[3L]]$data, "adsl_sub")
  e <- s$ard$analysis_data[[1L]]
  expect_match(as.character(e$where), "SEX", fixed = TRUE)
})

test_that("orr: the GROUPN is the set's subjects, the header N what it was", {
  hn_skip()
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- tflspec::adsl
  rs <- tflspec::adrs
  for (x in list(list("adsl", sl), list("adrs", rs))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "HN-2")
  a <- list(population = "SAF", by = "TRT01A", subset = "SEX == \"F\"")
  want <- table(sl$TRT01A[sl$SAFFL == "Y" & sl$SEX == "F"])
  for (lay in c("rows", "cols")) {
    r <- hn_report(d, "orr", c(a, list(stat_layout = lay)))
    g <- hn_groupn(r$ard)
    expect_equal(sort(unname(g)), sort(as.numeric(want)), info = lay)
    for (k in names(want)) {
      expect_true(any(grepl(sprintf("(N=%d)", want[[k]]), r$header, fixed = TRUE)),
                  info = paste(lay, k))
    }
    n <- hn_report(d, "orr", c(a, list(stat_layout = lay, header_n = "none")))
    expect_false(any(grepl("N=", n$header, fixed = TRUE)), info = lay)
    expect_false("GROUPN" %in% n$ard$analysis_id)
    # (the cells as they were)
    cell <- function(z) {
      x <- z$ard[z$ard$analysis_id != "GROUPN", c("analysis_id", "stat_name", "stat")]
      rownames(x) <- NULL
      x
    }
    expect_identical(cell(n), cell(r))
  }
})

# ---- ae_overview ----

test_that("ae_overview: header_n \"set\" is today's, \"none\" drops the N; \"tested\" stops", {
  x <- hn_expand("ae_overview")
  expect_identical(hn_ids(x)[1L], "GROUPN")
  expect_identical(hn_expand("ae_overview", header_n = "set"), x)
  expect_identical(hn_expand("ae_overview", header_n = "denominator"), x)
  expect_identical(hn_texts(x)[[4L]], "(N={n})")
  n <- hn_expand("ae_overview", header_n = "none")
  expect_false("GROUPN" %in% hn_ids(n))
  expect_identical(hn_ids(n), hn_ids(x)[-1L])
  expect_false(any(grepl("N=", hn_all(n), fixed = TRUE)))
  expect_identical(hn_texts(n), hn_texts(x)[-4L])
  expect_error(hn_expand("ae_overview", header_n = "tested"),
               hn_msg("ae_overview", "tested", paste0("is not supported: use header_n = \"set\", ",
                        "\"denominator\" (the data the percentages are of; \"data\" for the subjects ",
                        "with an event kept) or \"none\".")), fixed = TRUE)
})

# ---- shift, lab_shift ----

test_that("shift and lab_shift: header_n \"tested\" without param_page stops", {
  for (tpl in c("shift", "lab_shift")) {
    a <- if (tpl == "shift") list(baseline = "BNRIND", post = "ANRIND", param = "PARAMCD") else
      list(visit = "V", params = "ALT")
    expect_error(hn_expand(tpl, a, header_n = "tested", param_page = FALSE),
                 sprintf("%s: header_n = \"tested\" needs param_page = TRUE", tpl), fixed = TRUE)
    expect_no_error(hn_expand(tpl, a, header_n = "tested"))
    expect_no_error(hn_expand(tpl, a, header_n = "set", param_page = FALSE))
    expect_no_error(hn_expand(tpl, a, header_n = "none", param_page = FALSE))
  }
})
