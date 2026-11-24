# tflspec_v1/tflspec_listing_summary: the series base template for a
# listing of one value a subject and a variable's values across, with their
# summary under the same columns, a page a group (and analyte).  Fictional
# data only: the package's sample ADSL and a small ADPP made here.

lsb_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

# A fictional ADPP: 12 safety subjects of the sample ADSL, one analyte
# (DRUGX), three PK parameters; one CMAX value missing
lsb_adpp <- function() {
  sl <- tflspec::adsl
  sl <- sl[sl$SAFFL %in% "Y", ][seq_len(12L), ]
  base <- c(CMAX = 12.3, AUCLST = 981.2, TMAX = 1.5)
  k <- seq_len(nrow(sl))
  pp <- do.call(rbind, lapply(names(base), function(p) data.frame(
    STUDYID = sl$STUDYID, USUBJID = sl$USUBJID, PARCAT1 = "DRUGX", PARAMCD = p,
    AVAL = round(base[[p]] * (1 + ((k * 7L) %% 11L - 5L) / 20), 4L),
    stringsAsFactors = FALSE)))
  pp$AVAL[pp$PARAMCD == "CMAX"][2L] <- NA
  pp
}

# A study of the sample ADSL and the ADPP, the sample-01 defaults
lsb_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in list(list("adsl", tflspec::adsl), list("adpp", lsb_adpp()))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "LSB-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# A report of the template: `tpl` the lines under `template:` (after its name)
lsb_report <- function(id, tpl, more = character()) {
  c(paste0("id: ", id), "kind: table", "label: PK parameters by subject",
    "common:", "  population: SAF",
    "template:", "  name: tflspec_v1/tflspec_listing_summary", tpl, more,
    "style: compact")
}

lsb_tpl <- c("  data:", "    from: ADPP",
             "  analysis:", "    by: [TRT01A]", "    param_var: PARCAT1", "    across: PARAMCD")

# The report written to the study, as its code SPEC; its ARD and RTF
lsb_run <- function(d, id, lines, rtf = TRUE) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
  list(ard = as.data.frame(ard), rtf = if (rtf) tfl_run_output(id, dir = d))
}

# An ARD's list cell as text ("" for none)
lsb_chr <- function(v) {
  vapply(v, function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) "" else
    as.character(x[[1L]]), "")
}

test_that("tflspec_listing_summary: resolves to a listing, its summary part and the ARD item", {
  res <- function(...) {
    tfl_template_resolve(list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_listing_summary", from = "ADPP", by = "TRT01A",
      across = "PARAMCD", ...)))
  }
  r <- res(param_var = "PARCAT1")
  an <- r$ard$analyses
  expect_length(an, 1L)
  expect_identical(an[[1L]]$method, "continuous")
  expect_identical(as.character(unlist(an[[1L]]$by)), c("TRT01A", "PARCAT1", "PARAMCD"))
  expect_identical(as.character(unlist(an[[1L]]$variables)), "AVAL")
  expect_identical(an[[1L]]$analysis_per, "PARCAT1")
  fns <- vapply(r$content, function(i) i$fn, "")
  # (the listing reads rec_sub's records: the first card, rework, is the data map)
  expect_true("dataset" %in% c(r$data$source, r$ard$data$source))
  expect_identical(fns[1:3], c("table_plan", "plan_cells", "plan_append"))
  expect_true(all(c("plan_stub", "plan_blanks", "plan_col_header", "plan_paginate_rows",
                    "plan_columns") %in% fns))
  # no param_var: one analysis, SUMM; the pages by the groups alone
  r <- res()
  expect_identical(r$ard$analyses[[1L]]$id, "SUMM")
  expect_null(r$ard$analyses[[1L]]$analysis_per)
  expect_identical(as.character(unlist(r$ard$analyses[[1L]]$by)), c("TRT01A", "PARAMCD"))
  # required: from, by, across
  expect_error(tfl_template_resolve(list(id = "X", kind = "table", template = list(
    name = "tflspec_v1/tflspec_listing_summary", from = "ADPP", by = "TRT01A"))), "across")
})

test_that("tflspec_listing_summary: the ARD by analyte, a parameter's own formats, the RTF's two parts", {
  lsb_skip()
  d <- lsb_study(file.path(tempdir(), "lsb-main"))
  on.exit(unlink(d, recursive = TRUE))
  fm <- "    formats: {N: 0, mean: 2, sd: 3, median: 2, min: 2, max: 2, AUCLST: {mean: 4s, sd: 1}}"
  r <- lsb_run(d, "P-1", lsb_report("P-1", c(lsb_tpl, fm)))
  a <- r$ard
  # an analysis an analyte
  expect_identical(unique(as.character(a$analysis_id)), "DRUGX")
  par <- lsb_chr(a$group3_level)
  pick <- function(p, s) a[par == p & a$stat_name == s, ]
  # the parameter's own: AUCLST's mean in 4 significant digits, its sd 1 decimal
  m <- pick("AUCLST", "mean")
  expect_gt(nrow(m), 0L)
  expect_identical(unlist(m$stat_fmt), rtfreporter::fmt_signif(unlist(m$stat), 4L))
  s <- pick("AUCLST", "sd")
  expect_identical(unlist(s$stat_fmt), vapply(s$stat, cards::label_round(1L), ""))
  # the other parameters: the statistics' formats
  for (p in c("CMAX", "TMAX")) {
    m <- pick(p, "mean")
    expect_identical(unlist(m$stat_fmt), vapply(m$stat, cards::label_round(2L), ""), info = p)
    s <- pick(p, "sd")
    expect_identical(unlist(s$stat_fmt), vapply(s$stat, cards::label_round(3L), ""), info = p)
  }
  # the missing CMAX value is not counted
  n <- pick("CMAX", "N")
  expect_identical(sum(unlist(n$stat)), 11L)
  # the RTF: the listing (a row a subject) and the summary under it
  txt <- paste(readLines(r$rtf, warn = FALSE), collapse = "\n")
  ids <- as.character(lsb_adpp()$USUBJID[1:3])
  for (id in ids) expect_match(txt, id, fixed = TRUE)
  expect_match(txt, "Summary Statistics", fixed = TRUE)
  expect_match(txt, "Mean (SD)", fixed = TRUE)
  expect_match(txt, "Min, Max", fixed = TRUE)
  expect_match(txt, "DRUGX", fixed = TRUE)
  expect_match(txt, "AUCLST", fixed = TRUE)
})
