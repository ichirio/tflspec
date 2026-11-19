# tflspec_v1/tflspec_pk_conc: the series base template for PK
# concentrations, one row a subject and the time points across, their
# summary statistics below, a page an analyte and group.  Fictional data
# only: the package's sample ADSL and the fictional ADPC of helper-pk.R.

pkb_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

# The sample ADSL with a PK flag (the series' population_pk, PK: PKFL)
pkb_adsl <- function() {
  sl <- tflspec::adsl
  sl$PKFL <- sl$SAFFL
  sl
}

# A study of the sample ADSL and the fictional ADPC, the sample-01 defaults
pkb_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in list(list("adsl", pkb_adsl()), list("adpc", pk_adpc(tflspec::adsl)))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "PKB-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# The report's code list of the time points (their order)
pkb_codelist <- c("codelists:",
                  paste0("  ATPT: [", paste0("{value: ", pk_times, "}", collapse = ", "), "]"))

# A report of the template: `tpl` the lines under `template:` (after its
# name), `more` the report's other lines
pkb_report <- function(id, tpl, more = character()) {
  c(paste0("id: ", id), "kind: table", "label: PK concentrations",
    "template:", "  name: tflspec_v1/tflspec_pk_conc", "  by: [TRT01A]", tpl,
    pkb_codelist, more, "style: compact")
}

# The report written to the study, as its code SPEC; its ARD and RTF text
pkb_run <- function(d, id, lines) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
  rtf <- suppressMessages(tfl_run_output(id, dir = d))
  list(ard = as.data.frame(ard), txt = paste(readLines(rtf, warn = FALSE), collapse = "\n"))
}

# A value with n significant digits, as the RTF prints it
pkb_sig <- function(x, n) formatC(signif(x, n), digits = n, format = "fg", flag = "#")

test_that("tflspec_pk_conc: the listing with BLQ, the summary's significant digits, a page a group", {
  pkb_skip()
  d <- pkb_study(file.path(tempdir(), "pkb-main"))
  on.exit(unlink(d, recursive = TRUE))
  r <- pkb_run(d, "PK-1", pkb_report("PK-1", c("  blq: !r BLQFL == \"Y\"",
                                               "  decimal_align: false")))
  # an analysis an analyte; no BIGN
  expect_identical(unique(as.character(r$ard$analysis_id)), "DRUGX")
  # N a group and time point: the subjects with a value (the PK set)
  pc <- pk_adpc(tflspec::adsl)
  pc <- pc[!is.na(pc$AVAL), ]
  a <- r$ard[r$ard$stat_name == "N", ]
  got <- stats::setNames(as.integer(unlist(a$stat)),
                         paste(unlist(a$group1_level), unlist(a$group3_level)))
  want <- table(paste(pc$TRT01A, pc$ATPT))
  expect_identical(got[names(want)], stats::setNames(as.integer(want), names(want)))
  # the listing: a BLQ record prints BLQ; a value 3 significant digits
  expect_true(grepl("BLQ", r$txt, fixed = TRUE))
  one <- pc[pc$ATPT == "1 hr", ][1L, ]
  expect_true(grepl(pkb_sig(one$AVAL, 3L), r$txt, fixed = TRUE))
  # the summary: formats_pk's significant digits (mean 4s, sd 5s, min 3s)
  arm <- pc$TRT01A[1L]
  x <- pc$AVAL[pc$TRT01A == arm & pc$ATPT == "2 hr"]
  for (v in c(pkb_sig(mean(x), 4L), pkb_sig(stats::sd(x), 5L), pkb_sig(min(x), 3L))) {
    expect_true(grepl(v, r$txt, fixed = TRUE), info = v)
  }
  expect_true(grepl("Summary Statistics", r$txt, fixed = TRUE))
  expect_true(grepl("CV(%)", r$txt, fixed = TRUE))
  # a page an analyte and group, named by their values a line each
  for (g in unique(pc$TRT01A)) expect_true(grepl(g, r$txt, fixed = TRUE), info = g)
  expect_true(grepl("Time Relative to Dose", r$txt, fixed = TRUE))
})

test_that("tflspec_pk_conc: a report's summary card replaces the template's summary part", {
  pkb_skip()
  d <- pkb_study(file.path(tempdir(), "pkb-part"))
  on.exit(unlink(d, recursive = TRUE))
  part <- c("content:", "  - plan_append:", "      name: summary", "      data: {source: ard}",
            "      content:", "        - normalize_ard: {}",
            "        - table_plan: {cols: ATPT, rows: {block: !r '~ \"Statistics\"'}}",
            "        - plan_cells: {continuous: {n: \"{N}\"}}")
  r <- suppressMessages(pkb_run(d, "PK-2", pkb_report("PK-2", character(), part)))
  expect_true(grepl("Statistics", r$txt, fixed = TRUE))
  expect_false(grepl("Summary Statistics", r$txt, fixed = TRUE))
  expect_false(grepl("Median", r$txt, fixed = TRUE))
})

test_that("tflspec_pk_conc: the arguments' steps when resolved", {
  res <- function(...) {
    tfl_template_resolve(list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_pk_conc", by = "TRT01A", ...)))
  }
  item <- function(r, fn) {
    hit <- Filter(function(i) identical(i$fn, fn), r$content)
    if (length(hit)) hit[[1L]]$args
  }
  r <- res()
  # the series' PK set; no blq: no pk_value, the listing reads AVAL
  expect_identical(r$ard$analysis_data[[1L]]$population, "PK")
  expect_null(r$ard$analysis_data[[2L]]$derive)
  expect_identical(item(r, "table_plan")$value, "AVAL")
  expect_identical(item(r, "plan_cells")$default, "{value:.3s}")
  # a page's name: the analyte's and the group's values, a line each
  pg <- item(r, "plan_paginate_rows")
  expect_identical(unlist(pg$page_by), c("PARAMCD", "TRT01A"))
  expect_identical(pg$heading, "{PARAMCD}\n{TRT01A}")
  expect_false(is.null(item(r, "plan_columns")))
  expect_null(item(r, "plan_paginate_cols"))
  expect_length(item(r, "plan_col_header")$lines, 2L)
  # the summary's digits: formats_pk by the content path
  sm <- item(r, "plan_append")$content
  dg <- Filter(function(i) identical(i$fn, "plan_digits"), sm)[[1L]]$args$continuous
  expect_identical(dg$mean, "4s")
  expect_null(r$ard$analyses[[1L]]$formats)
  r <- res(blq = tfl_r("BLQFL == \"Y\""), page_heading = "Analyte: {PARAMCD}",
           time_heading = NULL, decimal_align = FALSE, split_at = "4 hr",
           analysis_ids = list(DRUGX = "CONC_X"))
  expect_match(as.character(r$ard$analysis_data[[2L]]$derive), "pk_value = ", fixed = TRUE)
  expect_identical(item(r, "table_plan")$value, "pk_value")
  expect_length(item(r, "plan_cells")$default, 2L)
  expect_identical(item(r, "plan_paginate_rows")$heading, "Analyte: {PARAMCD}")
  expect_length(item(r, "plan_col_header")$lines, 1L)
  expect_null(item(r, "plan_columns"))
  expect_identical(item(r, "plan_paginate_cols")$at, "4 hr")
  expect_match(r$ard$analyses[[1L]]$analysis_per, "DRUGX=CONC_X", fixed = TRUE)
})

test_that("tflspec_pk_conc: by is required; blq_label needs blq", {
  expect_error(tfl_template_resolve(list(id = "X", kind = "table",
                                         template = list(name = "tflspec_v1/tflspec_pk_conc"))),
               "needs by")
  expect_error(tfl_template_resolve(list(id = "X", kind = "table", template = list(
    name = "tflspec_v1/tflspec_pk_conc", by = "TRT01A", blq_label = "<LLOQ"))),
    "blq_label needs blq")
})
