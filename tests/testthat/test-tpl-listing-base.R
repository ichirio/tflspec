# tflspec_v1/tflspec_listing: the series base template for the records of
# any dataset listed one row a record.  The package's sample data
# (fictional) only.

lstb_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
}

# A study of the sample data: ADSL and ADAE, the sample-01 defaults
lstb_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "LST-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# The severe TEAE listing: ADSL's AGE and SEX added (subject_vars), the
# records kept on the lst_sub row; `more` lines under `template:`, `where`
# the row's condition
lstb_report <- function(id, more = character(), where = "TRTEMFL == \"Y\" & AESEV == \"SEVERE\"") {
  c(paste0("id: ", id), "kind: listing", "label: Severe treatment-emergent adverse events",
    "template:", "  name: tflspec_v1/tflspec_listing",
    "  data:", "    from: ADAE", "    subject_vars: [AGE, SEX]", "    sort: [USUBJID, AEDECOD]",
    "  content:", "    columns:",
    "      - {vars: [USUBJID], label: Subject, width: 20, collapse_repeats: true}",
    "      - {vars: [AGE, SEX], label: Age/Sex, width: 8}",
    "      - {vars: [AEDECOD], label: Preferred Term, width: 40}",
    "      - {vars: [AESEV], label: Severity, width: 10}",
    more,
    "data:", "  - id: lst_sub", paste0("    where: !r ", where))
}

# The report written to the study as its code SPEC, its RTF's cell texts
lstb_run <- function(d, id, lines) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  invisible(tfl_copy_templates(d))
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  rtf <- tfl_run_output(id, dir = d)
  txt <- paste(readLines(rtf, warn = FALSE), collapse = "\n")
  cells <- regmatches(txt, gregexpr("[^\\\\{}]*\\\\cell", txt))[[1L]]
  # (a cell's text follows the control word before it: "\ql\li0\ri0 text")
  cells <- trimws(sub("^[^ ]* ?", "", sub("\\\\cell$", "", cells)))
  list(spec = tfl_read_output(f), txt = txt, cells = cells[nzchar(cells)])
}

lstb_args <- list(from = "ADAE", columns = list(list(vars = "USUBJID"), list(vars = "AEDECOD")))
lstb_resolve <- function(...) {
  tfl_template_resolve(list(id = "X", kind = "listing", template = c(
    list(name = "tflspec_v1/tflspec_listing"), lstb_args, list(...))))
}
lstb_fns <- function(r) vapply(r$content, function(i) i$fn, "")
lstb_item <- function(r, fn) r$content[[match(fn, lstb_fns(r))]]$args

test_that("tflspec_listing: from and columns are required", {
  rep <- function(tpl) list(id = "X", kind = "listing",
                            template = c(list(name = "tflspec_v1/tflspec_listing"), tpl))
  expect_error(tfl_template_resolve(rep(list(columns = list(list(vars = "USUBJID"))))), "needs from")
  expect_error(tfl_template_resolve(rep(list(from = "ADAE"))), "needs columns")
})

test_that("tflspec_listing resolves: no analysis, the rows' add and arrange, one listing_col a column", {
  r <- lstb_resolve(subject_vars = c("AGE", "SEX"), sort = c("USUBJID", "-ASTDT"))
  expect_null(r$ard$analyses)
  expect_identical(r$style, "tfl_small")
  lst <- r$ard$analysis_data[[2L]]
  expect_identical(lst$id, "lst_sub")
  expect_identical(as.character(unlist(lst$add)), c("AGE", "SEX"))
  expect_identical(as.character(unlist(lst$arrange)), c("USUBJID", "-ASTDT"))
  expect_identical(lstb_fns(r), c("plan_listing", "plan_empty", "plan_paginate_rows"))
  cols <- lstb_item(r, "plan_listing")$cols
  expect_length(cols, 2L)
  expect_true(all(vapply(cols, function(cl) identical(cl$fn, "listing_col"), NA)))
  expect_identical(cols[[1L]]$args, list(vars = "USUBJID"))
  expect_identical(lstb_item(r, "plan_listing")$type, "multiline")
  expect_identical(lstb_item(r, "plan_paginate_rows"), list(max_rows = 30L))
  # no subject_vars, no sort: neither key
  r0 <- lstb_resolve()
  expect_null(r0$ard$analysis_data[[2L]]$arrange)
  expect_length(r0$ard$analysis_data[[2L]]$add, 0L)
  # empty_text "" or unset: no plan_empty
  expect_false("plan_empty" %in% lstb_fns(lstb_resolve(empty_text = "")))
  expect_false("plan_empty" %in% lstb_fns(lstb_resolve(empty_text = NULL)))
})

test_that("tflspec_listing: by divides the listing, by page or by block", {
  r <- lstb_resolve(by = "TRT01A", sort = "USUBJID")
  expect_identical(as.character(unlist(r$ard$analysis_data[[2L]]$arrange)), c("TRT01A", "USUBJID"))
  expect_identical(lstb_fns(r), c("plan_listing", "plan_hide", "plan_empty", "plan_paginate_rows"))
  cols <- lstb_item(r, "plan_listing")$cols
  expect_identical(cols[[3L]]$args, list(vars = "TRT01A", name = "by_TRT01A"))
  expect_identical(as.character(unlist(lstb_item(r, "plan_hide")$cols)), "by_TRT01A")
  pg <- lstb_item(r, "plan_paginate_rows")
  expect_identical(as.character(unlist(pg$page_by)), "by_TRT01A")
  expect_identical(pg$heading, "TRT01A: {value}")
  expect_identical(pg$heading_at, "title")
  expect_identical(lstb_item(lstb_resolve(by = "TRT01A", by_heading = "Arm: {value}"),
                             "plan_paginate_rows")$heading, "Arm: {value}")
  b <- lstb_resolve(by = "TRT01A", by_break = "block")
  expect_identical(lstb_item(b, "plan_row_group"), list(mode = "value", group_col = "by_TRT01A"))
  expect_identical(lstb_item(b, "plan_paginate_rows"), list(max_rows = 30L))
})

test_that("a one-value argument is one value of its type, an enum's one of its values", {
  # (a list would stop in rtfreporter naming plan_paginate_rows(heading = );
  # an enum's other value would leave the by steps out without a word)
  expect_error(lstb_resolve(by = "TRT01A", by_heading = c("a", "b")),
               "tflspec_listing's by_heading is one value (string); the report gives 2.", fixed = TRUE)
  expect_error(lstb_resolve(by = "TRT01A", by_break = "pages"),
               "tflspec_listing's by_break is one of page, block; the report gives \"pages\".", fixed = TRUE)
  expect_error(lstb_resolve(max_rows = list(10, 20)), "max_rows is one value (number)", fixed = TRUE)
  expect_identical(lstb_fns(lstb_resolve(by = "TRT01A", by_break = "block"))[2:3],
                   c("plan_hide", "plan_row_group"))
})

test_that("tflspec_listing on ADAE: a severe TEAE listing with ADSL's AGE and SEX, in the sort order", {
  lstb_skip()
  d <- lstb_study(file.path(tempdir(), "lstb-sev"))
  on.exit(unlink(d, recursive = TRUE))
  x <- lstb_run(d, "L-1", lstb_report("L-1"))
  ae <- tflspec::adae[tflspec::adae$TRTEMFL %in% "Y" & tflspec::adae$AESEV %in% "SEVERE", ]
  adsl <- tflspec::adsl[tflspec::adsl$SAFFL %in% "Y", ]
  ae <- ae[ae$USUBJID %in% adsl$USUBJID, ]
  ae <- ae[order(ae$USUBJID, ae$AEDECOD, method = "radix"), ]
  # every record a row: its severity, its term in that order
  expect_identical(sum(x$cells == "SEVERE"), nrow(ae))
  terms <- x$cells[x$cells %in% unique(ae$AEDECOD)]
  expect_identical(terms, as.character(ae$AEDECOD))
  # AGE and SEX came from ADSL (ADAE has neither)
  expect_false(any(c("AGE", "SEX") %in% names(tflspec::adae)))
  first <- adsl[adsl$USUBJID == ae$USUBJID[1L], ]
  expect_true(paste0(first$AGE, "/") %in% x$cells)
  expect_identical(as.character(unlist(x$spec$ard$analysis_data[[2L]]$add)), c("AGE", "SEX"))
})

test_that("tflspec_listing on ADAE: by starts a page each value, named in the title", {
  lstb_skip()
  d <- lstb_study(file.path(tempdir(), "lstb-by"))
  on.exit(unlink(d, recursive = TRUE))
  x <- lstb_run(d, "L-2", lstb_report("L-2", c("    by: [TRT01A]", "    max_rows: 200")))
  arms <- sort(unique(tflspec::adsl$TRT01A[tflspec::adsl$SAFFL %in% "Y"]))
  # the name once a page: the last title row (heading_at: title), not a
  # section row as well (one page an arm)
  for (a in arms) {
    hits <- gregexpr(paste0("TRT01A: ", a), x$txt, fixed = TRUE)[[1L]]
    expect_identical(sum(hits > 0L), 1L, info = a)
  }
  # a page an arm (200 rows a page hold an arm's records)
  pages <- as.integer(sub("^Page 1 of ", "", grep("^Page 1 of ", x$cells, value = TRUE)[1L]))
  expect_identical(pages, length(arms))
})

test_that("tflspec_listing on ADAE: no record left, the report carries empty_text's plan_empty", {
  lstb_skip()
  d <- lstb_study(file.path(tempdir(), "lstb-empty"))
  on.exit(unlink(d, recursive = TRUE))
  empty_text <- function(x) {
    it <- Filter(function(i) identical(i$fn, "plan_empty"), x$spec$content)
    if (length(it)) it[[1L]]$args$text
  }
  x <- lstb_run(d, "L-3", lstb_report("L-3", where = "TRTEMFL == \"Z\""))
  expect_identical(empty_text(x), "No records were reported.")
  # the listing is made, its header and no record
  expect_true("Preferred Term" %in% x$cells)
  expect_false("SEVERE" %in% x$cells)
  y <- lstb_run(d, "L-4", lstb_report("L-4", "    empty_text: No severe events.", where = "TRTEMFL == \"Z\""))
  expect_identical(empty_text(y), "No severe events.")
})

test_that("a data row's arrange: a column, -column or desc(column)", {
  expect_identical(tflspec:::.arrange_terms(c("USUBJID", "-ASTDT", "desc(AESEV)")),
                   c("USUBJID", "dplyr::desc(ASTDT)", "dplyr::desc(AESEV)"))
  ok <- grepl(tflspec:::.arrange_re, c("USUBJID", "-ASTDT", "desc(AESEV)", "ASTDT + 1", "desc(-X)"))
  expect_identical(ok, c(TRUE, TRUE, TRUE, FALSE, FALSE))
})
