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
  # (the sample study's settings.style, standard, is the listing's style:
  # the heading is a line above the table, not a cell of it)
  for (a in arms) {
    expect_match(x$txt, paste0("TRT01A: ", a), fixed = TRUE, info = a)
    expect_false(paste0("TRT01A: ", a) %in% x$cells, info = a)
  }
  expect_identical(x$spec$style, "standard")
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

# ---- the listing's style: the study's settings.style, else the series's ------
# (canon e023312: `style: !study {key: settings.style, fallback: !series
# style_listing}` in both listing templates; series.yml style_listing:
# tfl_small)

# The listing template's style for `study` (a list, a folder or NULL), and
# its origin, as tfl_template_resolve() resolves the template
lstb_style <- function(study, name = "tflspec_v1/tflspec_listing", report_style = NULL) {
  args <- if (identical(name, "tflspec_v1/listing")) list(from = "ADAE") else lstb_args
  rep <- list(id = "X", kind = "listing", template = c(list(name = name), args))
  rep$style <- report_style
  st <- tflspec:::.tr_study(study, "test")
  sd <- attr(st, "study_dir")
  tf <- tflspec:::.template_file(name, sd)
  tpl <- tflspec:::.series_resolve(tflspec:::.series_read_yaml(tf),
                                   tflspec:::.tr_series(name, tf, sd, "test"), st, name)
  list(style = tfl_template_resolve(rep, study = study)$style,
       origin = attr(tpl, "origins")[["style"]])
}

# A study folder under `root`: study.yml with `settings` (lines), its copy
# of the listing templates
lstb_folder <- function(root, name, settings) {
  st <- file.path(root, name)
  dir.create(file.path(st, "spec", "outputs"), recursive = TRUE)
  writeLines(c(paste0("study_id: ", name), "settings:", settings), file.path(st, "study.yml"))
  invisible(tfl_copy_templates(st, c("tflspec_v1/tflspec_listing", "tflspec_v1/listing")))
  st
}

test_that("both listing templates write the style as the study's, else the series's", {
  for (t in c("listing", "tflspec_listing")) {
    f <- system.file("series", "tflspec_v1", "templates", paste0(t, ".yml"), package = "tflspec")
    expect_true("style: !study {key: settings.style, fallback: !series style_listing}" %in%
                  readLines(f, warn = FALSE), info = t)
  }
  # the series keeps the fallback a value (a series's values: hold no !study)
  expect_identical(tfl_series_read("tflspec_v1")$values$style_listing, "tfl_small")
})

test_that("a listing's style: the study's settings.style (origin study)", {
  for (t in c("tflspec_v1/tflspec_listing", "tflspec_v1/listing")) {
    s <- lstb_style(list(settings = list(rounding = "sas", style = "standard")), t)
    expect_identical(s, list(style = "standard", origin = "study"), info = t)
  }
})

test_that("a listing's style: no settings.style, or no study, is tfl_small (origin series)", {
  for (t in c("tflspec_v1/tflspec_listing", "tflspec_v1/listing")) {
    expect_identical(lstb_style(list(settings = list(rounding = "sas")), t),
                     list(style = "tfl_small", origin = "series"), info = t)
    expect_identical(lstb_style(list(), t), list(style = "tfl_small", origin = "series"), info = t)
    expect_identical(lstb_style(NULL, t), list(style = "tfl_small", origin = "series"), info = t)
    # YAML null is no value: the fallback
    nul <- yaml::yaml.load("settings:\n  style: ~", handlers = tflspec:::.yaml_handlers())
    expect_identical(lstb_style(nul, t), list(style = "tfl_small", origin = "series"), info = t)
  }
})

test_that("a listing's style: settings.style !unset is no style (a report's default, tfl_standard)", {
  un <- yaml::yaml.load("settings:\n  style: !unset", handlers = tflspec:::.yaml_handlers())
  for (t in c("tflspec_v1/tflspec_listing", "tflspec_v1/listing")) {
    s <- lstb_style(un, t)
    # the study switched the style off: not the fallback (tfl_small); the
    # listing writes no style:, so it gets what any report without one
    # gets, the study's default style, and with no settings.style that is
    # tfl_standard
    expect_null(s$style, info = t)
    expect_identical(s$origin, "study", info = t)
  }
  expect_identical(tflspec:::.style_default_name(list(study = un)), "tfl_standard")
})

test_that("a listing in a study whose settings.style is !unset runs on tfl_standard's page", {
  lstb_skip()
  d <- lstb_study(file.path(tempdir(), "lstb-unset"))
  on.exit(unlink(d, recursive = TRUE))
  sy <- file.path(d, "study.yml")
  y <- readLines(sy, warn = FALSE)
  expect_true("  style: standard" %in% y)
  writeLines(sub("^  style: standard$", "  style: !unset", y), sy)
  x <- lstb_run(d, "L-5", lstb_report("L-5"))
  expect_null(x$spec$style)
  # not the study's standard (its company line), but tfl_standard's tokens
  expect_false(grepl("Sample Pharma", x$txt, fixed = TRUE))
  expect_match(x$txt, "COMPANY", fixed = TRUE)
})

test_that("a listing's style: the study's copy keeps the line and resolves the same way", {
  root <- tempfile("lsty")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(root)
  a <- lstb_folder(root, "STUDY-A", c("  rounding: sas", "  style: standard"))
  b <- lstb_folder(root, "STUDY-B", "  rounding: sas")
  for (t in c("listing", "tflspec_listing")) {
    copy <- file.path(a, "spec", "series", "tflspec_v1", "templates", paste0(t, ".yml"))
    expect_true("style: !study {key: settings.style, fallback: !series style_listing}" %in%
                  readLines(copy, warn = FALSE), info = t)
    tn <- paste0("tflspec_v1/", t)
    expect_identical(lstb_style(a, tn), list(style = "standard", origin = "study"), info = t)
    expect_identical(lstb_style(b, tn), list(style = "tfl_small", origin = "series"), info = t)
  }
  # the study.yml file names the same study as its folder
  expect_identical(lstb_style(file.path(a, "study.yml"))$style, "standard")
})

test_that("a listing's style: style_listing set in the study's copy is used without settings.style", {
  root <- tempfile("lsty")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(root)
  a <- lstb_folder(root, "STUDY-A", c("  rounding: sas", "  style: standard"))
  b <- lstb_folder(root, "STUDY-B", "  rounding: sas")
  for (st in c(a, b)) tfl_series_values_set(st, "tflspec_v1", list(style_listing = "compact"))
  # no settings.style: the copy's series value
  expect_identical(lstb_style(b), list(style = "compact", origin = "series"))
  expect_identical(lstb_style(b, "tflspec_v1/listing")$style, "compact")
  # settings.style is first: the series value is only the fallback
  expect_identical(lstb_style(a), list(style = "standard", origin = "study"))
})

test_that("a listing's style: the report's own style: wins over the study's and the series's", {
  root <- tempfile("lsty")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(root)
  b <- lstb_folder(root, "STUDY-B", "  rounding: sas")
  tfl_series_values_set(b, "tflspec_v1", list(style_listing = "compact"))
  own <- "landscape"
  expect_identical(lstb_style(list(settings = list(style = "standard")), report_style = own)$style, own)
  expect_identical(lstb_style(NULL, report_style = own)$style, own)
  expect_identical(lstb_style(b, report_style = own)$style, own)
  expect_identical(lstb_style(b, "tflspec_v1/listing", report_style = own)$style, own)
})
