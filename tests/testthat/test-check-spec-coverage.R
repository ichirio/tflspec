# tfl_check_spec(): the rows no other test made -- each with the case next to
# it that gives no row.  Fictional data only (the package's adsl).

cv_study <- function(env = parent.frame()) {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(.local_envir = env), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "CV-1"))
  d
}

# one report in the study's outputs folder, the others removed
cv_put <- function(d, o) {
  od <- file.path(d, "spec", "outputs")
  unlink(list.files(od, full.names = TRUE))
  tfl_write_output(o, file.path(od, paste0(o$id, ".yml")))
  invisible(o)
}

# a table of one categorical analysis of SEX, by TRT01A
cv_table <- function() {
  tfl_output("T-1", "table", common = list(population = "SAF"), label = "Sex",
    ard = list(analysis_data = list(list(id = "adsl_sub", from = "ADSL", population = "SAF")),
               analyses = list(list(id = "A1", method = "categorical", data = "adsl_sub",
                                    by = "TRT01A", variables = "SEX"))),
    content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
}

cv_rows <- function(p, part, item = NULL) {
  keep <- p$part == part
  if (!is.null(item)) keep <- keep & p$item %in% item
  p[keep, , drop = FALSE]
}

test_that("a `study` that is no study stops tfl_check_spec() and tfl_check_build()", {
  expect_error(tfl_check_spec(list()), "is a study read with tfl_read_study_spec()", fixed = TRUE)
  expect_error(tfl_check_build(list()), "is a study read with tfl_read_study_spec()", fixed = TRUE)
})

test_that("a report that cannot be resolved: a content error with no item; a known template: none", {
  d <- cv_study()
  cv_put(d, tfl_output("T-1", "table", common = list(population = "SAF"),
                       template = list(name = "no_such_template", args = list())))
  p <- tfl_check_spec(d, adam = FALSE)
  e <- cv_rows(p, "content", "")
  expect_identical(nrow(e), 1L)
  expect_identical(e$output_id, "T-1")
  expect_identical(e$level, "error")
  expect_match(e$message, "no template 'no_such_template'", fixed = TRUE)
  expect_identical(tfl_check_reports(p)$level, "ERROR")
  # an `extends` that names no layer
  o <- cv_table()
  o$extends <- "no_such_layer"
  cv_put(d, o)
  e <- cv_rows(tfl_check_spec(d, adam = FALSE), "content", "")
  expect_identical(e$level, "error")
  expect_match(e$message, "not 'no_such_layer'", fixed = TRUE)
  # the case next to them: a known template, a known layer
  cv_put(d, tfl_output("T-1", "table", common = list(population = "SAF"),
                       template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE",
                                                                   categorical = "SEX"))))
  expect_identical(nrow(cv_rows(tfl_check_spec(d, adam = FALSE), "content", "")), 0L)
  cv_put(d, cv_table())
  expect_identical(nrow(tfl_check_spec(d, adam = FALSE)), 0L)
})

test_that("an ARD program that cannot be written: a program warning; the code in its codes: none", {
  skip_if_not_installed("cards")
  d <- cv_study()
  o <- cv_table()
  o$codelists <- list(SEX = list(list(value = "M", label = "Male"), list(value = "F", label = "Female")))
  o$ard$analyses[[1L]]$where <- "SEX == \"Female\""
  cv_put(d, o)
  p <- tfl_check_spec(d, adam = FALSE)
  w <- cv_rows(p, "program", "ard")
  expect_identical(nrow(w), 1L)
  expect_identical(w$level, "warning")
  expect_match(w$message, "the ARD program cannot be written: ", fixed = TRUE)
  expect_match(w$message, "SEX: write \"F\" for \"Female\"", fixed = TRUE)
  expect_identical(tfl_check_reports(p)$level, "WARNING")
  o$ard$analyses[[1L]]$where <- "SEX == \"F\""
  cv_put(d, o)
  expect_identical(nrow(tfl_check_spec(d, adam = FALSE)), 0L)
})

test_that("a figure content key with a name the table does not have: a figure error; km_curves: none", {
  d <- cv_study()
  o <- tfl_output("F-1", "figure", common = list(population = "SAF"))
  o$figure <- list(name = "waterfall", args = list())
  cv_put(d, o)
  e <- cv_rows(tfl_check_spec(d, adam = FALSE), "figure", "figure")
  expect_identical(nrow(e), 1L)
  expect_identical(e$level, "error")
  expect_match(e$message, "the figure `waterfall` is not a figure content; one of km_curves, forest_hr",
               fixed = TRUE)
  o$figure <- list(name = "km_curves", args = list(by = "TRT01A", time = "AVAL", censor = "CNSR"))
  cv_put(d, o)
  expect_identical(nrow(cv_rows(tfl_check_spec(d, adam = FALSE), "figure")), 0L)
})

test_that("a plan_cells entry named as a plan_stat_groups group is no row's label", {
  d <- cv_study()
  o <- cv_table()
  o$content <- list(tfl_item("table_plan", list(cols = "TRT01A")),
                    tfl_item("plan_stat_groups", list(`Rate (CI)` = list("n", "estimate"))),
                    tfl_item("plan_cells", list(`Rate (CI)` = "{n} ({estimate:.1f%})")))
  cv_put(d, o)
  expect_identical(nrow(cv_rows(tfl_check_spec(d, adam = FALSE), "content")), 0L)
  # without the group: the label is an error of plan_cells
  o$content[[2L]] <- NULL
  cv_put(d, o)
  e <- cv_rows(tfl_check_spec(d, adam = FALSE), "content")
  expect_identical(e$item, "plan_cells(Rate (CI))")
  expect_identical(e$level, "error")
  expect_match(e$message, "is a row's label", fixed = TRUE)
})

test_that("the schema's warning that is no missing argument is a note, and the code is still written", {
  d <- cv_study()
  cv_put(d, cv_table())
  real <- tflspec::tfl_schema_check
  testthat::local_mocked_bindings(tfl_schema_check = function(item, ...) {
    p <- real(item, ...)
    if (identical(item$fn, "table_plan")) {
      p[nrow(p) + 1L, ] <- list("table_plan", "cols", "warning", "table_plan(cols = ) is now `columns`.")
    }
    p
  }, .package = "tflspec")
  n <- new.env()
  n$k <- 0L
  real_code <- tflspec::tfl_content_code
  testthat::local_mocked_bindings(tfl_content_code = function(...) {
    n$k <- n$k + 1L
    real_code(...)
  }, .package = "tflspec")
  p <- tfl_check_spec(d, adam = FALSE)
  expect_identical(p$part, "content")
  expect_identical(p$item, "table_plan(cols)")
  expect_identical(p$level, "note")
  expect_identical(tfl_check_reports(p)$level, "OK")
  expect_identical(n$k, 1L)
})

test_that("a content function the installed rtfreporter does not have: a program warning", {
  d <- cv_study()
  cv_put(d, cv_table())
  expect_identical(nrow(tfl_check_spec(d, adam = FALSE)), 0L)
  testthat::local_mocked_bindings(.pkg_fn_missing = function(call) {
    if (identical(call, "rtfreporter::table_plan")) "the installed rtfreporter 0.0.1 has no table_plan()"
  }, .package = "tflspec")
  w <- tfl_check_spec(d, adam = FALSE)
  expect_identical(w$part, "program")
  expect_identical(w$item, "table_plan")
  expect_identical(w$level, "warning")
  expect_identical(w$message, "the installed rtfreporter 0.0.1 has no table_plan(): update rtfreporter")
})

test_that("a report whose code stops: a program warning of its content", {
  d <- cv_study()
  cv_put(d, cv_table())
  testthat::local_mocked_bindings(tfl_content_code = function(...) stop("no code for this"),
                                  .package = "tflspec")
  w <- tfl_check_spec(d, adam = FALSE)
  expect_identical(w$part, "program")
  expect_identical(w$item, "content")
  expect_identical(w$level, "warning")
  expect_identical(w$message, "the report's code cannot be written: no code for this")
})

test_that("tfl_check_reports(): no report; the ids from the rows; a study row counts for each", {
  r <- tfl_check_reports(tflspec:::.check_rows())
  expect_identical(nrow(r), 0L)
  expect_named(r, c("output_id", "level", "errors", "warnings", "notes", "confirms", "confirm"))
  p <- tflspec:::.check_rows(output_id = c("A", "B", NA), part = "ard", item = "",
                             level = c("warning", "note", "error"), message = "m")
  r <- tfl_check_reports(p)
  expect_identical(r$output_id, c("A", "B"))
  expect_identical(r$level, c("ERROR", "ERROR"))
  expect_identical(r$errors, c(1L, 1L))
  expect_identical(r$warnings, c(1L, 0L))
  expect_identical(r$notes, c(0L, 1L))
})

test_that("tfl_check_fix_text(): a figure's instructions; no outputs folder: spec/outputs", {
  p <- tflspec:::.check_rows(output_id = c("F-1", "T-1"), part = "figure", item = c("x", ""),
                             level = c("error", "note"), message = "m")
  attr(p, "kinds") <- c("F-1" = "figure", "T-1" = "table")
  fx <- tfl_check_fix_text(p)
  expect_named(fx, "F-1")
  expect_match(fx[["F-1"]], "Fix the draft `spec/outputs/F-1.yml` (F-1)", fixed = TRUE)
  expect_match(fx[["F-1"]], "ai/instructions-figure.md", fixed = TRUE)
  expect_match(fx[["F-1"]], "- ERROR (figure, x): m", fixed = TRUE)
  # a row with no item: its part alone
  p$item[1L] <- ""
  expect_match(tfl_check_fix_text(p)[["F-1"]], "- ERROR (figure): m", fixed = TRUE)
  # "Japanese" is Japanese
  expect_identical(tfl_check_fix_text(p, language = "Japanese"), tfl_check_fix_text(p, language = "ja"))
  # no report at ERROR or WARNING
  expect_identical(tfl_check_fix_text(p[2L, ]), stats::setNames(character(), character()))
})

test_that("the outputs folder from the study folder; one outside it as it is", {
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  expect_identical(tflspec:::.check_rel_path(file.path(d, "spec", "outputs"), d), "spec/outputs")
  out <- file.path(dirname(d), "other")
  expect_identical(tflspec:::.check_rel_path(out, d), normalizePath(out, winslash = "/", mustWork = FALSE))
})

test_that("cells: a format with no digits, a template's `cell`, an analysis written as R", {
  cc <- function(o) tflspec:::.check_cell_rows(o, "T1")
  p <- cc(list(kind = "table", content = list(tfl_item("plan_cells", list(continuous = list(M = "{mean:abc}"))))))
  expect_identical(p$level, "warning")
  expect_match(p$message, "has the format \"abc\"", fixed = TRUE)
  expect_match(p$message, "write {mean:.1f}", fixed = TRUE)
  # a template's own cell argument: every statistic is allowed, the first ones named
  p <- cc(list(kind = "table", template = list(name = "shift", args = list(cell = "{nn}"))))
  expect_identical(p$item, "template$args$cell")
  expect_match(p$message, "names \"nn\"", fixed = TRUE)
  expect_match(p$message, "the statistics here are N, mean, sd", fixed = TRUE)
  expect_identical(nrow(cc(list(kind = "table", template = list(name = "shift", args = list(cell = "{n}"))))), 0L)
  # an analysis, or its method, written as R: the statistics are not known
  cell <- list(tfl_item("plan_cells", list(continuous = list(M = "{whatever}"))))
  expect_identical(nrow(cc(list(kind = "table", content = cell))), 1L)
  expect_identical(nrow(cc(list(kind = "table", ard = list(analyses = list(tfl_r("x"))), content = cell))), 0L)
  expect_identical(nrow(cc(list(kind = "table", ard = list(analyses = list(list(method = tfl_r("x")))),
                                content = cell))), 0L)
})

test_that("a KM figure: the time axis from `plot:`; the risk table off with its layer", {
  sr <- function(o) tflspec:::.check_static_rows(o, "F1")
  km <- list(name = "km_survival", args = list())
  p <- sr(list(kind = "figure", template = km, plot = list(x_label = "Days")))
  expect_identical(p$item, "x_label")
  expect_identical(p$level, "warning")
  expect_match(p$message, "\"Days (Months)\"", fixed = TRUE)
  expect_identical(nrow(sr(list(kind = "figure", template = km, plot = list(x_label = "Days (Days)")))), 0L)
  # the same label of a figure that is no KM, or of a table: nothing
  expect_identical(nrow(sr(list(kind = "figure", template = list(name = "forest_hr", args = list()),
                                plot = list(x_label = "Days")))), 0L)
  expect_identical(nrow(sr(list(kind = "table", template = km, plot = list(x_label = "Days")))), 0L)
  # risk_table: false and a risk_table layer
  o <- list(kind = "figure", template = list(name = "km_survival", args = list(risk_table = FALSE)),
            layers = list(list(layer = "risk_table")))
  p <- sr(o)
  expect_identical(p$item, "template$args$risk_table")
  expect_identical(p$level, "error")
  expect_match(p$message, "`risk_table: false` and a `risk_table` layer", fixed = TRUE)
  o$template$args$risk_table <- TRUE
  expect_identical(nrow(sr(o)), 0L)
})

test_that("ne-na: data that cannot be read give no row; a record's `!=` is a study row", {
  skip_if_not_installed("cards")
  d <- cv_study()
  o <- cv_table()
  o$ard$analyses[[1L]]$where <- "SEX != \"F\""
  cv_put(d, o)
  off <- tfl_check_spec(d, adam = FALSE)
  # data that are no data: as without
  expect_identical(tfl_check_spec(d, adam = 42), off)
  df <- data.frame(USUBJID = paste0("S", 1:4), SEX = c("F", "M", NA, NA), SAFFL = "Y",
                   TRT01A = "Drug X", AGE = c(30, 40, 50, 60), stringsAsFactors = FALSE)
  p <- tfl_check_spec(d, adam = list(ADSL = df))
  expect_identical(nrow(p), nrow(off) + 1L)
  # a study record whose `where` has `!=`: a row of the study (no report), with all reports only
  cat("records:\n  - {id: NOTF, dataset: ADSL, where: !r SEX != \"M\", label: Not M}\n",
      file = file.path(d, "spec", "data.yml"), append = TRUE)
  p <- tfl_check_spec(d, adam = list(ADSL = df))
  r <- p[p$item == "record NOTF / where", , drop = FALSE]
  expect_identical(nrow(r), 1L)
  expect_true(is.na(r$output_id))
  expect_identical(r$level, "warning")
  expect_match(r$message, "`SEX != \"M\"` drops 2 rows of ADSL where SEX is NA", fixed = TRUE)
  expect_identical(nrow(tfl_check_spec(d, "T-1", adam = list(ADSL = df))), nrow(off) + 1L)
  # the record's form that keeps them: no row
  ok <- df
  ok$SEX[is.na(ok$SEX)] <- "F"
  expect_identical(nrow(tfl_check_spec(d, adam = list(ADSL = ok))), nrow(off))
})
