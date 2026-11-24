# The study's report programs and ARD programs (tflspec#910, G5-1c), moved
# from tflplanner: byte for byte what tflplanner wrote before the move
# (tests/testthat/fixtures/programs-family/: README.md says how they were
# frozen).  For each study of the fixtures, in/<study>.rds holds the values
# tflplanner gives tfl_report_program() and tfl_ard_program() (its
# .report_values(), .report_shared(), the company's inputs), and out/<study>/
# the programs tflplanner wrote before the move, the `Generated` line
# masked: tfl/<id>.R a report's program, data/<id>.R its data part as a
# preview makes it, ard/<id>.R and ard/ard_setup.R the ARD programs.  No text
# may change in this move (design Q1, option A).

prog_fx <- function(...) test_path("fixtures", "programs-family", ...)

# a program's text as it is written (writeLines(): a line an element), its
# Generated line masked
prog_fx_text <- function(code) {
  code <- sub("^(#  Generated  :).*$", "\\1 <masked>", code)
  paste0(paste(enc2utf8(code), collapse = "\n"), "\n")
}

# a fixture's text, its line ends as \n whatever the checkout made them;
# NULL when there is none
prog_fx_read <- function(study, key) {
  f <- prog_fx("out", study, paste0(key, ".R"))
  if (!file.exists(f)) return(NULL)
  txt <- rawToChar(readBin(f, "raw", file.size(f)))
  Encoding(txt) <- "UTF-8"
  gsub("\r\n", "\n", txt, fixed = TRUE)
}

prog_fx_studies <- function() sub("\\.rds$", "", list.files(prog_fx("in"), "\\.rds$"))

# a study's values; its spec reads its series copies from the fixture's
# copy of its study.yml and spec folder (study/<study>/)
prog_fx_inputs <- function(study) {
  x <- readRDS(prog_fx("in", paste0(study, ".rds")))
  if (!is.null(x$study) && isTRUE(x$study_dir)) x$study$dir <- prog_fx("study", study)
  # the ARD programs' fingerprint reads the study's own function files
  # from the study folder: the fixture's copy of them
  if (isTRUE(x$ard$dir)) x$ard$dir <- prog_fx("study", study)
  x
}

# an error as the fixture has it
prog_fx_try <- function(expr) {
  tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
}

# one report's program, its data part, the ARD programs
prog_fx_report <- function(x, id, part = "program") {
  data <- identical(part, "data")
  prog_fx_try(tfl_report_program(
    x$reports[[id]], study = x$study, shared = x$shared, standards = x$standards,
    # (a preview runs its data part in the app's session: no study context)
    attached = if (data) character() else x$attached,
    study_code = if (data) FALSE else x$study_code,
    layout = x$layout, date = x$date, generator = "tflplanner 0.0.0",
    part = part, todo = FALSE))
}
prog_fx_ard <- function(x, id = NULL) {
  prog_fx_try(tfl_ard_program(x$ard$spec, id, standards = x$standards[c("ard_statistics", "ard_methods")],
                              codelists = x$ard$codelists, attached = x$attached,
                              dir = if (is.character(x$ard$dir)) x$ard$dir else ".",
                              study_code = x$study_code, layout = x$layout, date = x$date,
                              generator = "tflplanner 0.0.0"))
}

# every program of a fixture study compared with its fixture; the keys
# that differ
prog_fx_diff <- function(study) {
  x <- prog_fx_inputs(study)
  bad <- character()
  same <- function(key, code) {
    want <- prog_fx_read(study, key)
    got <- if (!is.null(code)) prog_fx_text(code)
    if (!identical(got, want)) bad[[length(bad) + 1L]] <<- key
  }
  for (id in names(x$reports)) {
    same(file.path("tfl", id), prog_fx_report(x, id))
    same(file.path("data", id), prog_fx_report(x, id, "data"))
  }
  if (!is.null(x$ard)) {
    same(file.path("ard", "ard_setup"), prog_fx_ard(x))
    for (id in x$ard$ids) same(file.path("ard", id), prog_fx_ard(x, id))
  }
  bad
}

test_that("the report and ARD programs are tflplanner's, byte for byte (golden fixtures)", {
  studies <- prog_fx_studies()
  expect_true(all(c("sample", "sample-company", "edited", "sheets") %in% studies))
  n <- 0L
  for (study in studies) {
    x <- prog_fx_inputs(study)
    n <- n + length(x$reports) + length(x$ard$ids)
    expect_identical(prog_fx_diff(study), character(), label = study)
  }
  # (every report a program, a few hundred files in all)
  expect_gt(n, 100L)
})

test_that("the fixtures hold every kind of report and of ARD program", {
  x <- prog_fx_inputs("edited")
  r <- x$reports
  kind <- function(f) names(Filter(f, r))
  # a table, a listing and a figure of their definitions
  expect_true(all(c("table", "listing", "figure") %in% vapply(r, `[[`, "", "type")))
  # template reports, a code SPEC report (its content R of its own)
  expect_true(all(c("T-TPL", "L-TPL", "F-TPL") %in% names(r)))
  # (and in the YAML study a report whose spec's content is R of its own)
  r_content <- function(v) inherits(v$spec_output$content, "tfl_r") || inherits(v$output$content, "tfl_r")
  expect_true(any(vapply(prog_fx_inputs("sample")$reports, r_content, NA)))
  # a figure's design, a figure's content key, a step of its own code, a
  # Rework, an ARD taken in, a listing of the listing sheets
  expect_gt(length(kind(function(v) !is.null(v$design) && is.null(v$design$figure))), 0L)
  expect_gt(length(kind(function(v) !is.null(v$design$figure))), 0L)
  expect_gt(length(kind(function(v) !is.na(v$steps$data))), 0L)
  expect_gt(length(kind(function(v) !is.na(v$steps$content))), 0L)
  expect_gt(length(kind(function(v) !is.na(v$process))), 0L)
  expect_gt(length(kind(function(v) !is.null(v$ard_import))), 0L)
  expect_gt(length(kind(function(v) !is.null(v$listing))), 0L)
  expect_true(all(c("own", "table", "import", "none") %in%
                    vapply(r, function(v) v$ard_source$kind, "")))
  # the sheets' study: no spec, its reports from the sheets
  expect_null(prog_fx_inputs("sheets")$study)
  # the ARD setup and an output's ARD program
  expect_gt(length(x$ard$ids), 0L)
  # the company's every input differs from tflspec's own
  s <- prog_fx_inputs("sample-company")$standards
  expect_false(identical(s$code_templates$code, tfl_builtin_standards()$code_templates$code))
  expect_identical(s$settings$value[s$settings$key == "listing_type"], "compact")
})

test_that("hostile session options do not reach the programs", {
  x <- prog_fx_inputs("edited")
  ids <- names(x$reports)
  before <- lapply(ids, function(id) prog_fx_report(x, id))
  ard <- prog_fx_ard(x, x$ard$ids[1L])
  old <- options(tflspec.paths = c(path_adam = "elsewhere/adam", path_ard = "x"),
                 tflspec.attached = c("ggplot2", "dplyr", "cards", "tflspec", "rtfreporter"),
                 tflspec.study_setup = FALSE, tflspec.series_dir = tempdir(),
                 tflspec.fig_style = list(settings = data.frame()),
                 tflspec.ard_methods = data.frame(), tflspec.ard_statistics = data.frame(),
                 rtfreporter.ard_pipe = "%>%", tflspec.ggplot2_version = "3.3.0",
                 rtfreporter.font_size_half_points = 30, tflspec.outputs_dir = "nowhere",
                 tflspec.plain_ns = TRUE, tflspec.user_attached = "ggplot2",
                 digits = 3, scipen = 100, OutDec = ",")
  on.exit(options(old), add = TRUE)
  after <- lapply(ids, function(id) prog_fx_report(x, id))
  expect_identical(after, before)
  expect_identical(prog_fx_ard(x, x$ard$ids[1L]), ard)
})

test_that("a value given as a function is worked out only when a program needs it", {
  x <- prog_fx_inputs("sample")
  called <- 0L
  sh <- x$shared
  sh$sheets <- function() {
    called <<- called + 1L
    x$shared$sheets
  }
  # a report the study's spec says: its report part is the spec's
  id <- names(Filter(function(v) !is.null(v$output), x$reports))[1L]
  code <- tfl_report_program(x$reports[[id]], study = x$study, shared = sh,
                             standards = x$standards, attached = x$attached,
                             layout = x$layout, date = x$date)
  expect_identical(called, 0L)
  # an error it gave is the program's error, as it was
  sh$sheets <- simpleError("the sheets do not hold")
  r <- x$reports[[id]]
  r$output <- NULL
  expect_error(tfl_report_program(r, study = x$study, shared = sh, layout = x$layout),
               "the sheets do not hold")
  expect_true(any(startsWith(code, "#  Generated  : tflspec ")))
})

test_that("tflspec's own company inputs when none are given", {
  b <- tfl_builtin_standards()
  expect_identical(b$code_templates$name, c("table_data", "table_process", "figure_plot", "setup"))
  # the default company home's inputs are tflspec's own: a program made
  # with `standards` left out is the one made with them
  x <- prog_fx_inputs("sample")
  expect_identical(x$standards$code_templates$code, b$code_templates$code)
  for (id in names(x$reports)) {
    with_std <- prog_fx_report(x, id)
    x0 <- x
    x0$standards <- list()
    expect_identical(prog_fx_report(x0, id), with_std, label = id)
  }
  # a listing type of the company's, else multiline
  expect_identical(.rp_setting(list(), "listing_type", "multiline"), "multiline")
})

test_that("the makers say what is wrong with their arguments", {
  expect_error(tfl_report_program(list(id = "T-1")), "`report` is a list")
  expect_error(tfl_report_program(list(id = "T-1", type = "table", program = "T-1.R"),
                                  standards = list(about = 1)), "`standards` is a list")
  expect_error(tfl_report_program(list(id = "T-1", type = "table", program = "T-1.R"),
                                  layout = c(adam = "a")), "`layout`")
  expect_error(tfl_ard_program(list()), "tfl_ard_spec")
  sp <- tfl_ard_spec(list(
    datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = "T1", analysis_id = "AGE", method = "continuous",
                          population_id = "SAF", by = "TRT01A", variables = "AGE")))
  expect_error(tfl_ard_program(sp, c("T1", "T2")), "`output_id` is one output")
  expect_error(tfl_ard_program(sp, "T9"), "No analyses for T9")
})
