# PROTOTYPE (c/proto-programs): a report's full program and its ARD program
# from one maker, the reports' own ARD files and the run choice.  Today's
# programs are the golden fixtures' (test-report-programs.R): the switch off
# leaves them as they are.

pp_fx_in <- function(study) {
  x <- readRDS(test_path("fixtures", "programs-family", "in", paste0(study, ".rds")))
  if (!is.null(x$study) && isTRUE(x$study_dir)) {
    x$study$dir <- test_path("fixtures", "programs-family", "study", study)
  }
  x
}
pp_prog <- function(x, id, kind) {
  tfl_onepass_program(x$reports[[id]], kind = kind, spec = x$ard$spec,
                      codelists = x$ard$codelists,
                      dir = test_path("fixtures", "programs-family", "study", "sample"),
                      study = x$study, shared = x$shared, standards = x$standards,
                      attached = x$attached, study_code = x$study_code, layout = x$layout,
                      date = x$date, generator = "tflspec 0.0.0")
}
# a program's lines, one element a line
pp_lines <- function(code) unlist(strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE))
# the analysis step's lines
pp_step <- function(code) {
  l <- pp_lines(code)
  a <- match(tflspec:::.op_begin, l)
  b <- match(tflspec:::.op_end, l)
  if (is.na(a) || is.na(b)) return(character())
  l[a:b]
}

test_that("the full and the ARD program run the same analysis lines, tfl_ard_code()'s", {
  x <- pp_fx_in("sample")
  for (id in c("T-14-1-1", "T-14-3-1", "F-14-2-4")) {
    full <- pp_step(pp_prog(x, id, "full"))
    ard <- pp_step(pp_prog(x, id, "ard"))
    expect_gt(length(full), 10L)
    expect_identical(full, ard, info = id)
  }
})

test_that("a full program makes its ARD: no row of the study ARD is picked, it is saved", {
  x <- pp_fx_in("sample")
  l <- pp_lines(pp_prog(x, "T-14-1-1", "full"))
  expect_false(any(grepl("ard.rds", l, fixed = TRUE)))
  expect_false(any(grepl("first.\")", l, fixed = TRUE)))
  expect_true(any(startsWith(l, "save_report_ard(ard, report_id, definition = \"")))
  # its old file removed first, then saved right after the analysis step
  expect_lt(match("save_report_ard(NULL, \"T-14-1-1\")", l),
            match(tflspec:::.op_begin, l))
  expect_identical(l[match(tflspec:::.op_end, l) + 1L],
                   l[startsWith(l, "save_report_ard(ard,")][1L])
  # the table reads it without its keys, then its plan as today
  expect_true("ard <- select(ard, -c(output_id, analysis_id, population_id))" %in% l)
  expect_true(any(grepl("^plan <- ard \\|>", l)))
})

test_that("a figure reading a table's ARD reads that table's file", {
  x <- pp_fx_in("sample")
  l <- pp_lines(pp_prog(x, "F-14-2-2", "full"))
  expect_true("ard <- read_report_ard(\"T-14-2-2\")" %in% l)
  expect_true("ard_built <- report_ard_definition(\"T-14-2-2\")" %in% l)
  expect_true("record_report(report_id, ard = ard_built)" %in% l)
  expect_false(any(grepl("ard.rds", l, fixed = TRUE)))
  # (it makes no ARD of its own: no ARD program)
  expect_null(pp_prog(x, "F-14-2-2", "ard"))
})

test_that("a figure with its own ARD records the definition it made it from", {
  x <- pp_fx_in("sample")
  l <- pp_lines(pp_prog(x, "F-14-2-4", "full"))
  h <- tfl_ard_spec_hash(x$ard$spec, "F-14-2-4",
                         test_path("fixtures", "programs-family", "study", "sample"),
                         x$ard$codelists)
  expect_true(paste0("ard_built <- \"", h, "\"") %in% l)
  expect_true(paste0("save_report_ard(ard, report_id, definition = \"", h,
                     "\", program = \"programs/tfl/F-14-2-4.R\")") %in% l)
})

test_that("a listing makes no ARD; its full program removes an old file and saves none", {
  x <- pp_fx_in("sample")
  expect_null(pp_prog(x, "L-16-2-7", "ard"))
  l <- pp_lines(pp_prog(x, "L-16-2-7", "full"))
  expect_true("save_report_ard(NULL, \"L-16-2-7\")" %in% l)
  expect_false(any(startsWith(l, "save_report_ard(ard")))
  expect_false(any(grepl("ard_setup.R", l, fixed = TRUE)))
})

test_that("the `ard` a step of the report's own code leaves joins the study ARD", {
  x <- pp_fx_in("edited")
  x$ard$codelists <- NULL
  for (id in c("T-91", "T-92")) {
    l <- pp_lines(pp_prog(x, id, "full"))
    expect_true("if (exists(\"ard\", inherits = FALSE) && is.data.frame(ard)) {" %in% l, info = id)
    a <- pp_lines(pp_prog(x, id, "ard"))
    # (the ARD program: the whole step of the report's code, no content, no page)
    expect_true("ard <- cards::ard_continuous(adsl, variables = AGE)" %in% a, info = id)
    expect_false(any(grepl("generate_rtfreport", a, fixed = TRUE)), info = id)
    expect_true(any(grepl("ard_setup.R", a, fixed = TRUE)), info = id)
  }
  # a step of its own code that leaves no `ard` and no analyses: no ARD
  # program only when the report has no code before its content
  expect_null(pp_prog(x, "F-14-2-1", "ard"))
})

test_that("an imported ARD is read as today and not saved", {
  x <- pp_fx_in("edited")
  x$ard$codelists <- NULL
  l <- pp_lines(pp_prog(x, "T-93", "full"))
  expect_true("ard <- tflspec::tfl_read_ard(\"input/ard/t93.rds\")" %in% l)
  expect_false(any(startsWith(l, "save_report_ard(ard")))
})

test_that("with the switch off the makers write today's text", {
  x <- pp_fx_in("sample")
  r <- x$reports[["T-14-1-1"]]
  code <- tfl_report_program(r, study = x$study, shared = x$shared, standards = x$standards,
                             attached = x$attached, study_code = x$study_code,
                             layout = x$layout, date = x$date)
  l <- pp_lines(code)
  expect_false(any(grepl("save_report_ard|read_report_ard|local\\(\\{", l)))
  expect_true(any(grepl("readRDS(file.path(path_ard, \"ard.rds\"))", l, fixed = TRUE)))
})

test_that("the analysis step's `data =` does not decide the table's normalization", {
  l <- c("x <- 1", tflspec:::.op_begin, "data <- 2", "f(data = y)", "ard",
         tflspec:::.op_end, "z <- 3")
  expect_identical(tflspec:::.op_outside_analysis(l), c("x <- 1", "z <- 3"))
  expect_false(tflspec:::.rp_makes_data(paste(l, collapse = "\n")))
  expect_true(tflspec:::.rp_makes_data("data <- normalize_ard(ard)"))
})

test_that("a report runs after the reports whose ARD it reads; a cycle stops", {
  o <- tflspec:::.op_run_order(c("F-1", "T-1", "L-1", "T-2"),
                               list(`F-1` = c("T-1", "T-2")))
  expect_identical(o, c("T-1", "T-2", "F-1", "L-1"))
  # (a read of an output not in the run is left to its program)
  expect_identical(tflspec:::.op_run_order("F-1", list(`F-1` = "T-9")), "F-1")
  expect_error(tflspec:::.op_run_order(c("A", "B"), list(A = "B", B = "A")),
               "read each other's ARD: A -> B -> A")
})

test_that("the reports' ARD files: saved with what made them, read, collected", {
  skip_if_not_installed("dplyr")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  e <- new.env()
  sys.source(system.file("helpers", "onepass_helpers.R", package = "tflspec"), envir = e)
  withr::local_options(tflplanner.run = "run1")
  a1 <- data.frame(output_id = "T-1", stat_name = c("n", "N"), stat = I(list(1, 2)))
  capture.output(e$save_report_ard(a1, "T-1", definition = "abc", program = "programs/tfl/T-1.R"))
  capture.output(e$save_report_ard(data.frame(stat_name = "n", stat = I(list(3))), "T/2"))
  expect_true(file.exists("output/ard/report/T-1.rds"))
  expect_true(file.exists("output/ard/report/T_2.rds"))
  expect_identical(attr(readRDS("output/ard/report/T-1.rds"), "made")$run, "run1")
  expect_identical(e$report_ard_definition("T-1"), "abc")
  expect_identical(e$report_ard_definition("T-9"), "")
  expect_identical(nrow(e$read_report_ard("T-1")), 2L)
  expect_null(attr(e$read_report_ard("T-1"), "made"))
  expect_error(e$read_report_ard("T-9"), "There is no ARD of T-9")
  capture.output(ard <- e$collect_ard(c("T-1", "T/2")))
  expect_identical(ard$output_id, c("T-1", "T-1", "T/2"))
  expect_identical(nrow(readRDS("output/ard/ard.rds")), 3L)
  st <- utils::read.csv("output/ard/ard_status.csv", colClasses = "character")
  expect_identical(st$output_id, c("T-1", "T/2"))
  expect_identical(st$program, c("programs/tfl/T-1.R", ""))
  expect_identical(st$rows, c("2", "1"))
  # NULL: the report makes no ARD now, its file is removed
  e$save_report_ard(NULL, "T/2")
  expect_false(file.exists("output/ard/report/T_2.rds"))
  expect_error(e$save_report_ard(list(1), "T-1"), "is a data frame")
})

test_that("the run choice: full and both run the full programs, ard the ARD programs", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "programs", "tfl"), recursive = TRUE)
  dir.create(file.path(d, "programs", "ard"), recursive = TRUE)
  file.copy(system.file("helpers", "onepass_helpers.R", package = "tflspec"),
            file.path(d, "programs", "onepass_helpers.R"))
  prog <- function(id, kind, body = NULL) {
    f <- file.path("programs", kind, paste0(id, ".R"))
    writeLines(c("source('programs/onepass_helpers.R')", body,
                 sprintf("cat('%s', file = 'ran.txt', append = TRUE, sep = '\\n')", f),
                 sprintf("save_report_ard(data.frame(stat_name = '%s'), '%s', program = '%s')",
                         kind, id, f)),
               file.path(d, f))
    f
  }
  p <- data.frame(output_id = c("F-1", "T-1"),
                  full = c(prog("F-1", "tfl", "a <- read_report_ard('T-1')"), prog("T-1", "tfl")),
                  ard = c(NA, prog("T-1", "ard")))
  ran <- function() readLines(file.path(d, "ran.txt"))
  r <- tfl_onepass_run(d, p, "full", needs = list(`F-1` = "T-1"))
  expect_identical(r$output_id, c("T-1", "F-1"))
  expect_identical(r$status, c(0L, 0L))
  expect_identical(attr(r, "collect")$status, 0L)
  expect_identical(ran(), c("programs/tfl/T-1.R", "programs/tfl/F-1.R"))
  expect_identical(readRDS(file.path(d, "output", "ard", "ard.rds"))$stat_name, c("tfl", "tfl"))
  unlink(file.path(d, "ran.txt"))
  tfl_onepass_run(d, p, "both", needs = list(`F-1` = "T-1"))
  expect_identical(ran(), c("programs/tfl/T-1.R", "programs/tfl/F-1.R"))
  unlink(file.path(d, c("ran.txt", "output")), recursive = TRUE)
  r <- tfl_onepass_run(d, p, "ard")
  expect_identical(r$program, "programs/ard/T-1.R")
  expect_identical(readRDS(file.path(d, "output", "ard", "ard.rds"))$stat_name, "ard")
  # a reader whose producer failed is not run
  writeLines("stop('no')", file.path(d, "programs", "tfl", "T-1.R"))
  unlink(file.path(d, "ran.txt"))
  r <- tfl_onepass_run(d, p, "full", needs = list(`F-1` = "T-1"))
  expect_true(r$status[r$output_id == "T-1"] != 0L)
  expect_true(is.na(r$status[r$output_id == "F-1"]))
  expect_false(file.exists(file.path(d, "ran.txt")))
})

test_that("a study with no ARD definition: a step of its own code is its ARD program", {
  x <- pp_fx_in("sheets")
  ids <- names(x$reports)
  own <- ids[vapply(x$reports, function(r) !is.na(r$steps$data %||% NA), NA)]
  for (id in ids) {
    a <- tfl_onepass_program(x$reports[[id]], kind = "ard", spec = NULL, study = x$study,
                             shared = x$shared, standards = x$standards, attached = x$attached,
                             study_code = x$study_code, layout = x$layout, date = x$date,
                             generator = "tflspec 0.0.0")
    if (id %in% own) {
      expect_false(any(grepl("^Analyses", pp_lines(a))), info = id)
    } else {
      expect_null(a, info = id)
    }
    f <- tfl_onepass_program(x$reports[[id]], kind = "full", spec = NULL, study = x$study,
                             shared = x$shared, standards = x$standards, attached = x$attached,
                             study_code = x$study_code, layout = x$layout, date = x$date,
                             generator = "tflspec 0.0.0")
    expect_true(paste0("save_report_ard(NULL, \"", id, "\")") %in% pp_lines(f), info = id)
  }
})
