# R/data_paths.R, R/spec_utils.R and R/ard_io.R: the branches no other test
# reaches -- a study folder without spec/data.yml, a data.yml that does not
# read, data with no records by each kind, the pipe asked for, a spec given
# as something else, and the ARD formats' edge cells.  Fictional data only.

# ---- data_paths.R ----------------------------------------------------------

# a study (ADSL, ADLB) with two reports on one chain of analysis data:
# lb_sub is made from lb_base, which reads ADLB with a set and a where
cov_dp_study <- function(d) {
  data <- tplfig_study(d, c("adsl", "adlb"), "PATHS-1")
  mk <- function(id) tfl_output(id, "table", data = list(
    list(id = "lb_base", from = "ADLB", population = "SAF", where = 'PARAMCD == "NOSUCH"'),
    list(id = "lb_sub", from = "lb_base")),
    ard = list(analyses = list(list(id = "LB", method = "cards::ard_tabulate",
                                    data = "lb_sub", variables = "ANRIND"))))
  for (id in c("T-A", "T-B")) {
    tfl_write_output(mk(id), file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  # a third: its data on the analysis set ALL, which has no condition
  tfl_write_output(tfl_output("T-C", "table", data = list(list(id = "lb_all", from = "ADLB", population = "ALL")),
                              ard = list(analyses = list(list(id = "LB", method = "cards::ard_tabulate",
                                                              data = "lb_all", variables = "ANRIND")))),
                   file.path(d, "spec", "outputs", "T-C.yml"))
  list(data = data, study = tfl_read_study_spec(d))
}

cov_dp_env <- function(name, value) {
  env <- new.env()
  assign(name, value, envir = env)
  env
}

test_that(".data_missing_message(only =): a missing file of a dataset not asked for is not named", {
  d <- withr_tempdir()
  ds <- list(list(dataset = "ADSL", path = "adam/adsl.xpt"),
             list(dataset = "ADAE", path = "adam/adae.xpt"))
  expect_null(.data_missing_message(ds, dir = d, only = "ADLB"))
  expect_match(.data_missing_message(ds, dir = d, only = "adae"),
               "^ADAE: no file at adam/adae.xpt")
})

test_that(".eval_study_code() runs code given as expressions, not text", {
  env <- new.env()
  .eval_study_code(expression(x <- 1 + 1), env)
  expect_identical(env$x, 2)
  # (an expression that fails: no file named, the error as it was)
  expect_error(.eval_study_code(expression(stop("plain")), new.env()), "plain")
})

test_that("no study: data with no records is not looked at", {
  expect_null(.empty_data_message(cov_dp_env("adsl", data.frame()), NULL))
})

test_that("an empty dataset read is named: 'The dataset ... has no records'", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  s <- cov_dp_study(d)
  expect_identical(.empty_data_message(cov_dp_env("adlb", s$data$adlb[0L, ]), s$study),
                   "The dataset ADLB has no records.")
})

test_that("an empty analysis set is named with its condition, or 'no condition'", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  s <- cov_dp_study(d)
  empty <- s$data$adsl[0L, ]
  expect_identical(.empty_data_message(cov_dp_env("pop_saf", empty), s$study),
                   "The analysis set SAF (SAFFL == \"Y\") selects no subjects of ADSL.")
  expect_identical(.empty_data_message(cov_dp_env("pop_all", empty), s$study),
                   "The analysis set ALL (no condition) selects no subjects of ADSL.")
})

test_that("analysis data made from analysis data: the source's set and where, and the other reports", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  s <- cov_dp_study(d)
  env <- cov_dp_env("lb_sub", s$data$adlb[0L, ])
  msg <- .empty_data_message(env, s$study)
  expect_match(msg, "The report T-A (and T-B) reads no records: its data lb_sub is empty", fixed = TRUE)
  expect_match(msg, "(dataset ADLB, analysis set SAF [SAFFL == \"Y\"], where PARAMCD == \"NOSUCH\")",
               fixed = TRUE)
  # one report asked for: the other is not named
  one <- .empty_data_message(env, s$study, output_id = "T-B")
  expect_match(one, "^The report T-B reads no records")
})

test_that("analysis data on a set with no condition: the set named without one", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  s <- cov_dp_study(d)
  msg <- .empty_data_message(cov_dp_env("lb_all", s$data$adlb[0L, ]), s$study)
  expect_match(msg, "The report T-C reads no records: its data lb_all is empty (dataset ADLB, analysis set ALL).",
               fixed = TRUE)
})

test_that("an ARD spec, not a study: no reports are read, the data is still named", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  s <- cov_dp_study(d)
  spec <- .ard_spec_of_study(s$study)$spec
  msg <- .empty_data_message(cov_dp_env("lb_sub", s$data$adlb[0L, ]), spec)
  expect_match(msg, "its data lb_sub is empty (dataset ADLB", fixed = TRUE)
})

test_that("a data set with no analysis set or dataset known says 'not known'", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("dplyr")
  d <- withr_tempdir()
  s <- cov_dp_study(d)
  spec <- .ard_spec_of_study(s$study)$spec
  # (the analysis data's rows and the analyses' dataset gone: nothing says where it is from)
  spec$analysis_data <- spec$analysis_data[0L, , drop = FALSE]
  spec$analyses$dataset <- NA_character_
  msg <- .empty_data_message(cov_dp_env("lb_sub", s$data$adlb[0L, ]), spec)
  skip_if(is.null(msg), "the spec without its analysis data names no data")
  expect_match(msg, "(dataset not known)", fixed = TRUE)
})

test_that(".data_file_must_exist(): a missing file stops naming the dataset; a Windows path is absolute", {
  d <- withr_tempdir()
  expect_error(.data_file_must_exist("ADSL", "adam/adsl.xpt", dir = d),
               "ADSL: no file at adam/adsl.xpt (spec/data.yml)", fixed = TRUE)
  expect_error(.data_file_must_exist("ADSL", "C:\\study\\adam\\adsl.xpt", dir = d),
               "ADSL: no file at C:\\study\\adam\\adsl.xpt", fixed = TRUE)
  writeLines("x", file.path(d, "adsl.rds"))
  expect_identical(.data_file_must_exist("ADSL", "adsl.rds", dir = d), "adsl.rds")
})

test_that(".ai_data_paths(): a study folder without spec/data.yml gives NULL", {
  d <- withr_tempdir()
  expect_null(.ai_data_paths(d, file.path(d, "adam")))
})

test_that(".ai_data_paths(): a data.yml that does not read, or lists no datasets, gives NULL", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  f <- file.path(d, "spec", "data.yml")
  writeLines("datasets: [a, b", f)
  expect_null(.ai_data_paths(d, d))
  writeLines("datasets: ~", f)
  expect_null(.ai_data_paths(d, d))
  writeLines("- just a list", f)
  expect_null(.ai_data_paths(d, d))
})

test_that(".ai_data_paths(): an ADaM folder that is not there gives every dataset as not given", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  writeLines(c("datasets:", "  - {dataset: ADSL, path: adam/adsl.xpt}"),
             file.path(d, "spec", "data.yml"))
  x <- .ai_data_paths(d, file.path(d, "no-such-folder"))
  expect_identical(x$dataset, "ADSL")
  expect_true(is.na(x$given))
  expect_false(x$exists)
  expect_false(x$differs)
  expect_match(.ai_data_paths_text(x), "ADSL: spec/data.yml has adam/adsl.xpt (no such file); the ADaM given is not there",
               fixed = TRUE)
})

# ---- spec_utils.R ----------------------------------------------------------

test_that(".ard_rstudio_pref(): outside RStudio there is no answer", {
  skip_if_not_installed("rstudioapi")
  testthat::local_mocked_bindings(isAvailable = function(...) FALSE, .package = "rstudioapi")
  expect_null(.ard_rstudio_pref())
})

test_that(".ard_rstudio_pref(): in RStudio, its Insert Pipe Operator preference is read", {
  skip_if_not_installed("rstudioapi")
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    readRStudioPreference = function(name, default) {
      expect_identical(name, "insert_native_pipe_operator")
      TRUE
    }, .package = "rstudioapi")
  expect_true(.ard_rstudio_pref())
})

test_that(".ard_rstudio_pref(): a preference that cannot be read is no answer", {
  skip_if_not_installed("rstudioapi")
  testthat::local_mocked_bindings(
    isAvailable = function(...) TRUE,
    readRStudioPreference = function(...) stop("no preference"), .package = "rstudioapi")
  expect_null(.ard_rstudio_pref())
})

test_that(".ard_pipe_rstudio(): TRUE is |>, FALSE %>%, anything else no answer", {
  expect_identical(.ard_pipe_rstudio(TRUE), "|>")
  expect_identical(.ard_pipe_rstudio(FALSE), "%>%")
  expect_null(.ard_pipe_rstudio(NULL))
  expect_null(.ard_pipe_rstudio(NA))
  expect_null(.ard_pipe_rstudio(c(TRUE, FALSE)))
  expect_null(.ard_pipe_rstudio("yes"))
})

test_that(".ard_pipe_op(): a pipe that is none of %>%, |>, rstudio is refused", {
  expect_error(.ard_pipe_op("->"), "`pipe` must be \"%>%\"", fixed = TRUE)
  expect_error(.ard_pipe_op(c("|>", "%>%")), "`pipe` must be", fixed = TRUE)
  withr::local_options(rtfreporter.ard_pipe = 1)
  expect_error(.ard_pipe_op(), "`pipe` must be", fixed = TRUE)
})

test_that(".ard_pipe_op(\"rstudio\"): RStudio's answer is taken", {
  testthat::local_mocked_bindings(.ard_rstudio_pref = function() TRUE)
  expect_identical(.ard_pipe_op("rstudio"), "|>")
})

test_that(".ard_pipe_op(\"rstudio\") asked with no RStudio: a message, then %>%", {
  testthat::local_mocked_bindings(.ard_rstudio_pref = function() NULL)
  expect_message(x <- .ard_pipe_op("rstudio"), "no RStudio preference to read")
  expect_identical(x, "%>%")
})

test_that(".ard_pipe_op(): \"rstudio\" set as the option is asked for too: a message, then %>%", {
  testthat::local_mocked_bindings(.ard_rstudio_pref = function() NULL)
  withr::local_options(rtfreporter.ard_pipe = "rstudio")
  expect_message(x <- .ard_pipe_op(), "no RStudio preference to read")
  expect_identical(x, "%>%")
})

test_that(".what(): NULL, and a class that starts with a vowel takes 'an'", {
  expect_identical(.what(NULL), "NULL")
  expect_identical(.what(1L), "an integer")
  expect_identical(.what(list()), "a list")
})

test_that(".as_spec(): an empty path or an NA path is refused naming what was given", {
  expect_error(.as_spec(character(), "ard", "tfl_build_ard"),
               "tfl_build_ard(): `spec` must be an ARD spec", fixed = TRUE)
  expect_error(.as_spec(NA_character_, "table", "tfl_build_tables"),
               "got text (NA)", fixed = TRUE)
})

test_that(".as_spec(): a path is read as an ARD spec workbook", {
  expect_error(.as_spec(file.path(tempdir(), "no-such-spec.xlsx"), "ard", "tfl_build_ard"),
               "no-such-spec.xlsx")
})

test_that(".as_spec(): a list that is not a table spec, and anything but a listing spec, are refused", {
  expect_error(.as_spec(list(nope = 1), "table", "tfl_build_tables"),
               "must be a table / report spec", fixed = TRUE)
  expect_error(.as_spec(list(columns = 1), "listing", "tfl_build_listing"),
               "must be a listing spec", fixed = TRUE)
  expect_error(.as_spec(data.frame(a = 1), "ard", "tfl_build_ard"),
               "got a data.frame (1 x 1)", fixed = TRUE)
})

# ---- ard_io.R --------------------------------------------------------------

# a small ARD by hand, as cards writes one (list columns), fictional numbers
cov_io_ard <- function() {
  d <- data.frame(group1 = "TRT01A", variable = c("AGE", "AGE", "SEX", "FLAG"),
                  context = "summary", stat_name = c("mean", "N", "n", "p"),
                  stat_label = c("Mean", "N", "n", "%"), stringsAsFactors = FALSE)
  d$group1_level <- list(factor("Drug X"), factor("Drug X"), factor("Drug X"), factor("Drug X"))
  d$variable_level <- list(NULL, NULL, factor("F", levels = c("M", "F")), NULL)
  d$stat <- list(54.5, 12L, 7L, 0.5)
  d$fmt_fun <- list(1L, 0L, 0L, 1L)
  d$warning <- list(NULL, NULL, NULL, NULL)
  d$error <- list(NULL, NULL, NULL, NULL)
  d
}

test_that("tfl_write_ard() with format given: case does not matter, and rds keeps the ARD as it is", {
  a <- cov_io_ard()
  f <- tempfile(fileext = ".bin")
  on.exit(unlink(f), add = TRUE)
  out <- tfl_write_ard(a, f, format = "RDS")
  expect_identical(attr(out, "lost"), character())
  expect_identical(readRDS(f), a)
  expect_identical(tfl_read_ard(f, format = "rds"), a)
})

test_that("an .rds path is written and read as rds", {
  a <- cov_io_ard()
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  expect_silent(tfl_write_ard(a, f))
  expect_identical(tfl_read_ard(f), a)
})

test_that(".ard_cell(): a function or empty is NA, a date its text, several values a list", {
  expect_true(is.na(.ard_cell(function(x) x)))
  expect_true(is.na(.ard_cell(character())))
  expect_identical(.ard_cell(as.Date("2026-01-31")), "2026-01-31")
  expect_identical(.ard_cell(c(a = 1, b = 2)), list(1, 2))
})

test_that(".ard_column_types(): number, logical, list and character", {
  d <- data.frame(n = 1.5, l = TRUE, s = "x", stringsAsFactors = FALSE)
  d$v <- list(1)
  expect_identical(.ard_column_types(d), c(n = "number", l = "logical", s = "character", v = "list"))
})

test_that("JSON rows: a factor column is written as its labels, several values come back as one", {
  a <- cov_io_ard()
  a$context <- factor(a$context)
  a$stat[[1L]] <- c(54.5, 60)
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  tfl_write_ard(a, f)
  js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_identical(js$rows[[1L]]$context, "summary")
  expect_identical(unlist(js$rows[[1L]]$stat), c(54.5, 60))
  b <- tfl_read_ard(f)
  expect_identical(b$stat[[1L]], c(54.5, 60))
  # (warning / error written as null come back as NULL)
  expect_null(b$warning[[1L]])
  expect_null(b$error[[2L]])
})

test_that("JSON rows: a warning kept in the ARD comes back as its text", {
  a <- cov_io_ard()
  a$warning[[2L]] <- "fictional warning"
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  tfl_write_ard(a, f)
  b <- tfl_read_ard(f)
  expect_identical(b$warning[[2L]], "fictional warning")
  expect_null(b$warning[[1L]])
})

test_that("YAML rows: a row with no value for a level is NULL again", {
  a <- cov_io_ard()
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tfl_write_ard(a, f)
  b <- tfl_read_ard(f)
  expect_null(b$variable_level[[1L]])
  expect_identical(levels(b$variable_level[[3L]]), c("M", "F"))
})

test_that("JSON rows: a statistic written as null reads back as NULL", {
  a <- cov_io_ard()
  a$stat[2L] <- list(NULL)
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  tfl_write_ard(a, f)
  expect_null(tfl_read_ard(f)$stat[[2L]])
})

test_that("XPT: numbers only keep stat a number; a factor column is written as text", {
  skip_if_not_installed("haven")
  a <- cov_io_ard()
  a$context <- factor(a$context)
  f <- tempfile(fileext = ".xpt")
  on.exit(unlink(f), add = TRUE)
  w <- tryCatch(tfl_write_ard(a, f), warning = function(w) conditionMessage(w))
  expect_false(grepl("the type of stat", w, fixed = TRUE))
  x <- haven::read_xpt(f)
  expect_type(x$stat, "double")
  expect_type(x$context, "character")
})

test_that("XPT: text longer than 200 characters is cut and said", {
  skip_if_not_installed("haven")
  a <- cov_io_ard()
  a$stat_label[1L] <- strrep("x", 250L)
  f <- tempfile(fileext = ".xpt")
  on.exit(unlink(f), add = TRUE)
  w <- tryCatch(tfl_write_ard(a, f), warning = function(w) conditionMessage(w))
  expect_match(w, "text longer than 200 characters (stat_label) is cut", fixed = TRUE)
  expect_identical(nchar(haven::read_xpt(f)$stat_label[1L]), 200L)
})

test_that("a CSV is read as a flat table: blanks are NULL, numbers numbers", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  utils::write.csv(data.frame(VARIABLE = c("AGE", "SEX"), VARLVL = c("", "F"),
                              STATNAME = c("mean", "n"), STAT = c("54.5", "")),
                   f, row.names = FALSE)
  b <- tfl_read_ard(f)
  expect_identical(b$variable, c("AGE", "SEX"))
  expect_null(b$variable_level[[1L]])
  expect_identical(b$variable_level[[2L]], "F")
  expect_identical(b$stat[[1L]], 54.5)
  expect_null(b$stat[[2L]])
  # format = "csv" for a file with another extension
  g <- tempfile(fileext = ".txt")
  on.exit(unlink(g), add = TRUE)
  file.copy(f, g)
  expect_identical(tfl_read_ard(g, format = "csv")$stat_name, c("mean", "n"))
})
