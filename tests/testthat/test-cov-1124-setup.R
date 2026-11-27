# Branches of the study's setup and program files (G5-1, G5-3) that no other
# test reaches: the program writer's back-up and states, the company's
# setup seed with no setup code, the attached packages of odd calls, a
# copied template whose retired source is gone, the defaults.yml blocks
# kept, a report's own value read by a template.  Fictional data only.

# a program as the makers write it: a banner with its Generated line
cs_program <- function(body = "x <- 1") {
  c("# ==========", "#  Program    : programs/ard/T-1.R",
    "#  Generated  : tflspec 0.0.0, 2026-01-01", "# ==========", "", body)
}

# ---- the program writer (R/program_writer.R) ----------------------------------

test_that("a new program is written, an untouched one is left, an edited one is rewritten", {
  root <- withr_tempdir()
  f <- file.path(root, "programs", "ard", "T-1.R")
  expect_identical(.put_program(cs_program(), f, root), "written")
  expect_true(any(startsWith(readLines(f), "#  Checksum   :")))
  # (the same code again, the file as it was written: from the cache)
  expect_identical(.put_program(cs_program(), f, root), "unchanged")
  # (the cache forgotten: the file read and found current)
  rm(list = f, envir = .put_cache)
  expect_identical(.put_program(cs_program(), f, root), "unchanged")
  # a new definition over the untouched file
  expect_identical(.put_program(cs_program("x <- 2"), f, root), "written")
  # edited by hand: copied to programs/.edited/ first
  writeLines(c(readLines(f), "y <- 3"), f)
  expect_identical(.put_program(cs_program("x <- 2"), f, root), "rewritten")
  kept <- list.files(file.path(root, "programs", ".edited"))
  expect_length(kept, 1L)
  expect_match(kept, "^ard_T-1_[0-9]{8}-[0-9]{6}[.]R$")
})

test_that("an edited program outside the study folder is backed up by its file name", {
  root <- withr_tempdir()
  other <- withr_tempdir()
  f <- file.path(other, "T-9.R")
  writeLines(c(cs_program(), "# edited"), f)
  dest <- .back_up_edited(f, root)
  expect_identical(dirname(dest), file.path(root, "programs", ".edited"))
  expect_match(basename(dest), "^T-9_[0-9]{8}-[0-9]{6}[.]R$")
  expect_true(file.exists(dest))
})

# ---- the setup programs and values ------------------------------------------------

test_that("company_setup.R with no setup code of the company: the header alone", {
  code <- tfl_setup_program("company_setup", meta = list(study_id = "STUDY-X"),
                            standards = list(setup_code = NULL), date = as.Date("2026-01-01"))
  expect_match(code[1L], "^# The company's code \\(")
  expect_identical(code[length(code)], "")
  expect_length(code, 4L)
  # (tflspec's own setup code, the study's id in it)
  own <- tfl_setup_program("company_setup", meta = list(study_id = "STUDY-X"), date = as.Date("2026-01-01"))
  expect_gte(length(own), 4L)
})

test_that("the attached packages: base::library() is read, a call of a call is not", {
  v <- tfl_setup_values(c("base::library(dplyr)", "f <- function() identity", "f()(require(tidyr))"))
  expect_identical(v$attached, c("dplyr", "tidyr"))
})

# ---- a copied template whose source was retired ---------------------------------------

test_that("a copy of a retired template whose source is gone: the stop says it was retired", {
  root <- withr_tempdir()
  st <- file.path(root, "STUDY-R")
  dir.create(st)
  writeLines(c("study_id: STUDY-R", "settings:", "  rounding: sas"), file.path(st, "study.yml"))
  suppressMessages(invisible(tfl_copy_templates(st, "tflspec_v1/tflspec_summary")))
  sd <- file.path(st, "spec", "series", "tflspec_v1")
  # (the copy made, in an earlier tflspec, from tflspec_v1/demog)
  file.rename(file.path(sd, "templates", "tflspec_summary.yml"), file.path(sd, "templates", "demog.yml"))
  cp <- readLines(file.path(sd, "copied.yml"))
  cp <- sub("^templates/tflspec_summary.yml:", "templates/demog.yml:", cp)
  cp <- sub("template: tflspec_summary$", "template: demog", cp)
  writeLines(cp, file.path(sd, "copied.yml"))
  err <- tryCatch(tfl_template_diff(st, "tflspec_v1/demog"), error = conditionMessage)
  expect_match(err, "the source of template tflspec_v1/demog is missing", fixed = TRUE)
  expect_match(err, "tflspec_v1/demog was retired from the series (2026-11-24); use tflspec_v1/tflspec_summary instead.",
               fixed = TRUE)
})

# ---- defaults.yml written over: the study's other blocks kept ---------------------------

test_that("defaults.yml written over keeps the study's other blocks, by the styles", {
  old <- c("# the code lists", "codelists:", "  SEX: [F, M]", "# the styles",
           "styles:", "  tfl_standard: {}", "# the options", "options:", "  digits: 1")
  new <- c("content:", "  - table_plan: {}", "styles:", "  tfl_standard: {}")
  expect_identical(.defaults_keep_blocks(new, old),
                   c("content:", "  - table_plan: {}", "# the code lists", "codelists:", "  SEX: [F, M]",
                     "styles:", "  tfl_standard: {}", "# the options", "options:", "  digits: 1"))
})

test_that("defaults.yml with no styles: the kept blocks at the end", {
  old <- c("adam_meta:", "  ADSL: {}", "figure_style:", "  font: Arial")
  new <- c("content:", "  - table_plan: {}")
  expect_identical(.defaults_keep_blocks(new, old),
                   c(new, "adam_meta:", "  ADSL: {}", "figure_style:", "  font: Arial"))
  # (a key the writer writes is its, not kept twice)
  expect_identical(.defaults_keep_blocks(c("codelists:", "  SEX: [M]"), c("codelists:", "  SEX: [F]")),
                   c("codelists:", "  SEX: [M]"))
})

test_that("tfl_write_study_part() over the study's defaults.yml keeps its code lists block", {
  st <- withr_tempdir()
  dir.create(file.path(st, "spec"))
  writeLines(c("study_id: STUDY-D"), file.path(st, "study.yml"))
  writeLines(c("content:", "  - table_plan: {}", "# the study's own", "options:", "  digits: 2"),
             file.path(st, "spec", "defaults.yml"))
  s <- tfl_read_study_spec(st)
  p <- tfl_write_study_part(s, "defaults")
  got <- readLines(p)
  expect_true(all(c("# the study's own", "options:", "  digits: 2") %in% got))
})

# ---- a template reads a report's own value -------------------------------------------

test_that("a template's output.<key> reads the report's own value, none when it has none", {
  o <- tfl_output("T-1", "table", label = "Demographics")
  expect_identical(.tpl_study_read1("output.label", output = o), "Demographics")
  expect_null(.tpl_study_read1("output.label", output = NULL))
  expect_null(.tpl_study_read1("output.page.missing", output = o))
})
