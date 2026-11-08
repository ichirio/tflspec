# The study's sheets are read once while its files stay as they are
# (R/sheets_memo.R): tfl_data_code() of each report of a study used to run the
# bridge again for each one.

.memo_study <- function(env = parent.frame()) {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(.local_envir = env), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)), file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SM-3")
  d
}

.memo_listing <- function(id = "L-LB") {
  tfl_output(id, "listing", population = "SAF",
    data = list(source = "dataset", dataset = "ADLB", population = "SAF"),
    content = list(tfl_item("plan_listing", list(cols = c("USUBJID", "AVISIT", "AVAL")))))
}

.memo_runs <- function() tflspec:::.sheets_memo$runs

test_that("the bridge runs once for two tfl_data_code() calls on an unchanged study", {
  d <- .memo_study()
  tflspec:::.sheets_memo_clear()
  st <- tfl_read_study_spec(d)
  n0 <- .memo_runs()
  a <- tfl_data_code(.memo_listing("L-1"), st)
  expect_identical(.memo_runs(), n0 + 1L)
  b <- tfl_data_code(.memo_listing("L-2"), st)
  # (the study as its folder too: read again, the same files)
  a2 <- tfl_data_code(.memo_listing("L-1"), d)
  expect_identical(.memo_runs(), n0 + 1L)
  expect_identical(a2, a)
  expect_true(any(grepl("^data <- ", a$data)))
  expect_named(b, names(a))
})

test_that("the memo gives what the bridge gives", {
  d <- .memo_study()
  tflspec:::.sheets_memo_clear()
  st <- tfl_read_study_spec(d)
  s1 <- tflspec:::.sheets_of_study(d)
  s2 <- tflspec:::.sheets_of_study(d)
  expect_identical(s2, s1)
  expect_identical(s1, tfl_sheets_from_yaml(d))
})

test_that("the bridge runs again after a file changes, the size and the time kept too", {
  d <- .memo_study()
  tflspec:::.sheets_memo_clear()
  st <- tfl_read_study_spec(d)
  o <- .memo_listing()
  tfl_data_code(o, st)
  n1 <- .memo_runs()
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1)
  # a file changed: a longer one
  f <- file.path(d, "spec", "defaults.yml")
  txt <- readLines(f, warn = FALSE)
  writeLines(c(txt, "# a note"), f)
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 1L)
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 1L)
  # a file changed to the same size, its modification time put back
  f <- file.path(d, "study.yml")
  before <- file.info(f)
  x <- readBin(f, "raw", before$size)
  at <- gregexpr("SM-3", rawToChar(x), fixed = TRUE)[[1]]
  expect_gt(at[1], 0L)
  x[at[1] + 3L] <- charToRaw("4")
  writeBin(x, f)
  Sys.setFileTime(f, before$mtime)
  expect_identical(file.info(f)$size, before$size)
  expect_lt(abs(as.numeric(file.info(f)$mtime) - as.numeric(before$mtime)), 0.01)
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 2L)
  # a report file added
  tfl_write_output(.memo_listing("L-NEW"), file.path(d, "spec", "outputs", "L-NEW.yml"))
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 3L)
})

test_that("the bridge runs again when tflspec.outputs_dir changes", {
  d <- .memo_study()
  tflspec:::.sheets_memo_clear()
  st <- tfl_read_study_spec(d)
  o <- .memo_listing()
  tfl_data_code(o, st)
  n1 <- .memo_runs()
  # (another folder of reports, with one report in it)
  dir.create(file.path(d, "drafts"))
  tfl_write_output(.memo_listing("L-DRAFT"), file.path(d, "drafts", "L-DRAFT.yml"))
  withr::local_options(tflspec.outputs_dir = "drafts")
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 1L)
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 1L)
  withr::local_options(tflspec.outputs_dir = NULL)
  tfl_data_code(o, st)
  expect_identical(.memo_runs(), n1 + 2L)
})

test_that("the memo keeps the last few studies, and a failing read is not kept", {
  d1 <- .memo_study()
  d2 <- .memo_study()
  tflspec:::.sheets_memo_clear()
  tflspec:::.sheets_of_study(d1)
  tflspec:::.sheets_of_study(d2)
  n <- .memo_runs()
  tflspec:::.sheets_of_study(d1)
  tflspec:::.sheets_of_study(d2)
  expect_identical(.memo_runs(), n)
  expect_lte(length(tflspec:::.sheets_memo$entries), 3L)
  # (a failing read is not kept)
  expect_error(tflspec:::.sheets_of_study(file.path(d1, "nowhere")))
  expect_identical(.memo_runs(), n)
})

test_that("a template variant registered in the session is a miss", {
  d <- .memo_study()
  tflspec:::.sheets_memo_clear()
  tflspec:::.sheets_of_study(d)
  n <- .memo_runs()
  from <- tfl_templates()$name[1]
  nm <- "memo_test_variant"
  on.exit(rm(list = nm, envir = tflspec:::.tpl_registry), add = TRUE)
  suppressMessages(tfl_template_variant(nm, from = from))
  tflspec:::.sheets_of_study(d)
  expect_identical(.memo_runs(), n + 1L)
  tflspec:::.sheets_of_study(d)
  expect_identical(.memo_runs(), n + 1L)
})
