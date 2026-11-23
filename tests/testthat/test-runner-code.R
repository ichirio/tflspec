# The runner of the official runs (G4, slice M): tflspec ships the base-R file
# and checks the manifest of the study's programs that goes in front of it

runner_manifest <- function() {
  list(
    root = "runs",
    ard = "output/ard/ard.rds",
    programs = list(
      ard = c("programs/ard/ard_t_demo.R", "programs/ard/ard_t_ae.R"),
      tfl = c("programs/tfl/t_demo.R", "programs/tfl/t_ae.R")),
    ard_outputs = c(ard_t_demo.R = "T-1", ard_t_ae.R = "T-2"),
    report_ids = c(`programs/tfl/t_demo.R` = "T-1",
                   `programs/tfl/t_ae.R` = "T-2"),
    needs = list(`programs/tfl/t_ae.R` = "T-1"),
    unresolved = c(`programs/tfl/t_demo.R` = "no source"),
    sets = list(main = c("T-1", "T-2")),
    outputs = list(`programs/tfl/t_demo.R` = "output/tfl/t_demo.rtf",
                   `programs/tfl/t_ae.R` = c("output/tfl/t_ae.rtf",
                                             "output/tfl/t_ae.pdf")),
    support = list(ard = "programs/ard/setup.R",
                   tfl = c("programs/tfl/setup.R", "spec/study.yml"),
                   all = character()),
    data = list(`programs/ard/ard_t_demo.R` = "data/adsl.csv",
                `programs/ard/ard_t_ae.R` = character()))
}

test_that("tfl_runner_file() is the shipped runner", {
  f <- tfl_runner_file()
  expect_true(nzchar(f))
  expect_true(file.exists(f))
})

test_that("tfl_runner_code(): the head, then the runner's lines", {
  code <- tfl_runner_code(runner_manifest())
  runner <- readLines(tfl_runner_file(), warn = FALSE, encoding = "UTF-8")
  n <- length(code) - length(runner)
  expect_gt(n, 10L)
  expect_identical(utils::tail(code, length(runner)), runner)
  head_lines <- code[seq_len(n)]
  expect_match(head_lines[[1L]], "^# ---- the study's programs")
  expect_identical(head_lines[[2L]], ".batch_root <- \"runs\"")
  expect_identical(head_lines[[3L]], ".batch_ard  <- \"output/ard/ard.rds\"")
  expect_identical(head_lines[[n]], "")
  for (nm in c("programs", "ard_outputs", "report_ids", "needs", "unresolved", "sets",
               "outputs", "support", "data")) {
    expect_true(any(startsWith(head_lines, paste0(".batch_", nm, " <- "))), nm)
  }
  # no call line: the caller adds it
  expect_false(any(grepl("^run_batch[(]", code)))
})

test_that("tfl_runner_code(): the head is the values, as R", {
  code <- tfl_runner_code(runner_manifest())
  env <- new.env()
  head_end <- which(code == "")[[1L]]
  eval(parse(text = code[seq_len(head_end)]), env)
  m <- runner_manifest()
  expect_identical(env$.batch_root, m$root)
  expect_identical(env$.batch_programs, m$programs)
  expect_identical(env$.batch_ard_outputs, m$ard_outputs)
  expect_identical(env$.batch_report_ids, m$report_ids)
  expect_identical(env$.batch_needs, m$needs)
  expect_identical(env$.batch_unresolved, m$unresolved)
  expect_identical(env$.batch_sets, m$sets)
  expect_identical(env$.batch_outputs, m$outputs)
  expect_identical(env$.batch_support, m$support)
  expect_identical(env$.batch_data, m$data)
})

test_that("tfl_runner_code(): a study with no programs still gives the head", {
  m <- runner_manifest()
  m$programs <- list(ard = character(), tfl = character())
  m$ard_outputs <- character()
  m$report_ids <- character()
  m$needs <- list()
  m$unresolved <- character()
  m$sets <- list()
  m$outputs <- list()
  m$data <- list()
  code <- tfl_runner_code(m)
  expect_true(".batch_report_ids <- character()" %in% code)
  expect_true(".batch_needs <- list()" %in% code)
})

test_that("tfl_runner_code(): an empty element stays character()", {
  m <- runner_manifest()
  m$sets$none <- character()
  m$outputs[["programs/tfl/t_demo.R"]] <- character()
  code <- tfl_runner_code(m)
  env <- new.env()
  eval(parse(text = code[seq_len(which(code == "")[[1L]])]), env)
  expect_identical(env$.batch_sets, m$sets)
  expect_identical(env$.batch_outputs, m$outputs)
  expect_identical(env$.batch_data, m$data)
})

test_that("tfl_runner_code(): two ARD programs with one file name stop", {
  m <- runner_manifest()
  m$programs$ard[[2L]] <- "programs/ard/sub/ard_t_demo.R"
  expect_error(tfl_runner_code(m), "manifest[$]programs`.*ard_t_demo")
})

test_that("tfl_runner_code(): a missing field stops, naming it", {
  for (f in names(runner_manifest())) {
    m <- runner_manifest()
    m[[f]] <- NULL
    expect_error(tfl_runner_code(m), paste0("manifest[$]", f, "`"), info = f)
  }
  expect_error(tfl_runner_code("runs"), "named list")
  expect_error(tfl_runner_code(c(runner_manifest(), list(extra = 1))),
               "manifest[$]extra`")
})

test_that("tfl_runner_code(): a field of the wrong type stops, naming it", {
  wrong <- list(
    root = 1, ard = c("a", "b"), programs = list(ard = "a"),
    ard_outputs = c("T-1"), report_ids = list("T-1"), needs = "T-1",
    unresolved = c("x"),
    sets = list("T-1"), outputs = c(a = "b"), support = list(ard = "a"),
    data = 1)
  for (f in names(wrong)) {
    m <- runner_manifest()
    m[[f]] <- wrong[[f]]
    expect_error(tfl_runner_code(m), paste0("manifest[$]", f, "`"), info = f)
  }
  m <- runner_manifest()
  m$programs$tfl <- 1:2
  expect_error(tfl_runner_code(m), "manifest[$]programs`")
  m <- runner_manifest()
  m$needs[[1L]] <- 1
  expect_error(tfl_runner_code(m), "manifest[$]needs`")
  m <- runner_manifest()
  m$root <- NA_character_
  expect_error(tfl_runner_code(m), "manifest[$]root`")
})

test_that("tfl_runner_code(): the names must agree", {
  m <- runner_manifest()
  names(m$needs) <- "programs/tfl/t_other.R"
  expect_error(tfl_runner_code(m), "manifest[$]needs`.*t_other")
  m <- runner_manifest()
  names(m$unresolved) <- "programs/tfl/t_other.R"
  expect_error(tfl_runner_code(m), "manifest[$]unresolved`.*t_other")
  m <- runner_manifest()
  names(m$ard_outputs)[[1L]] <- "ard_t_other.R"
  expect_error(tfl_runner_code(m), "manifest[$]ard_outputs`.*ard_t_other")
  m <- runner_manifest()
  m$report_ids <- m$report_ids[1L]
  expect_error(tfl_runner_code(m), "manifest[$]report_ids`.*t_ae")
  m <- runner_manifest()
  m$outputs <- m$outputs[2L]
  expect_error(tfl_runner_code(m), "manifest[$]outputs`.*t_demo")
})

test_that("the runner file is base R", {
  pd <- utils::getParseData(parse(tfl_runner_file(), keep.source = TRUE))
  pkg <- unique(pd$text[pd$token == "SYMBOL_PACKAGE"])
  expect_true(all(pkg %in% c("utils", "tools", "stats")), info = toString(pkg))
  calls <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]
  # (loadNamespace() only in session_info.txt: the versions of the packages
  # the run's programs used, read after they ran)
  expect_false(any(calls %in% c("library", "require")))
  # requireNamespace() only asks whether an optional engine is there (logrx)
  rn <- grep("requireNamespace(", readLines(tfl_runner_file()), fixed = TRUE, value = TRUE)
  expect_true(all(grepl("requireNamespace(\"logrx\"", rn, fixed = TRUE)), info = toString(rn))
  expect_false(any(grepl("^(tfl|tflspec|tflplanner)", pkg)))
  # no call of tflspec's or tflplanner's functions
  expect_false(any(grepl("^(tfl_|tflplanner)", calls)))
})

test_that("tfl_runner_code() parses", {
  expect_no_error(parse(text = tfl_runner_code(runner_manifest())))
})
