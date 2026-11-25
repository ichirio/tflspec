# More coverage of R/runner.R: the whole manifest written as R and read back,
# the field list, the preview's paths and the runner file the code is joined
# to.  Fictional data only; nothing is written outside tempdir().

rcv_manifest <- function() {
  list(
    root = "runs", ard = "output/ard/ard.rds", study = "S-1", spec = "spec",
    programs = list(ard = c("programs/ard/ard_a.R", "programs/ard/ard_b.R"),
                    tfl = c("programs/tfl/t_a.R", "programs/tfl/t_b.R")),
    ard_outputs = c(ard_a.R = "T-A", ard_b.R = "T-B"),
    report_ids = c(`programs/tfl/t_a.R` = "T-A", `programs/tfl/t_b.R` = "T-B"),
    needs = list(`programs/tfl/t_b.R` = c("T-A", "T-C")),
    unresolved = c(`programs/tfl/t_a.R` = "no source"),
    sets = list(main = c("T-A", "T-B")),
    outputs = list(`programs/tfl/t_a.R` = "output/tfl/t_a.rtf",
                   `programs/tfl/t_b.R` = c("output/tfl/t_b.rtf", "output/tfl/t_b.pdf")),
    support = list(ard = "programs/ard/setup.R",
                   tfl = c("programs/tfl/setup.R", "spec/study.yml"),
                   all = "renv.lock"),
    data = list(`programs/ard/ard_a.R` = c("data/adam/adsl.csv", "data/adam/adae.csv")),
    data_vars = c(`data/adam/adsl.csv` = "path_adam", `data/other/x.csv` = ""),
    data_dirs = c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other"),
    sources = c("programs/ard/functions/f.R", "input/x.csv"),
    check_code = c("programs/company_setup.R", "programs/study_user.R"),
    kind = "run", paths = list())
}

# the lines of the code before the runner's own (the head), evaluated
rcv_head <- function(m) {
  code <- tfl_runner_code(m)
  env <- new.env(parent = baseenv())
  eval(parse(text = code[seq_len(which(code == "")[[1L]] - 1L)]), envir = env)
  env
}

test_that("the manifest's field list is the fields of a valid manifest, nothing more", {
  expect_setequal(.runner_fields, names(rcv_manifest()))
  expect_length(.runner_fields, 19L)
  expect_false(anyDuplicated(.runner_fields) > 0L)
})

test_that("a valid manifest passes the check and gives TRUE without printing", {
  expect_invisible(.runner_check(rcv_manifest()))
  expect_true(.runner_check(rcv_manifest()))
})

test_that("a stop from the manifest check starts with the function's name and has no call", {
  m <- rcv_manifest()
  m$root <- NULL
  expect_error(tfl_runner_code(m), "^tfl_runner_code\\(\\): `manifest\\$root` is missing\\.$")
  err <- tryCatch(tfl_runner_code(m), error = function(e) e)
  expect_null(conditionCall(err))
})

test_that("every field of the manifest is written to the head and reads back as given", {
  m <- rcv_manifest()
  env <- rcv_head(m)
  expect_identical(env$.batch_root, m$root)
  expect_identical(env$.batch_ard, m$ard)
  expect_identical(env$.batch_study, m$study)
  expect_identical(env$.batch_spec, m$spec)
  expect_identical(env$.batch_programs, m$programs)
  expect_identical(env$.batch_ard_outputs, m$ard_outputs)
  expect_identical(env$.batch_report_ids, m$report_ids)
  expect_identical(env$.batch_needs, m$needs)
  expect_identical(env$.batch_unresolved, m$unresolved)
  expect_identical(env$.batch_sets, m$sets)
  expect_identical(env$.batch_outputs, m$outputs)
  expect_identical(env$.batch_support, m$support)
  expect_identical(env$.batch_data, m$data)
  expect_identical(env$.batch_data_vars, m$data_vars)
  expect_identical(env$.batch_data_dirs, m$data_dirs)
  expect_identical(env$.batch_sources, m$sources)
  expect_identical(env$.batch_check_code, m$check_code)
  expect_identical(env$.batch_kind, "run")
  expect_identical(env$.batch_paths, list())
})

test_that("a data folder variable of \"\" (no variable maps the file) is kept in the head", {
  env <- rcv_head(rcv_manifest())
  expect_identical(unname(env$.batch_data_vars[["data/other/x.csv"]]), "")
})

test_that("a report that needs no ARD of another report has an empty vector written, not NULL", {
  m <- rcv_manifest()
  m$needs <- list(`programs/tfl/t_a.R` = character(), `programs/tfl/t_b.R` = "T-A")
  env <- rcv_head(m)
  expect_identical(env$.batch_needs, m$needs)
  expect_identical(env$.batch_needs[[1L]], character())
})

test_that("backslashes, quotes and line breaks in strings are escaped and read back", {
  m <- rcv_manifest()
  m$root <- "runs\\with \"quote\""
  m$spec <- "spec\\sub"
  m$study <- "S 1\nline two"
  m$check_code <- c("a \\ b.R", "tab\there.R")
  env <- rcv_head(m)
  expect_identical(env$.batch_root, m$root)
  expect_identical(env$.batch_spec, m$spec)
  expect_identical(env$.batch_study, m$study)
  expect_identical(env$.batch_check_code, m$check_code)
})

test_that("a preview with every path written: they all read back", {
  m <- rcv_manifest()
  m$kind <- "preview"
  m$paths <- list(data = c(adsl = "/fictional/adsl.rds", adae = "/fictional/adae.rds"),
                  company_setup = "/fictional/company_setup.R",
                  study_user = "/fictional/study_user.R",
                  output = "/fictional/out")
  env <- rcv_head(m)
  expect_identical(env$.batch_kind, "preview")
  expect_identical(env$.batch_paths, m$paths)
})

test_that("a preview with no paths is allowed and writes list()", {
  m <- rcv_manifest()
  m$kind <- "preview"
  expect_no_error(code <- tfl_runner_code(m))
  expect_true(".batch_paths <- list()" %in% code)
  expect_true(".batch_kind <- \"preview\"" %in% code)
})

test_that("paths: an unnamed list, or two names alike, stops", {
  m <- rcv_manifest()
  m$kind <- "preview"
  m$paths <- list("/fictional/out")
  expect_error(tfl_runner_code(m), "manifest\\$paths` must be a named list")
  m$paths <- list(output = "/fictional/a", output = "/fictional/b")
  expect_error(tfl_runner_code(m), "manifest\\$paths` must be a named list")
})

test_that("paths: an empty string for one of the three strings stops", {
  m <- rcv_manifest()
  m$kind <- "preview"
  for (k in c("company_setup", "study_user", "output")) {
    m$paths <- stats::setNames(list(""), k)
    expect_error(tfl_runner_code(m), paste0("`", k, "` must be one non-empty string"), info = k)
  }
})

test_that("an official run takes no paths, whatever the path is", {
  m <- rcv_manifest()
  m$paths <- list(output = "/fictional/out")
  expect_error(tfl_runner_code(m), "manifest\\$paths` must be list\\(\\) for a kind of \"run\"")
})

test_that("an ARD output is named by the program's file name, not its path", {
  m <- rcv_manifest()
  names(m$ard_outputs)[[1L]] <- "programs/ard/ard_a.R"
  expect_error(tfl_runner_code(m), "manifest\\$ard_outputs` names an ARD program that is not in")
})

test_that("a report program that has no output id or no files stops, naming the program", {
  m <- rcv_manifest()
  m$report_ids <- m$report_ids[2L]
  expect_error(tfl_runner_code(m),
               "manifest\\$report_ids` has no output for the program programs/tfl/t_a\\.R")
  m <- rcv_manifest()
  m$outputs <- m$outputs[1L]
  expect_error(tfl_runner_code(m),
               "manifest\\$outputs` has no files for the program programs/tfl/t_b\\.R")
})

test_that("data, sources and check_code are not matched against the programs", {
  m <- rcv_manifest()
  m$data <- list(`programs/other/unknown.R` = "data/adam/x.csv")
  m$sources <- "input/not-on-disk.csv"
  m$check_code <- "programs/not-on-disk.R"
  expect_no_error(env <- rcv_head(m))
  expect_identical(env$.batch_data, m$data)
})

test_that("an extra element of programs is accepted and left out of the head", {
  m <- rcv_manifest()
  m$programs$misc <- "programs/misc.R"
  env <- rcv_head(m)
  expect_named(env$.batch_programs, c("ard", "tfl"))
})

test_that("an ARD program list with the same file name twice names it in the stop", {
  m <- rcv_manifest()
  m$programs$ard <- c("programs/ard/a/ard_a.R", "programs/ard/b/ard_a.R", "programs/ard/ard_b.R")
  expect_error(tfl_runner_code(m), "manifest\\$programs` `ard` has two programs named ard_a\\.R\\.")
})

test_that("the code is the head followed by whatever runner file tfl_runner_file() gives", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("# fictional runner", "x <- 1"), f)
  local_mocked_bindings(tfl_runner_file = function() f)
  code <- tfl_runner_code(rcv_manifest())
  expect_identical(utils::tail(code, 2L), c("# fictional runner", "x <- 1"))
  expect_identical(code[[which(code == "")[[1L]] + 1L]], "# fictional runner")
  expect_type(code, "character")
  expect_false(anyNA(code))
})

test_that("a runner file that is empty gives the head and a blank line only", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  file.create(f)
  local_mocked_bindings(tfl_runner_file = function() f)
  code <- tfl_runner_code(rcv_manifest())
  expect_identical(code[[length(code)]], "")
  expect_gt(length(code), 10L)
})

test_that("tfl_runner_file() is the batch/runner.R of the installed package", {
  f <- tfl_runner_file()
  expect_match(f, "batch[/\\\\]runner\\.R$")
  expect_identical(basename(dirname(f)), "batch")
})
