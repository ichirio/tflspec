# The official run inside its own batch folder (G4, slice A'): real Rscript
# children run a tiny fake study through the runner's two phases

rr_manifest <- function() {
  list(
    root = "runs", ard = "output/ard/ard.rds", study = "S-1", spec = "spec",
    programs = list(ard = "programs/ard/A.R",
                    tfl = c("programs/tfl/T.R", "programs/tfl/U.R")),
    ard_outputs = c(A.R = "T-1"),
    report_ids = c(`programs/tfl/T.R` = "T-1", `programs/tfl/U.R` = "T-2"),
    needs = list(`programs/tfl/T.R` = "T-1"),
    unresolved = character(), sets = list(),
    outputs = list(`programs/tfl/T.R` = "output/tfl/T.rtf",
                   `programs/tfl/U.R` = "output/tfl/U.rtf"),
    support = list(ard = character(), tfl = character(), all = character()),
    data = list(`programs/tfl/T.R` = "data/adam/x.csv"),
    data_vars = c(`data/adam/x.csv` = "path_adam"),
    data_dirs = c("data/adam", "data/sdtm", "data/other"),
    sources = character(), check_code = "programs/company_setup.R",
    kind = "run", paths = list())
}

# a fake study (fictional data): an ARD program, two report programs, one
# data file, and the runner's code as programs/batch.R
rr_study <- function(m = rr_manifest()) {
  s <- normalizePath(tempfile("rrstudy"), winslash = "/", mustWork = FALSE)
  for (d in c("programs/ard", "programs/tfl", "spec", "data/adam")) {
    dir.create(file.path(s, d), recursive = TRUE)
  }
  writeLines("study_id: S-1", file.path(s, "study.yml"))
  writeLines("a: 1", file.path(s, "spec", "s.yml"))
  writeLines(c("a,b", "1,2", "3,4"), file.path(s, "data", "adam", "x.csv"))
  writeLines(c("# nothing here reaches outside the study",
               "x <- paste(\"a\", \"b\", sep = \"/\")", "y <- \"output/y\""),
             file.path(s, "programs", "company_setup.R"))
  writeLines(c("dir.create('output/ard', recursive = TRUE, showWarnings = FALSE)",
               "saveRDS(data.frame(output_id = 'T-1'), 'output/ard/ard.rds')"),
             file.path(s, "programs", "ard", "A.R"))
  writeLines(c("if (file.exists('run_paths.R')) source('run_paths.R')",
               "if (!exists('.tfl_data_root')) .tfl_data_root <- '.'",
               "x <- read.csv(file.path(.tfl_data_root, 'data/adam/x.csv'))",
               "dir.create('output/tfl', recursive = TRUE, showWarnings = FALSE)",
               "writeLines(Sys.getenv('TFL_UPTO'), 'output/tfl/upto.txt')",
               "writeLines(as.character(nrow(x)), 'output/tfl/T.rtf')"),
             file.path(s, "programs", "tfl", "T.R"))
  writeLines("writeLines('u', 'output/tfl/U.rtf')",
             file.path(s, "programs", "tfl", "U.R"))
  rr_write_batch(s, m)
  s
}

rr_write_batch <- function(s, m) {
  writeLines(tfl_runner_code(m), file.path(s, "programs", "batch.R"))
  for (e in c(all = "c(\"ard\", \"tfl\")", ard = "\"ard\"", report = "\"tfl\"")) {
    writeLines(c("source(\"programs/batch.R\")",
                 paste0("run_batch(", e, ", commandArgs(TRUE))")),
               file.path(s, "programs", paste0("autoexec_", names(e), ".R")))
  }
}

# Rscript <script> <args> in `wd`: the output lines and the exit status
rr_rscript <- function(wd, script, args = character(), env = character()) {
  old <- setwd(wd)
  on.exit(setwd(old))
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c(script, shQuote(args)),
    stdout = TRUE, stderr = TRUE, env = env))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else st, text = paste(out, collapse = "\n"))
}

rr_runs <- function(s) list.dirs(file.path(s, "runs"), recursive = FALSE)

rr_refused <- function(m, pattern, setup = function(s) NULL) {
  s <- rr_study(m)
  on.exit(unlink(s, recursive = TRUE))
  setup(s)
  r <- rr_rscript(s, "programs/autoexec_all.R")
  expect_true(r$status != 0L, info = pattern)
  expect_match(r$text, pattern, info = r$text)
  # nothing was made
  expect_length(rr_runs(s), 0L)
  expect_false(dir.exists(file.path(s, "output")))
}

# the first run of the fake study
rr_first_run <- function(args = character(), env = character()) {
  s <- rr_study()
  r <- rr_rscript(s, "programs/autoexec_all.R", args, env)
  list(s = s, r = r, batch = rr_runs(s))
}

test_that("a run keeps everything in its batch folder", {
  skip_on_cran()
  x <- rr_first_run(env = "TFL_UPTO=T-1")
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  expect_length(x$batch, 1L)
  b <- x$batch
  # the study's output/ stays absent; the results are in the folder
  expect_false(dir.exists(file.path(x$s, "output")))
  for (f in c("output/ard/ard.rds", "output/tfl/T.rtf", "output/tfl/U.rtf",
              "snapshot.csv", "run_paths.R", "run_args.txt", "run.csv",
              "issues.csv", "inputs.csv", "data_read.csv",
              "autoexec_all.R", "autoexec_ard.R", "autoexec_report.R",
              "programs/batch.R", "study.yml")) {
    expect_true(file.exists(file.path(b, f)), info = f)
  }
  # the data are read through the root: not copied without --data
  expect_false(dir.exists(file.path(b, "data")))
  expect_identical(readLines(file.path(b, "run_paths.R")),
                   ".tfl_data_root <- \"../..\"")
  expect_identical(readLines(file.path(b, "output", "tfl", "T.rtf")), "2")
  # TFL_UPTO reaches the program children unchanged
  expect_identical(readLines(file.path(b, "output", "tfl", "upto.txt")), "T-1")
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Kind     : run" %in% bt)
  expect_true("Errors   : 0" %in% bt)
  expect_true("Warnings : 0" %in% bt)
  expect_true("Data root : ../.." %in% bt)
  expect_identical(utils::tail(bt, 1L), "Verified : yes")
  r <- utils::read.csv(file.path(b, "run.csv"), colClasses = "character")
  expect_true(all(r$status == "OK"))
  expect_identical(r$empty[r$part == "ard"], "")
  expect_true(all(r$empty[r$part == "tfl"] == "unknown"))
  expect_identical(nrow(utils::read.csv(file.path(b, "issues.csv"))), 0L)
  # the snapshot lists what was copied, with the md5 of the study's file
  sn <- utils::read.csv(file.path(b, "snapshot.csv"), colClasses = "character")
  expect_true("programs/tfl/T.R" %in% sn$path)
  expect_false(any(grepl("^data/", sn$path)))
  expect_identical(sn$md5[sn$path == "study.yml"],
                   unname(tools::md5sum(file.path(x$s, "study.yml"))))
  expect_identical(nrow(tfl_run_index(x$s)), 3L)
})

test_that("a path outside the batch folder is refused before any folder is made", {
  skip_on_cran()
  # an absolute data path
  m <- rr_manifest()
  m$data[["programs/tfl/T.R"]] <- "C:/elsewhere/x.csv"
  rr_refused(m, "C:/elsewhere/x.csv.*move the file under data/ or give it through the data root")
  m$data[["programs/tfl/T.R"]] <- "../x.csv"
  rr_refused(m, "\\.\\./x\\.csv.*move the file under data/")
  # a path no data-folder variable maps
  m <- rr_manifest()
  m$data_vars <- c(`data/adam/x.csv` = "")
  rr_refused(m, "data/adam/x\\.csv.*not mapped.*move the file under data/ or give it through the data root")
  # an ARD file that is not the default
  m <- rr_manifest()
  m$ard <- "output/other.rds"
  rr_refused(m, "output/other\\.rds.*tflspec#914")
  # a literal path into a data folder, in the study's own code
  rr_refused(rr_manifest(), "company_setup\\.R:2",
             setup = function(s) {
               writeLines(c("a <- 1", "x <- \"data/adam/x.csv\""),
                          file.path(s, "programs", "company_setup.R"))
             })
  rr_refused(rr_manifest(), "company_setup\\.R:1",
             setup = function(s) {
               writeLines("x <- \"C:/data/y.csv\"",
                          file.path(s, "programs", "company_setup.R"))
             })
  # a study key `source` outside the copied trees
  m <- rr_manifest()
  m$sources <- "../lib/f.R"
  rr_refused(m, "\\.\\./lib/f\\.R")
  m$sources <- "other/f.R"
  rr_refused(m, "other/f\\.R.*programs/, spec/ or input/")
})

test_that("a declared output that is not made turns the program into an error", {
  skip_on_cran()
  m <- rr_manifest()
  m$outputs[["programs/tfl/U.R"]] <- c("output/tfl/U.rtf", "output/tfl/gone.rtf")
  s <- rr_study(m)
  on.exit(unlink(s, recursive = TRUE))
  r <- rr_rscript(s, "programs/autoexec_all.R")
  expect_true(r$status != 0L)
  b <- rr_runs(s)
  expect_length(b, 1L)
  # (the run ends in error, and still has its end record)
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Errors   : 1" %in% bt)
  expect_identical(utils::tail(bt, 1L), "Verified : yes")
  run <- utils::read.csv(file.path(b, "run.csv"), colClasses = "character")
  expect_identical(run$status[grepl("U[.]R$", run$program)], "ERROR")
  expect_identical(run$errors[grepl("U[.]R$", run$program)], "1")
  iss <- utils::read.csv(file.path(b, "issues.csv"), colClasses = "character")
  expect_identical(nrow(iss), 1L)
  expect_identical(iss$level, "error")
  expect_identical(iss$source, "output")
  expect_match(iss$text, "gone.rtf")
})

test_that("the entry file of a batch folder refuses to run in place", {
  skip_on_cran()
  x <- rr_first_run()
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  before <- readLines(file.path(x$batch, "batch.txt"))
  r <- rr_rscript(x$batch, "autoexec_all.R")
  expect_true(r$status != 0L)
  expect_match(r$text, "Official results cannot be overwritten")
  expect_identical(readLines(file.path(x$batch, "batch.txt")), before)
})

test_that("a batch folder runs again elsewhere, with its selection, and is never official", {
  skip_on_cran()
  # the first run selects T-1 only (not the report T-2)
  x <- rr_first_run("T-1")
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  expect_false(file.exists(file.path(x$batch, "output", "tfl", "U.rtf")))
  expect_identical(readLines(file.path(x$batch, "run_args.txt")), "T-1")
  el <- tempfile("else")
  dir.create(el)
  on.exit(unlink(el, recursive = TRUE), add = TRUE)
  file.copy(x$batch, el, recursive = TRUE)
  b2 <- file.path(el, basename(x$batch))
  # no data in the copy and none given: stops
  r <- rr_rscript(b2, "autoexec_all.R")
  expect_true(r$status != 0L)
  expect_match(r$text, "This batch folder has no data: run it with --data-root=")
  # with the data root: it runs, keeps the first run.csv, and notes it
  writeLines("stale", file.path(b2, "output", "tfl", "stale.txt"))
  r <- rr_rscript(b2, "autoexec_all.R", paste0("--data-root=", x$s))
  expect_identical(r$status, 0L, info = r$text)
  expect_true(file.exists(file.path(b2, "run_1.csv")))
  expect_false(file.exists(file.path(b2, "output", "tfl", "stale.txt")))
  # the selection of the first run, with no arguments
  expect_false(file.exists(file.path(b2, "output", "tfl", "U.rtf")))
  expect_true(file.exists(file.path(b2, "output", "tfl", "T.rtf")))
  bt <- readLines(file.path(b2, "batch.txt"))
  expect_true(any(grepl("^Re-run   : ", bt)))
  expect_false("Verified : yes" %in% bt)
  expect_true(paste("Data root :", x$s) %in% bt)
  expect_match(readLines(file.path(b2, "run_paths.R")), x$s, fixed = TRUE)
  # a re-run copied back into runs/ is not counted
  back <- file.path(x$s, "runs", "20261201_000000_copy")
  dir.create(back)
  file.copy(list.files(b2, full.names = TRUE, all.files = TRUE, no.. = TRUE),
            back, recursive = TRUE)
  expect_true(file.exists(file.path(back, "batch.txt")))
  idx <- tfl_run_index(x$s)
  expect_identical(unique(idx$run), basename(x$batch))
})

test_that("autoexec_report.R of a batch folder clears only output/tfl/", {
  skip_on_cran()
  x <- rr_first_run()
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  el <- tempfile("else")
  dir.create(el)
  on.exit(unlink(el, recursive = TRUE), add = TRUE)
  file.copy(x$batch, el, recursive = TRUE)
  b2 <- file.path(el, basename(x$batch))
  writeLines("keep", file.path(b2, "output", "ard", "marker.txt"))
  writeLines("stale", file.path(b2, "output", "tfl", "stale.txt"))
  # (its ARD is in the folder: the reports' needs are met by it)
  r <- rr_rscript(b2, "autoexec_report.R", paste0("--data-root=", x$s))
  expect_identical(r$status, 0L, info = r$text)
  expect_true(file.exists(file.path(b2, "output", "ard", "marker.txt")))
  expect_true(file.exists(file.path(b2, "output", "ard", "ard.rds")))
  expect_false(file.exists(file.path(b2, "output", "tfl", "stale.txt")))
  expect_true(file.exists(file.path(b2, "output", "tfl", "T.rtf")))
})

test_that("issues.csv folds in the conditions a program wrote", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  writeLines(c("dir.create('logs/conditions', recursive = TRUE, showWarnings = FALSE)",
               "write.csv(data.frame(output_id = 'T-2',",
               "  class = c('simpleWarning', 'simpleError', 'message'),",
               "  message = c('w', 'e', 'm')), 'logs/conditions/T-2.csv', row.names = FALSE)",
               "writeLines('u', 'output/tfl/U.rtf')"),
             file.path(s, "programs", "tfl", "U.R"))
  r <- rr_rscript(s, "programs/autoexec_all.R")
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  iss <- utils::read.csv(file.path(b, "issues.csv"), colClasses = "character")
  expect_identical(iss$source, rep("condition", 3L))
  expect_identical(iss$level, c("warning", "error", "note"))
  expect_identical(iss$output_id, rep("T-2", 3L))
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Errors   : 1" %in% bt)
  expect_true("Warnings : 1" %in% bt)
})
