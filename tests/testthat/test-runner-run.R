# The official run inside its own batch folder (G4, slice A'): real Rscript
# children run a tiny fake study through the runner's two phases

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
                   c(".tfl_data_root <- \"../..\"", ".tfl_study_root <- \"../..\""))
  expect_identical(readLines(file.path(b, "output", "tfl", "T.rtf")), "2")
  # TFL_UPTO reaches the program children unchanged
  expect_identical(readLines(file.path(b, "output", "tfl", "upto.txt")), "T-1")
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Kind     : run" %in% bt)
  expect_true("Errors   : 0" %in% bt)
  expect_true("Warnings : 0" %in% bt)
  expect_true("Data root : ../.." %in% bt)
  expect_identical(utils::tail(bt, 1L), "Verified : yes")
  # the start record's lines come first: the definition, the mode, the kind,
  # the start, the log engine (R CMD BATCH by default, said)
  expect_identical(bt[1:3], c("Run      : Main", "Mode     : all", "Kind     : run"))
  expect_match(bt[[4L]], "^Started  : [0-9]{4}-[0-9]{2}-[0-9]{2} ")
  expect_identical(bt[[5L]], "Logs     : R CMD BATCH")
  expect_false(any(grepl("^Batch set", bt)))
  # the folder is batch_<date>-<time>_<definition>
  expect_match(basename(b), "^batch_[0-9]{8}-[0-9]{6}_Main$")
  expect_identical(readLines(file.path(b, "run_args.txt")), "--run=Main")
  expect_true(file.exists(file.path(b, "run_definition.yml")))
  # the stamp is the one tflspec computes, from the same function
  expect_true(paste("Spec     :", tfl_spec_stamp(x$s)) %in% bt)
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
  rr_refused(m, paste0("data/adam/x\\.csv.*not mapped.*add its folder as a data key of the layout ",
                       "\\(adam, sdtm or other in study\\.yml\\).*move the file under one of: ",
                       "data/adam, data/sdtm, data/other"))
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
  r <- rr_study_run(s)
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
  tree <- rr_tree(file.path(x$batch, "output"))
  r <- rr_rscript(x$batch, "autoexec_all.R")
  expect_true(r$status != 0L)
  expect_match(r$text, "Official results cannot be overwritten")
  expect_identical(readLines(file.path(x$batch, "batch.txt")), before)
  expect_identical(rr_tree(file.path(x$batch, "output")), tree)
})

test_that("a batch folder runs again elsewhere, with its selection, and is never official", {
  skip_on_cran()
  # the first run is the definition One: T-1 only (not the report T-2)
  m <- rr_manifest()
  m$runs <- list(rr_def("Main", c("T-1", "T-2")), rr_def("One", "T-1"))
  x <- rr_first_run("--run=One", m = m)
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  expect_false(file.exists(file.path(x$batch, "output", "tfl", "U.rtf")))
  expect_identical(readLines(file.path(x$batch, "run_args.txt")), "--run=One")
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
  expect_match(readLines(file.path(b2, "run_paths.R"))[1L], x$s, fixed = TRUE)
  # (--data-root changes only the data root)
  expect_identical(readLines(file.path(b2, "run_paths.R"))[2L], ".tfl_study_root <- \"../..\"")
  expect_true(file.exists(file.path(b2, "issues_1.csv")))
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
  r <- rr_study_run(s)
  # (an error-level condition makes the program ERROR: the run ends with status 1)
  expect_identical(r$status, 1L, info = r$text)
  b <- rr_runs(s)
  iss <- utils::read.csv(file.path(b, "issues.csv"), colClasses = "character")
  expect_identical(iss$source, rep("condition", 3L))
  expect_identical(iss$level, c("warning", "error", "note"))
  expect_identical(iss$output_id, rep("T-2", 3L))
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Errors   : 1" %in% bt)
  expect_true("Warnings : 1" %in% bt)
  expect_true("Result   : 2 of 3 program(s) without error" %in% bt)
  run <- utils::read.csv(file.path(b, "run.csv"), colClasses = "character")
  expect_identical(run$status[grepl("U[.]R$", run$program)], "ERROR")
  expect_identical(run$errors[grepl("U[.]R$", run$program)], "1")
})

# ---- review fixes (Fable, S1) -----------------------------------------------

test_that("the literal check: what is a path and what is not", {
  e <- rr_env()
  bad <- e$.batch_bad_literal
  # strings that only look like paths pass
  for (s in c("/$", "/N", "/100 PY", "N:", "\\\\", "..", "..ard_x..", "a/b", "% (n)")) {
    expect_identical(bad(s), "", info = s)
  }
  # absolute paths, either slash
  for (s in c("/usr/lib", "//srv/share/x", "C:/data/y.csv", "C:\\data\\y.csv",
              "\\\\srv\\share\\x", "~/x")) {
    expect_identical(bad(s), "an absolute path", info = s)
  }
  # a .. segment in a string with a separator, either slash
  for (s in c("../x", "x/../y", "..\\data\\x.csv", "x\\..\\y")) {
    expect_identical(bad(s), "a path with a .. segment", info = s)
  }
  # a data folder: lower case, repeated slashes, ./ and backslashes
  for (s in c("data/adam/x.csv", "Data/ADaM/x.csv", "data//adam/x.csv",
              "./data/adam", "data\\adam\\x.csv")) {
    expect_match(bad(s), "data folder", info = s)
  }
})

test_that("the shipped study_helpers.R gives no finding from the literal check", {
  f <- system.file("helpers", "study_helpers.R", package = "tflspec")
  skip_if(!nzchar(f))
  m <- rr_manifest()
  m$check_code <- f
  e <- rr_env(m)
  expect_identical(e$.batch_check_paths(list(tfl = character())), character())
})

test_that("a line ending in '# tfl: not a path' is not scanned; the refusal names it", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f))
  writeLines(c("x <- \"C:/data/y.csv\" # tfl: not a path",
               "y <- \"C:/data/z.csv\"",
               "z <- \"/dir/\"  #tfl:not a path"), f)
  m <- rr_manifest()
  m$check_code <- f
  e <- rr_env(m)
  p <- e$.batch_check_paths(list(tfl = character()))
  expect_length(p, 1L)
  expect_match(p, ":2: the string")
  expect_match(p, "# tfl: not a path", fixed = TRUE)
})

test_that("backslash and ./ data paths pass the stage check", {
  m <- rr_manifest()
  m$data[["programs/tfl/T.R"]] <- "data\\adam\\x.csv"
  m$data_vars <- c(`data/adam/x.csv` = "path_adam")
  expect_identical(rr_env(m)$.batch_check_paths(list(tfl = "programs/tfl/T.R")), character())
  m$data[["programs/tfl/T.R"]] <- "./data/adam/x.csv"
  expect_identical(rr_env(m)$.batch_check_paths(list(tfl = "programs/tfl/T.R")), character())
  # keyed as written
  m$data_vars <- c(`./data/adam/x.csv` = "path_adam")
  expect_identical(rr_env(m)$.batch_check_paths(list(tfl = "programs/tfl/T.R")), character())
})

test_that("the copy walk leaves out dot names, editor files and Excel lock files", {
  d <- tempfile("walk")
  dir.create(d)
  old <- setwd(d)
  on.exit({
    setwd(old)
    unlink(d, recursive = TRUE)
  })
  dir.create("spec/.backup", recursive = TRUE)
  for (f in c("spec/a.yml", "spec/~$book.xlsx", "spec/Thumbs.db", "spec/desktop.ini",
              "spec/b.yml~", "spec/c.tmp", "spec/.hidden", "spec/.backup/old.yml",
              "spec/book.xlsx")) {
    writeLines("x", f)
  }
  e <- rr_env()
  expect_identical(sort(e$.batch_walk("spec")), c("spec/a.yml", "spec/book.xlsx"))
  expect_false(identical(e$.batch_spec_md5(), "none"))
})

test_that("a failing report is an ERROR with an issue; the run ends with status 1", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "tfl/U.R", "stop('x')")
  r <- rr_study_run(s)
  expect_identical(r$status, 1L, info = r$text)
  b <- rr_runs(s)
  run <- rr_read(b, "run.csv")
  expect_identical(run$status[grepl("U[.]R$", run$program)], "ERROR")
  expect_identical(run$status[grepl("T[.]R$", run$program)], "OK")
  iss <- rr_read(b, "issues.csv")
  u <- iss[grepl("U[.]R$", iss$program), ]
  expect_gte(nrow(u), 1L)
  expect_true(all(u$level == "error"))
  bt <- readLines(file.path(b, "batch.txt"))
  n_err <- sum(iss$level == "error")
  expect_true(paste("Errors   :", n_err) %in% bt)
  expect_true("Result   : 2 of 3 program(s) without error" %in% bt)
  expect_identical(utils::tail(bt, 1L), "Verified : yes")
})

test_that("a failed ARD program: its reports are 'not run'", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "ard/A.R", "stop('x')")
  r <- rr_study_run(s)
  expect_identical(r$status, 1L, info = r$text)
  b <- rr_runs(s)
  run <- rr_read(b, "run.csv")
  expect_identical(run$status[grepl("A[.]R$", run$program)], "ERROR")
  t <- run[grepl("T[.]R$", run$program), ]
  expect_identical(t$status, "ERROR")
  expect_match(t$note, "not run")
  expect_false(file.exists(file.path(b, "output", "tfl", "T.rtf")))
  expect_identical(run$status[grepl("U[.]R$", run$program)], "OK")
  iss <- rr_read(b, "issues.csv")
  expect_true(any(grepl("T[.]R$", iss$program) & iss$level == "error"))
})

test_that("an exit with a status and no Error line is an ERROR with the fallback issue", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "tfl/U.R", "quit(save = 'no', status = 2)")
  r <- rr_study_run(s)
  expect_identical(r$status, 1L, info = r$text)
  b <- rr_runs(s)
  run <- rr_read(b, "run.csv")
  expect_identical(run$status[grepl("U[.]R$", run$program)], "ERROR")
  expect_identical(run$errors[grepl("U[.]R$", run$program)], "1")
  iss <- rr_read(b, "issues.csv")
  u <- iss[grepl("U[.]R$", iss$program), ]
  expect_identical(nrow(u), 1L)
  expect_identical(u$source, "log")
  expect_identical(u$level, "error")
  expect_match(u$text, "the program ended with status [1-9]; see ")
  expect_true("Errors   : 1" %in% readLines(file.path(b, "batch.txt")))
})

test_that("--data copies the data under their own paths; the folder runs again anywhere", {
  skip_on_cran()
  x <- rr_first_run("--data")
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  b <- x$batch
  expect_true(file.exists(file.path(b, "data", "adam", "x.csv")))
  expect_true(file.exists(file.path(b, "data_manifest.csv")))
  expect_identical(readLines(file.path(b, "run_paths.R"))[1L], ".tfl_data_root <- \".\"")
  sn <- rr_read(b, "snapshot.csv")
  expect_true("data/adam/x.csv" %in% sn$path)
  expect_true("Data root : ." %in% readLines(file.path(b, "batch.txt")))
  el <- tempfile("else")
  dir.create(el)
  on.exit(unlink(el, recursive = TRUE), add = TRUE)
  file.copy(b, el, recursive = TRUE)
  b2 <- file.path(el, basename(b))
  r <- rr_rscript(b2, "autoexec_all.R")
  expect_identical(r$status, 0L, info = r$text)
  expect_identical(readLines(file.path(b2, "output", "tfl", "T.rtf")), "2")
})

test_that("two stages in one second: the second folder gets _2", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  t0 <- Sys.time()
  # the folder names of the next seconds are taken
  for (i in -2:60) {
    dir.create(file.path(s, "runs", paste0("batch_", format(t0 + i, "%Y%m%d-%H%M%S"), "_Main")),
               recursive = TRUE)
  }
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  expect_match(r$text, "Batch folder: .*_Main_2")
  d <- rr_runs(s)
  made <- d[file.exists(file.path(d, "batch.txt"))]
  expect_length(made, 1L)
  expect_match(basename(made), "_Main_2$")
  # the list tells the definition from the name: Main, not Main_2
  l <- tfl_list_runs(s)
  expect_identical(l$definition[l$run == basename(made)], "Main")
})

test_that("an md5 mismatch after the copy removes the batch folder", {
  skip_on_cran()
  s <- rr_study()
  m <- rr_manifest()
  m$spec_md5 <- tfl_spec_stamp(s)
  e <- rr_env(m)
  e$.batch_quit <- function(status) stop("quit ", status, call. = FALSE)
  # something is saved while the copy is made
  e$file.copy <- function(from, to, ...) {
    ok <- base::file.copy(from, to, ...)
    if (basename(to) == "study.yml") writeLines("changed", to)
    ok
  }
  old <- setwd(s)
  on.exit({
    setwd(old)
    unlink(s, recursive = TRUE)
  })
  msgs <- character()
  err <- tryCatch(
    withCallingHandlers(
      utils::capture.output(e$.batch_stage(c("ard", "tfl"))),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }),
    error = function(err) conditionMessage(err))
  expect_match(err, "quit 1")
  expect_match(paste(msgs, collapse = "\n"), "copy does not match the study")
  expect_length(list.dirs(file.path(s, "runs"), recursive = FALSE), 0L)
  expect_false(dir.exists(file.path(s, "output")))
})

test_that("a preview: no Verified, its own data files, not in the run index", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  out <- tempfile("outside")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  d <- normalizePath(file.path(out, "d.csv"), winslash = "/", mustWork = FALSE)
  writeLines(c("a,b", "1,2", "3,4", "5,6"), d)
  m <- rr_manifest()
  m$kind <- "preview"
  m$root <- file.path(s, "prev")
  m$data[["programs/tfl/T.R"]] <- d
  m$data_vars <- character()
  m$paths <- list(data = c(ds = d))
  rr_write_batch(s, m)
  rr_set(s, "tfl/T.R", c(sprintf("x <- read.csv('%s')", d),
                         "dir.create('output/tfl', recursive = TRUE, showWarnings = FALSE)",
                         "writeLines(as.character(nrow(x)), 'output/tfl/T.rtf')"))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- list.dirs(file.path(s, "prev"), recursive = FALSE)
  expect_length(b, 1L)
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Kind     : preview" %in% bt)
  expect_false("Verified : yes" %in% bt)
  expect_identical(readLines(file.path(b, "output", "tfl", "T.rtf")), "3")
  # the data file is read where it is: size and md5 are there
  dr <- rr_read(b, "data_read.csv")
  expect_false(is.na(dr$md5[[1L]]))
  expect_false(dir.exists(file.path(s, "output")))
  # an absolute runs root: the study's own path as the study root
  sr <- sub("^[.]tfl_study_root <- \"(.*)\"$", "\\1", readLines(file.path(b, "run_paths.R"))[2L])
  expect_identical(normalizePath(sr, winslash = "/"), normalizePath(s, winslash = "/"))
  # copied into runs/, it is still not counted
  dir.create(file.path(s, "runs"))
  file.copy(b, file.path(s, "runs"), recursive = TRUE)
  expect_identical(nrow(tfl_run_index(s)), 0L)
})

test_that("an empty report: run.csv says TRUE and issues.csv notes 'no rows'", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "tfl/U.R", c(
    "writeLines('u', 'output/tfl/U.rtf')",
    "write.csv(data.frame(output_id = c('T-1', 'T-2'), empty = c('FALSE', 'TRUE')),",
    "          'output/tfl/report_status.csv', row.names = FALSE)"))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  run <- rr_read(b, "run.csv")
  expect_identical(run$empty[grepl("U[.]R$", run$program)], "TRUE")
  expect_identical(run$empty[grepl("T[.]R$", run$program)], "FALSE")
  iss <- rr_read(b, "issues.csv")
  expect_identical(nrow(iss), 1L)
  expect_identical(iss$level, "note")
  expect_identical(iss$text, "no rows")
  expect_true("Errors   : 0" %in% readLines(file.path(b, "batch.txt")))
})

test_that("a missing or malformed conditions file is skipped", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "tfl/U.R", c(
    "dir.create('logs/conditions', recursive = TRUE, showWarnings = FALSE)",
    "writeLines('not,a \"csv', 'logs/conditions/T-2.csv')",
    "writeLines(c('a,b', '1,2'), 'logs/conditions/T-3.csv')",
    "writeLines('u', 'output/tfl/U.rtf')"))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  expect_identical(nrow(rr_read(b, "issues.csv")), 0L)
})

test_that("a runs root two levels deep: the data are found and running in place is refused", {
  skip_on_cran()
  m <- rr_manifest()
  m$root <- "output/runs"
  s <- rr_study(m)
  on.exit(unlink(s, recursive = TRUE))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- list.dirs(file.path(s, "output", "runs"), recursive = FALSE)
  expect_length(b, 1L)
  expect_identical(readLines(file.path(b, "run_paths.R")),
                   c(".tfl_data_root <- \"../../..\"", ".tfl_study_root <- \"../../..\""))
  expect_identical(readLines(file.path(b, "output", "tfl", "T.rtf")), "2")
  tree <- rr_tree(file.path(b, "output"))
  r <- rr_rscript(b, "autoexec_all.R")
  expect_true(r$status != 0L)
  expect_match(r$text, "Official results cannot be overwritten")
  expect_identical(rr_tree(file.path(b, "output")), tree)
})

test_that("a reports-only run again needs the folder's ARD and drops stale conditions", {
  skip_on_cran()
  x <- rr_first_run()
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  el <- tempfile("else")
  dir.create(el)
  on.exit(unlink(el, recursive = TRUE), add = TRUE)
  file.copy(x$batch, el, recursive = TRUE)
  b2 <- file.path(el, basename(x$batch))
  dir.create(file.path(b2, "logs", "conditions"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(output_id = "T-2", class = "simpleError", message = "stale"),
            file.path(b2, "logs", "conditions", "T-2.csv"), row.names = FALSE)
  r <- rr_rscript(b2, "autoexec_report.R", paste0("--data-root=", x$s))
  expect_identical(r$status, 0L, info = r$text)
  expect_false(file.exists(file.path(b2, "logs", "conditions", "T-2.csv")))
  expect_false("condition" %in% rr_read(b2, "issues.csv")$source)
  # without its ARD it stops, before clearing anything
  unlink(file.path(b2, "output", "ard", "ard.rds"))
  tree <- rr_tree(file.path(b2, "output"))
  r <- rr_rscript(b2, "autoexec_report.R", paste0("--data-root=", x$s))
  expect_true(r$status != 0L)
  expect_match(r$text, "No ARD in this batch folder: run autoexec_ard.R or autoexec_all.R first.",
               fixed = TRUE)
  expect_identical(rr_tree(file.path(b2, "output")), tree)
})

# ---- run definitions (G4 slice B1): --run, the folder, the start record, the stamp ----

test_that("two definitions and no --run: the run stops, listing the names", {
  skip_on_cran()
  m <- rr_manifest()
  m$runs <- list(rr_def("Main", c("T-1", "T-2")), rr_def("One", "T-1"))
  rr_refused(m, "several run definitions; say which with --run=<name>:\n  Main\n  One")
})

test_that("no definition and no --run: the message says how to make one", {
  skip_on_cran()
  m <- rr_manifest()
  m$runs <- list()
  rr_refused(m, "the study has no run definition.*spec/runs.yml")
})

test_that("--run=<name> and --run <name> start the definition; the name is exact", {
  skip_on_cran()
  m <- rr_manifest()
  m$runs <- list(rr_def("Main", c("T-1", "T-2")), rr_def("One", "T-1"))
  x <- rr_first_run(c("--run", "One"), m = m)
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  expect_match(basename(x$batch), "_One$")
  expect_true("Run      : One" %in% readLines(file.path(x$batch, "batch.txt")))
  # the other spelling
  x2 <- rr_first_run("--run=Main", m = m)
  on.exit(unlink(x2$s, recursive = TRUE), add = TRUE)
  expect_identical(x2$r$status, 0L, info = x2$r$text)
  expect_match(basename(x2$batch), "_Main$")
  # a name that differs in case is not the definition: the message names the near one
  rr_refused(m, "no run definition named 'main'.*\n  Names are exact: did you mean 'Main'", args = "--run=main")
  rr_refused(m, "no run definition named 'Nine'", args = "--run=Nine")
  rr_refused(m, "--run needs the definition's name", args = "--run")
  rr_refused(m, "--run is given twice", args = c("--run=One", "--run=Main"))
})

test_that("--batch is refused, with no alias", {
  skip_on_cran()
  rr_refused(rr_manifest(), "--batch is gone: use --run=<name>", args = "--batch=Main")
  rr_refused(rr_manifest(), "--batch is gone: use --run=<name>", args = c("--batch", "Main"))
})

test_that("an official run takes no names and no --exclude", {
  skip_on_cran()
  rr_refused(rr_manifest(), "takes no names and no --exclude", args = "T-1")
  rr_refused(rr_manifest(), "takes no names and no --exclude", args = "--exclude=T-2")
  rr_refused(rr_manifest(), "takes no names and no --exclude", args = c("--run=Main", "T-1"))
})

test_that("a definition with a problem is refused, with its text", {
  skip_on_cran()
  m <- rr_manifest()
  m$runs <- list(rr_def("Main", c("T-1", "T-2"), problem = "T-2 reads the ARD of T-9, which the definition does not select"))
  rr_refused(m, "the run definition 'Main' has a problem[.]\n  T-2 reads the ARD of T-9")
  # an option the runner does not take
  m$runs <- list(rr_def("Main", c("T-1", "T-2"), mode = "weird"))
  rr_refused(m, "mode 'weird' is not one of: all, ard")
})

test_that("a name that breaks the rule is refused before any folder is made", {
  skip_on_cran()
  for (nm in c("a b", "../x", "Final.", "1st", strrep("a", 41L), "")) {
    m <- rr_manifest()
    m$runs <- list(rr_def(nm, c("T-1", "T-2")))
    rr_refused(m, "breaks the rule: ASCII letters, digits, _ and -")
  }
})

test_that("a listed id with no program is refused, not 'the rest runs'", {
  skip_on_cran()
  m <- rr_manifest()
  m$runs <- list(rr_def("Main", c("T-1", "T-77")))
  rr_refused(m, "no program for T-77")
})

test_that("mode ard runs the ARD part only; copy_data keeps the data", {
  skip_on_cran()
  m <- rr_manifest()
  m$runs <- list(rr_def("Ard", c("T-1", "T-2"), mode = "ard"))
  x <- rr_first_run(m = m)
  on.exit(unlink(x$s, recursive = TRUE))
  expect_identical(x$r$status, 0L, info = x$r$text)
  bt <- readLines(file.path(x$batch, "batch.txt"))
  expect_true("Mode     : ard" %in% bt)
  expect_identical(unique(rr_read(x$batch, "run.csv")$part), "ard")
  expect_false(file.exists(file.path(x$batch, "output", "tfl", "T.rtf")))
  m$runs <- list(rr_def("Keep", c("T-1", "T-2"), copy_data = TRUE))
  y <- rr_first_run(m = m)
  on.exit(unlink(y$s, recursive = TRUE), add = TRUE)
  expect_identical(y$r$status, 0L, info = y$r$text)
  expect_true(file.exists(file.path(y$batch, "data", "adam", "x.csv")))
  expect_identical(readLines(file.path(y$batch, "run_paths.R"))[1L], ".tfl_data_root <- \".\"")
})

test_that("the stamp: a spec that changed since the programs were made is refused", {
  skip_on_cran()
  pat <- "programs/batch.R is older than the spec: save the study \\(tflplanner\\) or regenerate the programs"
  # a spec file edited, a file added by hand (runs.yml), study.yml edited
  rr_refused(rr_manifest(), pat, setup = function(s) writeLines("a: 2", file.path(s, "spec", "s.yml")))
  rr_refused(rr_manifest(), pat, setup = function(s) writeLines("runs: []", file.path(s, "spec", "runs.yml")))
  rr_refused(rr_manifest(), pat, setup = function(s) {
    writeLines(c("study_id: S-1", "tokens: {A: b}"), file.path(s, "study.yml"))
  })
  # what a save leaves out does not count: the programs, dot names, editor files
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  u <- file.path(s, "programs", "tfl", "U.R")
  writeLines(c(readLines(u), "# edited by hand"), u)
  writeLines("x", file.path(s, "spec", "s.yml~"))
  dir.create(file.path(s, "spec", ".backup"))
  writeLines("x", file.path(s, "spec", ".backup", "old.yml"))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
})

test_that("the stamp is the same in a copy of the study, and hashes bytes", {
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  a <- tfl_spec_stamp(s)
  expect_match(a, "^[0-9a-f]{32}$")
  s2 <- tempfile("copy")
  dir.create(s2)
  on.exit(unlink(s2, recursive = TRUE), add = TRUE)
  file.copy(file.path(s, c("study.yml", "spec")), s2, recursive = TRUE)
  expect_identical(tfl_spec_stamp(s2), a)
  # bytes, not lines: the same text with LF and with CRLF is two stamps
  writeBin(c(charToRaw("a: 1"), as.raw(10L)), file.path(s2, "spec", "s.yml"))
  lf <- tfl_spec_stamp(s2)
  writeBin(c(charToRaw("a: 1"), as.raw(c(13L, 10L))), file.path(s2, "spec", "s.yml"))
  expect_false(identical(tfl_spec_stamp(s2), lf))
  # nothing to hash
  e <- tempfile("none")
  dir.create(e)
  on.exit(unlink(e, recursive = TRUE), add = TRUE)
  expect_identical(tfl_spec_stamp(e), "none")
})

test_that("the start record is there before any program runs", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "ard/A.R", c(
    "dir.create('output/ard', recursive = TRUE, showWarnings = FALSE)",
    "saveRDS(data.frame(output_id = 'T-1'), 'output/ard/ard.rds')",
    paste("writeLines(c(readLines('batch.txt')[1:3], file.exists('run_definition.yml'),",
          "readLines('run_args.txt')), 'output/ard/seen.txt')")))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  expect_identical(readLines(file.path(b, "output", "ard", "seen.txt")),
                   c("Run      : Main", "Mode     : all", "Kind     : run", "TRUE", "--run=Main"))
  # run_definition.yml: the resolved definition and the stamp
  d <- readLines(file.path(b, "run_definition.yml"))
  expect_true("name: \"Main\"" %in% d)
  expect_true("  - \"T-1\"" %in% d)
  expect_true(paste0("stamp: \"", tfl_spec_stamp(s), "\"") %in% d)
})

test_that("a run definition written beside a folder reads back", {
  e <- rr_env()
  d <- rr_def("One", c("T-1", "a\"b"), mode = "ard", copy_data = TRUE)
  f <- tempfile()
  writeLines(e$.batch_definition_lines(d), f)
  r <- e$.batch_read_definition(f)
  expect_identical(r$name, "One")
  expect_identical(r$reports, c("T-1", "a\"b"))
  expect_identical(r$mode, "ard")
  expect_true(r$copy_data)
  expect_identical(r$stamp, "x")
  expect_null(e$.batch_read_definition(tempfile()))
})

test_that("a preview takes no --run; its folder is preview_<date>-<time>", {
  skip_on_cran()
  m <- rr_manifest()
  m$kind <- "preview"
  m$runs <- list()
  s <- rr_study(m)
  on.exit(unlink(s, recursive = TRUE))
  r <- rr_study_run(s, "--run=Main")
  expect_true(r$status != 0L)
  expect_match(r$text, "A preview takes no --run.", fixed = TRUE)
  expect_length(rr_runs(s), 0L)
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  expect_match(basename(b), "^preview_[0-9]{8}-[0-9]{6}$")
  bt <- readLines(file.path(b, "batch.txt"))
  expect_identical(bt[1:3], c("Run      : none", "Mode     : all", "Kind     : preview"))
  # a preview is not tied to the saved spec
  writeLines("a: 2", file.path(s, "spec", "s.yml"))
  expect_identical(rr_study_run(s)$status, 0L)
})

test_that("a run in the study is always a definition's run: the parts alone are refused", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  msg <- "a run in the study starts from a run definition"
  r <- rr_study_run(s, parts = "'ard'")
  expect_true(r$status != 0L)
  expect_match(r$text, msg, fixed = TRUE)
  r <- rr_study_run(s, "--run=Main", parts = "'tfl'")
  expect_true(r$status != 0L)
  expect_match(r$text, msg, fixed = TRUE)
  expect_length(rr_runs(s), 0L)
  expect_false(dir.exists(file.path(s, "output")))
})

test_that("logrx is used only when asked for", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  if (requireNamespace("logrx", quietly = TRUE)) {
    # (installed: the default run still says R CMD BATCH)
    r <- rr_study_run(s)
    expect_identical(r$status, 0L, info = r$text)
    expect_true("Logs     : R CMD BATCH" %in% readLines(file.path(rr_runs(s), "batch.txt")))
  } else {
    r <- rr_study_run(s, "--logrx")
    expect_true(r$status != 0L)
    expect_match(r$text, "--logrx needs the logrx package", fixed = TRUE)
    expect_length(rr_runs(s), 0L)
  }
})

test_that("tfl_run_study(): the same refusal text as a hand Rscript, and the same run", {
  skip_on_cran()
  # a definition with a problem
  m <- rr_manifest()
  m$runs <- list(rr_def("Main", c("T-1", "T-2"), problem = "the study has no report T-9"))
  s <- rr_study(m)
  on.exit(unlink(s, recursive = TRUE))
  hand <- rr_study_run(s, "--run=Main")
  err <- tryCatch(tfl_run_study(s, "Main"), error = function(e) conditionMessage(e))
  expect_true(hand$status != 0L)
  expect_match(err, "has a problem", fixed = TRUE)
  expect_true(grepl(hand$text, err, fixed = TRUE), info = paste(hand$text, "|", err))
  expect_length(rr_runs(s), 0L)
  # stale programs
  s2 <- rr_study()
  on.exit(unlink(s2, recursive = TRUE), add = TRUE)
  writeLines("a: 2", file.path(s2, "spec", "s.yml"))
  hand <- rr_study_run(s2, "--run=Main")
  err <- tryCatch(tfl_run_study(s2, "Main"), error = function(e) conditionMessage(e))
  expect_match(err, "older than the spec", fixed = TRUE)
  expect_true(grepl(hand$text, err, fixed = TRUE), info = paste(hand$text, "|", err))
  # the run itself: the folder it returns, the same run.csv as by hand
  s3 <- rr_study()
  on.exit(unlink(s3, recursive = TRUE), add = TRUE)
  hand <- rr_study_run(s3, "--run=Main")
  expect_identical(hand$status, 0L, info = hand$text)
  b <- suppressMessages(utils::capture.output(res <- tfl_run_study(s3, "Main")))
  expect_true(dir.exists(res))
  expect_match(basename(res), "^batch_[0-9]{8}-[0-9]{6}_Main(_2)?$")
  runs <- rr_runs(s3)
  expect_length(runs, 2L)
  keep <- c("part", "program", "output_id", "status")
  a <- rr_read(runs[basename(runs) != basename(res)][[1L]], "run.csv")[keep]
  c2 <- rr_read(res, "run.csv")[keep]
  expect_identical(a, c2)
  # each is in the list, under its definition
  l <- tfl_list_runs(s3)
  expect_identical(l$definition, c("Main", "Main"))
  expect_true(all(l$verified))
})

test_that("the stamp leaves out study.yml's updated: line, and nothing else of it", {
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  y <- file.path(s, "study.yml")
  base <- readLines(y)
  writeLines(c(base, "updated: 2026-01-01 10:00:00"), y)
  a <- tfl_spec_stamp(s)
  # a save at another time: the same stamp
  writeLines(c(base, "updated: 2026-11-24 18:30:00"), y)
  expect_identical(tfl_spec_stamp(s), a)
  # a key that only starts like it is another line
  writeLines(c(base, "updated: 2026-11-24 18:30:00", "updated_by: x"), y)
  expect_false(identical(tfl_spec_stamp(s), a))
  # another change: another stamp
  writeLines(c(base, "updated: 2026-11-24 18:30:00", "tokens: {A: b}"), y)
  expect_false(identical(tfl_spec_stamp(s), a))
})

test_that("a study without renv.lock: the run writes one, the packages the programs name and what they need", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  # a report program that names a package (yaml), and a comment and a
  # string that name others: only the call counts
  rr_set(s, "tfl/U.R", c("# ggplot2::ggplot() in a comment is not a use",
                         "x <- yaml::as.yaml(list(a = 1))",
                         "writeLines('u', 'output/tfl/U.rtf')"))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  expect_false(file.exists(file.path(s, "renv.lock")))
  lock <- jsonlite::fromJSON(file.path(b, "renv.lock"), simplifyVector = FALSE)
  expect_identical(lock$R$Version, paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(lock$R$Repositories[[1L]]$Name, "CRAN")
  y <- lock$Packages$yaml
  expect_identical(y$Package, "yaml")
  expect_identical(y$Version, as.character(utils::packageVersion("yaml")))
  expect_true(y$Source %in% c("Repository", "GitHub", "unknown"))
  # nothing the programs do not name; no base package; not the runner's own
  # mentions (tflspec, logrx)
  expect_false(any(c("ggplot2", "tflspec", "logrx", "utils", "tools") %in% names(lock$Packages)))
  bt <- readLines(file.path(b, "batch.txt"))
  expect_true("Session  : session_info.txt and renv.lock" %in% bt)
  # a re-run of the folder elsewhere keeps the lockfile as it is
  before <- unname(tools::md5sum(file.path(b, "renv.lock")))
  b2 <- file.path(tempfile("handed"), basename(b))
  dir.create(dirname(b2))
  on.exit(unlink(dirname(b2), recursive = TRUE), add = TRUE)
  file.copy(b, dirname(b2), recursive = TRUE)
  rr_set(b2, "tfl/U.R", c("x <- jsonlite::toJSON(1)", "writeLines('u', 'output/tfl/U.rtf')"))
  r2 <- rr_rscript(b2, "autoexec_all.R", paste0("--data-root=", s))
  expect_identical(r2$status, 0L, info = r2$text)
  expect_identical(unname(tools::md5sum(file.path(b2, "renv.lock"))), before)
})

test_that("a study's own renv.lock is copied as it is; a package it lacks is a warning, not a stop", {
  skip_on_cran()
  s <- rr_study()
  on.exit(unlink(s, recursive = TRUE))
  rr_set(s, "tfl/U.R", c("x <- yaml::as.yaml(list(a = 1))",
                         "writeLines('u', 'output/tfl/U.rtf')"))
  own <- c("{", "  \"R\": {\"Version\": \"4.6.1\", \"Repositories\": []},",
           "  \"Packages\": {",
           "    \"cards\": {\"Package\": \"cards\", \"Version\": \"0.9.0\", \"Source\": \"Repository\"}",
           "  }", "}")
  writeLines(own, file.path(s, "renv.lock"))
  r <- rr_study_run(s)
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  expect_identical(readLines(file.path(b, "renv.lock")), own)
  iss <- rr_read(b, "issues.csv")
  w <- iss[iss$source == "renv", , drop = FALSE]
  expect_identical(nrow(w), 1L)
  expect_identical(w$level, "warning")
  expect_identical(w$program, "renv.lock")
  expect_match(w$text, "renv.lock (the study's) does not list yaml", fixed = TRUE)
  expect_true("Warnings : 1" %in% readLines(file.path(b, "batch.txt")))
  # the study's file is not touched
  expect_identical(readLines(file.path(s, "renv.lock")), own)
})

test_that("a lockfile entry: GitHub keeps its Remote* fields (RemoteSha), a repository its name, a local install is unknown", {
  e <- rr_env()
  entry <- function(d) {
    txt <- c("{", "\"x\": {", e$.batch_lock_entry(d), "}", "}")
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)$x
  }
  gh <- entry(c(Package = "tflspec", Version = "0.0.25.9000", Imports = "yaml (>= 2.3.5),\n    utils",
                RemoteType = "github", RemoteHost = "api.github.com", RemoteUsername = "ichirio",
                RemoteRepo = "tflspec", RemoteRef = "next", RemoteSha = "abc123"))
  expect_identical(gh$Source, "GitHub")
  expect_identical(gh$RemoteSha, "abc123")
  expect_identical(gh$RemoteRepo, "tflspec")
  # (not utils: base packages come with R)
  expect_identical(unlist(gh$Requirements), "yaml")
  cr <- entry(c(Package = "yaml", Version = "2.3.10", Repository = "CRAN", Depends = "R (>= 3.6)"))
  expect_identical(cr$Source, "Repository")
  expect_identical(cr$Repository, "CRAN")
  expect_length(cr$Requirements, 0L)
  lo <- entry(c(Package = "mine", Version = "1.0"))
  expect_identical(lo$Source, "unknown")
  # JSON strings: quotes, backslashes, new lines
  s <- paste0("a \"q\" b", "\\", "c\nd")
  expect_identical(jsonlite::fromJSON(e$.batch_json(s)), s)
})

test_that("a program's R reads no .Rprofile, no .Renviron (B2); the run records its libraries", {
  skip_on_cran()
  s <- rr_study()
  prof <- tempfile("rprofile", fileext = ".R")
  on.exit(unlink(c(s, prof), recursive = TRUE))
  # a user's profile that would change what a program gives
  writeLines("options(tfl.seen = 'profile')", prof)
  rr_set(s, "tfl/U.R", c("dir.create('output/tfl', recursive = TRUE, showWarnings = FALSE)",
                         "writeLines(getOption('tfl.seen', 'none'), 'output/tfl/U.rtf')"))
  r <- rr_rscript(s, "programs/autoexec_all.R", env = paste0("R_PROFILE_USER=", prof))
  expect_identical(r$status, 0L, info = r$text)
  b <- rr_runs(s)
  expect_identical(readLines(file.path(b, "output", "tfl", "U.rtf")), "none")
  si <- readLines(file.path(b, "session_info.txt"))
  expect_true(any(startsWith(si, "The library paths the programs' R used")))
  # both ways of running a program take the flags (logrx's Rscript, R CMD BATCH)
  code <- tfl_runner_code(rr_manifest())
  flags <- '.batch_r_flags <- c("--no-init-file", "--no-environ")'
  expect_true(any(grepl(flags, code, fixed = TRUE)))
  expect_identical(sum(grepl(".batch_r_flags,", code, fixed = TRUE)), 2L)
})
