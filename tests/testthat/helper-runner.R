# Helpers for the runner tests (test-runner-run.R, test-runs-run.R): a fake
# study run by real Rscript children.  A helper file, so that every test
# file sees them.

rr_manifest <- function() {
  list(
    root = "runs", ard = "output/ard/ard.rds", study = "S-1", spec = "spec",
    spec_md5 = "x",
    programs = list(ard = "programs/ard/A.R",
                    tfl = c("programs/tfl/T.R", "programs/tfl/U.R")),
    ard_outputs = c(A.R = "T-1"),
    report_ids = c(`programs/tfl/T.R` = "T-1", `programs/tfl/U.R` = "T-2"),
    needs = list(`programs/tfl/T.R` = "T-1"),
    unresolved = character(),
    runs = list(rr_def("Main", c("T-1", "T-2"))),
    outputs = list(`programs/tfl/T.R` = "output/tfl/T.rtf",
                   `programs/tfl/U.R` = "output/tfl/U.rtf"),
    support = list(ard = character(), tfl = character(), all = character()),
    data = list(`programs/tfl/T.R` = "data/adam/x.csv"),
    data_vars = c(`data/adam/x.csv` = "path_adam"),
    data_dirs = c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other"),
    sources = character(), check_code = "programs/company_setup.R",
    kind = "run", paths = list())
}

# a run definition as the manifest holds it
rr_def <- function(name, reports, mode = "all", copy_data = FALSE, problem = "") {
  list(name = name, reports = reports, mode = mode, copy_data = copy_data, problem = problem)
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
  # the stamp of the spec the programs were made from
  m$spec_md5 <- tfl_spec_stamp(s)
  rr_write_batch(s, m)
  s
}

rr_write_batch <- function(s, m) {
  writeLines(tfl_runner_code(m), file.path(s, "programs", "batch.R"))
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

# the study's official run, as tfl_run_command() starts it (the runner called with
# `parts`, then the arguments): the output lines and the exit status
rr_study_run <- function(s, args = character(), env = character(),
                         parts = "c('ard', 'tfl')") {
  old <- setwd(s)
  on.exit(setwd(old))
  expr <- paste0("source('programs/batch.R'); run_batch(", parts, ", commandArgs(TRUE))")
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("-e", shQuote(expr), shQuote(args)),
    stdout = TRUE, stderr = TRUE, env = env))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else st, text = paste(out, collapse = "
"))
}

rr_runs <- function(s) list.dirs(file.path(s, "runs"), recursive = FALSE)

# a folder's files with their md5, to see that nothing changed
rr_tree <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE))
  paste(f, unname(tools::md5sum(file.path(d, f))))
}

# the runner's code in an environment, to call its functions in-process
rr_env <- function(m = rr_manifest()) {
  e <- new.env(parent = globalenv())
  eval(parse(text = tfl_runner_code(m), keep.source = FALSE), e)
  e
}

rr_set <- function(s, program, lines) {
  writeLines(lines, file.path(s, "programs", program))
}

rr_read <- function(...) utils::read.csv(file.path(...), colClasses = "character")

rr_refused <- function(m, pattern, setup = function(s) NULL, args = character()) {
  s <- rr_study(m)
  on.exit(unlink(s, recursive = TRUE))
  setup(s)
  r <- rr_study_run(s, args)
  expect_true(r$status != 0L, info = pattern)
  expect_match(r$text, pattern, info = r$text)
  # nothing was made
  expect_length(rr_runs(s), 0L)
  expect_false(dir.exists(file.path(s, "output")))
}

# the first run of the fake study
rr_first_run <- function(args = character(), env = character(), m = rr_manifest()) {
  s <- rr_study(m)
  r <- rr_study_run(s, args, env)
  list(s = s, r = r, batch = rr_runs(s))
}
