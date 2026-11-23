# The index of a study's official runs, and each report's latest run (G4,
# slice A'')

# a batch folder as the runner leaves it (only what the index reads)
fake_run <- function(study, run, rows, study_id = "S-1",
                     started = "2026-11-20 10:00:00", finished = TRUE) {
  d <- file.path(study, "runs", run)
  dir.create(d, recursive = TRUE)
  utils::write.csv(rows, file.path(d, "run.csv"), row.names = FALSE)
  writeLines(c(paste("Batch    :", run), paste("Study    :", study_id),
               "Spec     : none", paste("Started  :", started),
               if (finished) "Finished : 2026-11-20 10:05:00"),
             file.path(d, "batch.txt"))
  d
}
run_rows <- function(part, id, status = "OK") {
  data.frame(part = part, program = paste0("programs/", part, "/", id, ".R"),
             output_id = id, status = status, errors = 0L, warnings = 0L,
             md5 = "abc", stringsAsFactors = FALSE)
}
fake_study <- function() {
  s <- tempfile("study")
  dir.create(s)
  writeLines("study_id: S-1", file.path(s, "study.yml"))
  s
}

test_that("tfl_latest_run(): per report, the newest run that made it", {
  s <- fake_study()
  on.exit(unlink(s, recursive = TRUE))
  fake_run(s, "20261120_100000_all",
           rbind(run_rows("ard", "T-1"), run_rows("ard", "T-2"),
                 run_rows("tfl", "T-1"), run_rows("tfl", "T-2")))
  # a later run where T-2 failed: T-1 is its, T-2 stays the earlier run's
  fake_run(s, "20261121_100000_all",
           rbind(run_rows("ard", "T-1"), run_rows("tfl", "T-1"),
                 run_rows("tfl", "T-2", "ERROR")),
           started = "2026-11-21 10:00:00")
  # an ARD-only run answers for the ARD only
  fake_run(s, "20261122_100000_ard", run_rows("ard", "T-2"),
           started = "2026-11-22 10:00:00")
  r <- tfl_latest_run(s, c("T-1", "T-2", "T-9"))
  expect_identical(r$output_id, c("T-1", "T-2", "T-9"))
  expect_identical(r$run, c("20261121_100000_all", "20261120_100000_all", NA))
  expect_true(is.na(r$path[[3L]]))
  expect_s3_class(r$finished, "POSIXct")
  a <- tfl_latest_run(s, c("T-1", "T-2"), part = "ard")
  expect_identical(a$run, c("20261121_100000_all", "20261122_100000_ard"))
  idx <- tfl_run_index(s)
  expect_identical(unique(idx$run), c("20261122_100000_ard",
                                      "20261121_100000_all",
                                      "20261120_100000_all"))
})

test_that("tfl_run_index(): a run of another study, a run going on and an older run do not count", {
  s <- fake_study()
  on.exit(unlink(s, recursive = TRUE))
  fake_run(s, "20261120_100000_all", run_rows("tfl", "T-1"))
  # a batch folder of another study put under runs/
  fake_run(s, "20261123_100000_all", run_rows("tfl", "T-1"), study_id = "S-2",
           started = "2026-11-23 10:00:00")
  # one still going (no Finished line)
  fake_run(s, "20261124_100000_all", run_rows("tfl", "T-1"),
           started = "2026-11-24 10:00:00", finished = FALSE)
  # one of the runner before output_id
  old <- run_rows("tfl", "T-1")
  old$output_id <- NULL
  fake_run(s, "20261125_100000_all", old, started = "2026-11-25 10:00:00")
  expect_identical(unique(tfl_run_index(s)$run), "20261120_100000_all")
  expect_identical(tfl_latest_run(s, "T-1")$run, "20261120_100000_all")
})

test_that("tfl_run_index(): no runs, no study.yml: an empty index", {
  s <- fake_study()
  on.exit(unlink(s, recursive = TRUE))
  expect_identical(nrow(tfl_run_index(s)), 0L)
  r <- tfl_latest_run(s, "T-1")
  expect_identical(nrow(r), 1L)
  expect_true(is.na(r$run))
  unlink(file.path(s, "study.yml"))
  fake_run(s, "20261120_100000_all", run_rows("tfl", "T-1"))
  expect_identical(nrow(tfl_run_index(s)), 0L)
})

# a real run of the runner: run.csv's output_id, data_read.csv, batch.txt's
# Study and Spec lines; the index reads it
test_that("the runner records what the index reads", {
  skip_on_cran()
  s <- fake_study()
  on.exit(unlink(s, recursive = TRUE))
  for (d in c("programs/ard", "programs/tfl", "spec/.backup", "data"))
    dir.create(file.path(s, d), recursive = TRUE)
  utils::write.csv(data.frame(x = 1:3), file.path(s, "data", "adsl.csv"),
                   row.names = FALSE)
  outside <- tempfile(fileext = ".csv")
  on.exit(unlink(outside), add = TRUE)
  writeLines("y\n1", outside)
  writeLines("a: 1", file.path(s, "spec", "outputs.yml"))
  # names whose order differs between locales (case, _ and .)
  for (f in c("Zeta.yml", "alpha.yml", "B_c.yml", "b.c.yml")) {
    writeLines(f, file.path(s, "spec", f))
  }
  writeLines(c("dir.create('output/ard', recursive = TRUE, showWarnings = FALSE)",
               "saveRDS(data.frame(id = 'T-1'), 'output/ard/ard.rds')"),
             file.path(s, "programs", "ard", "ard_t1.R"))
  writeLines(c("dir.create('output/tfl', recursive = TRUE, showWarnings = FALSE)",
               "writeLines('{\\\\rtf1}', 'output/tfl/t1.rtf')"),
             file.path(s, "programs", "tfl", "t1.R"))
  m <- list(
    root = "runs", ard = "output/ard/ard.rds", study = "S-1", spec = "spec",
    programs = list(ard = "programs/ard/ard_t1.R", tfl = "programs/tfl/t1.R"),
    ard_outputs = c(ard_t1.R = "T-1"),
    report_ids = c(`programs/tfl/t1.R` = "T-1"),
    needs = list(), unresolved = character(), sets = list(),
    outputs = list(`programs/tfl/t1.R` = "output/tfl/t1.rtf"),
    support = list(ard = character(), tfl = character(), all = character()),
    data = list(`programs/ard/ard_t1.R` = c("data/adsl.csv", outside)))
  writeLines(c(tfl_runner_code(m), 'run_batch(c("ard", "tfl"))'),
             file.path(s, "batch.R"))
  old <- setwd(s)
  out <- system2(file.path(R.home("bin"), "Rscript"), "batch.R",
                 stdout = TRUE, stderr = TRUE)
  setwd(old)
  run <- list.dirs(file.path(s, "runs"), recursive = FALSE)
  expect_length(run, 1L)
  r <- utils::read.csv(file.path(run, "run.csv"), colClasses = "character")
  expect_identical(names(r)[1:3], c("part", "program", "output_id"))
  expect_identical(r$output_id, c("T-1", "T-1"))
  bt <- readLines(file.path(run, "batch.txt"))
  expect_true("Study    : S-1" %in% bt)
  spec1 <- grep("^Spec     : ", bt, value = TRUE)
  expect_match(spec1, "^Spec     : [0-9a-f]{32}$")
  # the md5 of "path md5" lines in byte (radix) order, whatever the locale
  sf <- c("B_c.yml", "Zeta.yml", "alpha.yml", "b.c.yml", "outputs.yml")
  expect_identical(sf, sort(sf, method = "radix"))
  tmp <- tempfile()
  writeLines(paste(sf, tools::md5sum(file.path(s, "spec", sf))), tmp,
             useBytes = TRUE)
  expect_identical(spec1, paste("Spec     :", unname(tools::md5sum(tmp))))
  unlink(tmp)
  dr <- utils::read.csv(file.path(run, "data_read.csv"),
                        colClasses = "character")
  expect_identical(dr$file, c("data/adsl.csv",
                              file.path("<outside>", basename(outside))))
  expect_identical(dr$md5[[1L]],
                   unname(tools::md5sum(file.path(s, "data", "adsl.csv"))))
  # no absolute path in the batch folder's records
  for (f in c("run.csv", "batch.txt", "data_read.csv")) {
    txt <- readLines(file.path(run, f))
    for (p in c(s, normalizePath(s, "/"), dirname(outside),
                normalizePath(dirname(outside), "/"))) {
      expect_false(any(grepl(p, txt, fixed = TRUE)), info = f)
    }
  }
  expect_identical(tfl_latest_run(s, "T-1")$run, basename(run))
  expect_identical(tfl_latest_run(s, "T-1", part = "ard")$run, basename(run))
  # the spec md5 leaves out spec/.backup/ and editor temporary files
  writeLines("b: 2", file.path(s, "spec", ".backup", "outputs.yml"))
  writeLines("x", file.path(s, "spec", "outputs.yml~"))
  writeLines("x", file.path(s, "spec", "outputs.yml.tmp"))
  Sys.sleep(1.1)
  old <- setwd(s)
  system2(file.path(R.home("bin"), "Rscript"), "batch.R", stdout = TRUE,
          stderr = TRUE)
  setwd(old)
  run2 <- setdiff(list.dirs(file.path(s, "runs"), recursive = FALSE), run)
  expect_length(run2, 1L)
  expect_identical(grep("^Spec     : ", readLines(file.path(run2, "batch.txt")),
                        value = TRUE), spec1)
  expect_identical(tfl_latest_run(s, "T-1")$run, basename(run2))
})
