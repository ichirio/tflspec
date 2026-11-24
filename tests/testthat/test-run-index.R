# The index of a study's official runs, and each report's latest run (G4,
# slice A'')

# a batch folder as the runner leaves it (only what the index reads)
fake_run <- function(study, run, rows, study_id = "S-1",
                     started = "2026-11-20 10:00:00", finished = TRUE,
                     verified = TRUE, extra = character()) {
  d <- file.path(study, "runs", run)
  dir.create(d, recursive = TRUE)
  utils::write.csv(rows, file.path(d, "run.csv"), row.names = FALSE)
  writeLines(c(paste("Batch    :", run), paste("Study    :", study_id),
               "Spec     : none", paste("Started  :", started),
               if (finished) "Finished : 2026-11-20 10:05:00",
               if (finished && verified) "Verified : yes",
               extra),
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

test_that("tfl_run_index(): only a verified run that was not run again counts", {
  s <- fake_study()
  on.exit(unlink(s, recursive = TRUE))
  fake_run(s, "20261120_100000_all", run_rows("tfl", "T-1"))
  # finished but without the end record
  fake_run(s, "20261121_100000_all", run_rows("tfl", "T-1"),
           started = "2026-11-21 10:00:00", verified = FALSE)
  # run again (a copy put back into runs/)
  fake_run(s, "20261122_100000_all", run_rows("tfl", "T-1"),
           started = "2026-11-22 10:00:00",
           extra = c("Kind     : run", "Re-run   : 2026-11-22 11:00:00"))
  # a preview
  fake_run(s, "20261123_100000_all", run_rows("tfl", "T-1"),
           started = "2026-11-23 10:00:00", extra = "Kind     : preview")
  # an official run
  fake_run(s, "20261124_100000_all", run_rows("tfl", "T-1"),
           started = "2026-11-24 10:00:00", extra = "Kind     : run")
  expect_identical(unique(tfl_run_index(s)$run),
                   c("20261124_100000_all", "20261120_100000_all"))
})
