# More coverage of R/run_index.R: the columns, the batch.txt reading, the
# order, and tfl_latest_run()'s choices.  Fictional data only; every study
# is a tempdir() folder that is removed at the end of the test.

ricv_study <- function(study_yml = "study_id: S-1", env = parent.frame()) {
  s <- tempfile("ricv-study")
  dir.create(s)
  do.call(on.exit, list(bquote(unlink(.(s), recursive = TRUE)), add = TRUE), envir = env)
  if (!is.null(study_yml)) writeLines(study_yml, file.path(s, "study.yml"))
  s
}

ricv_rows <- function(part = "tfl", id = "T-1", status = "OK", md5 = "0a1b") {
  data.frame(part = part, program = paste0("programs/", part, "/", id, ".R"),
             output_id = id, status = status, md5 = md5, stringsAsFactors = FALSE)
}

ricv_good <- function(started = "2026-11-20 10:00:00", finished = "2026-11-20 10:05:00") {
  c("Study    : S-1", paste("Started  :", started), paste("Finished :", finished),
    "Kind     : run", "Verified : yes")
}

# a batch folder; `lines` is batch.txt as it is, `rows` the run.csv
ricv_run <- function(study, run, rows = ricv_rows(), lines = ricv_good()) {
  d <- file.path(study, "runs", run)
  dir.create(d, recursive = TRUE)
  utils::write.csv(rows, file.path(d, "run.csv"), row.names = FALSE)
  writeLines(lines, file.path(d, "batch.txt"))
  d
}

test_that("a study folder that does not exist gives the empty index with its columns", {
  idx <- tfl_run_index(file.path(tempdir(), "ricv-no-such-study"))
  expect_identical(nrow(idx), 0L)
  expect_named(idx, c("run", "started", "finished", "part", "program", "output_id",
                      "status", "md5", "path"))
})

test_that("an empty runs/ folder gives the empty index", {
  s <- ricv_study()
  dir.create(file.path(s, "runs"))
  expect_identical(nrow(tfl_run_index(s)), 0L)
})

test_that("a file lying in runs/ is not taken for a batch folder", {
  s <- ricv_study()
  ricv_run(s, "r1")
  writeLines("notes", file.path(s, "runs", "notes.txt"))
  expect_identical(tfl_run_index(s)$run, "r1")
})

test_that("the index has the documented columns only: the other columns of run.csv are left out", {
  s <- ricv_study()
  rows <- ricv_rows()
  rows$errors <- 0L
  rows$warnings <- 2L
  ricv_run(s, "r1", rows = rows)
  idx <- tfl_run_index(s)
  expect_named(idx, c("run", "started", "finished", "part", "program", "output_id",
                      "status", "md5", "path"))
  expect_s3_class(idx$started, "POSIXct")
  expect_s3_class(idx$finished, "POSIXct")
})

test_that("a run that made several programs has one row each, sharing the run's times and folder", {
  s <- ricv_study()
  rows <- rbind(ricv_rows("ard", "T-1"), ricv_rows("ard", "T-2"),
                ricv_rows("tfl", "T-1"), ricv_rows("tfl", "T-2", "ERROR"))
  ricv_run(s, "r1", rows = rows)
  idx <- tfl_run_index(s)
  expect_identical(nrow(idx), 4L)
  expect_identical(unique(idx$run), "r1")
  expect_length(unique(idx$path), 1L)
  expect_length(unique(idx$started), 1L)
  expect_identical(sum(idx$status == "ERROR"), 1L)
  expect_identical(sum(idx$status == "OK"), 3L)
})

test_that("the md5 stays text: leading zeros are not lost", {
  s <- ricv_study()
  ricv_run(s, "r1", rows = ricv_rows(md5 = "0012345"))
  expect_identical(tfl_run_index(s)$md5, "0012345")
})

test_that("the index is ordered by when a run started, not by the folder's name", {
  s <- ricv_study()
  ricv_run(s, "zzz_early", lines = ricv_good(started = "2026-11-18 09:00:00"))
  ricv_run(s, "aaa_late", lines = ricv_good(started = "2026-11-22 09:00:00"))
  ricv_run(s, "mmm_middle", lines = ricv_good(started = "2026-11-20 09:00:00"))
  expect_identical(tfl_run_index(s)$run, c("aaa_late", "mmm_middle", "zzz_early"))
})

test_that("the index's row names are 1, 2, 3 ... after the ordering", {
  s <- ricv_study()
  ricv_run(s, "r1", lines = ricv_good(started = "2026-11-18 09:00:00"))
  ricv_run(s, "r2", lines = ricv_good(started = "2026-11-19 09:00:00"))
  idx <- tfl_run_index(s)
  expect_identical(attr(idx, "row.names"), 1:2)
})

test_that("a batch.txt with Windows line ends is read", {
  s <- ricv_study()
  d <- ricv_run(s, "r1")
  writeBin(charToRaw(paste0(paste(ricv_good(), collapse = "\r\n"), "\r\n")),
           file.path(d, "batch.txt"))
  idx <- tfl_run_index(s)
  expect_identical(idx$run, "r1")
  expect_identical(format(idx$finished), "2026-11-20 10:05:00")
})

test_that("spaces after the study id in batch.txt do not change which study it is", {
  s <- ricv_study()
  ricv_run(s, "r1", lines = c("Study    : S-1    ", ricv_good()[-1L]))
  expect_identical(tfl_run_index(s)$run, "r1")
})

test_that("a batch.txt with no Study line does not count", {
  s <- ricv_study()
  ricv_run(s, "r1", lines = ricv_good()[-1L])
  expect_identical(nrow(tfl_run_index(s)), 0L)
})

test_that("a batch.txt with an empty Verified value or no Verified line does not count", {
  s <- ricv_study()
  ricv_run(s, "empty_verified", lines = c(ricv_good()[1:4], "Verified :"))
  ricv_run(s, "no_verified", lines = ricv_good()[1:4])
  expect_identical(nrow(tfl_run_index(s)), 0L)
})

test_that("a Finished line whose time cannot be read counts, with an NA finish", {
  # NOTE: looks wrong: R/run_index.R only checks that a Finished line is there
  # (not NA), so `Finished : not a time` (or an empty value) counts as finished
  # and the run is listed with finished = NA.
  s <- ricv_study()
  ricv_run(s, "r1", lines = c(ricv_good()[1:2], "Finished : not a time", ricv_good()[4:5]))
  idx <- tfl_run_index(s)
  expect_identical(idx$run, "r1")
  expect_true(is.na(idx$finished))
  expect_false(is.na(idx$started))
})

test_that("a time is read as year-month-day hour:minute:second, other forms are NA", {
  expect_identical(format(.as_time("2026-11-20 10:05:00")), "2026-11-20 10:05:00")
  expect_true(is.na(.as_time("20/11/2026 10:05")))
  expect_true(is.na(.as_time(NA_character_)))
  expect_s3_class(.as_time(character()), "POSIXct")
  expect_length(.as_time(character()), 0L)
})

test_that("the runs of a study are in its runs/ folder", {
  expect_identical(.tfl_runs_dir("/fictional/study"), file.path("/fictional/study", "runs"))
})

test_that("tfl_latest_run(): a status other than exactly OK does not make a report", {
  s <- ricv_study()
  ricv_run(s, "r1", rows = rbind(ricv_rows(id = "T-1", status = "ok"),
                                 ricv_rows(id = "T-2", status = "ERROR"),
                                 ricv_rows(id = "T-3", status = "OK")))
  r <- tfl_latest_run(s, c("T-1", "T-2", "T-3"))
  expect_identical(r$run, c(NA, NA, "r1"))
})

test_that("tfl_latest_run(): the part is chosen by name, and a partial name works", {
  s <- ricv_study()
  ricv_run(s, "r_tfl", rows = ricv_rows("tfl", "T-1"),
           lines = ricv_good(started = "2026-11-20 10:00:00"))
  ricv_run(s, "r_ard", rows = ricv_rows("ard", "T-1"),
           lines = ricv_good(started = "2026-11-21 10:00:00"))
  expect_identical(tfl_latest_run(s, "T-1")$run, "r_tfl")
  expect_identical(tfl_latest_run(s, "T-1", part = "ard")$run, "r_ard")
  expect_identical(tfl_latest_run(s, "T-1", part = "a")$run, "r_ard")
})

test_that("tfl_latest_run(): the answer has the columns, and the run's folder, time and md5", {
  s <- ricv_study()
  ricv_run(s, "r1", rows = ricv_rows(md5 = "ff00"))
  r <- tfl_latest_run(s, "T-1")
  expect_named(r, c("output_id", "run", "path", "finished", "md5"))
  expect_identical(r$md5, "ff00")
  expect_identical(r$path, tfl_run_index(s)$path)
  expect_identical(r$finished, tfl_run_index(s)$finished)
})

test_that("tfl_latest_run(): a report asked for twice is answered twice", {
  s <- ricv_study()
  ricv_run(s, "r1")
  r <- tfl_latest_run(s, c("T-1", "T-1"))
  expect_identical(r$output_id, c("T-1", "T-1"))
  expect_identical(r$run, c("r1", "r1"))
})

test_that("tfl_latest_run(): with no run at all, every report is NA but its id", {
  s <- ricv_study()
  r <- tfl_latest_run(s, c("T-1", "T-2"))
  expect_identical(r$output_id, c("T-1", "T-2"))
  expect_true(all(is.na(r$run)))
  expect_true(all(is.na(r$path)))
  expect_true(all(is.na(r$md5)))
  expect_s3_class(r$finished, "POSIXct")
})

test_that("tfl_latest_run(): a run that failed one report still answers for the others", {
  s <- ricv_study()
  ricv_run(s, "r_old", rows = rbind(ricv_rows(id = "T-1"), ricv_rows(id = "T-2")),
           lines = ricv_good(started = "2026-11-18 10:00:00"))
  ricv_run(s, "r_new",
           rows = rbind(ricv_rows(id = "T-1", status = "ERROR"), ricv_rows(id = "T-2")),
           lines = ricv_good(started = "2026-11-19 10:00:00"))
  r <- tfl_latest_run(s, c("T-1", "T-2"))
  expect_identical(r$run, c("r_old", "r_new"))
})
