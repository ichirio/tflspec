# The file writer of generated programs (R/program_writer.R, moved from tflplanner)

pw_code <- function(body = "x <- 1") {
  c("# ----", "#  Program    : T-1.R", "#  Generated  : tflspec 0.0.0  2026-11-25", "# ----", body)
}

test_that("a program is written with the checksum of its body under the Generated line", {
  root <- tempfile("pw"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  f <- file.path(root, "programs", "tfl", "T-1.R")
  expect_identical(tflspec:::.put_program(pw_code(), f, root), "written")
  got <- readLines(f)
  at <- grep("^#  Generated  :", got)
  expect_match(got[at + 1L], "^#  Checksum   : [0-9a-f]{32}$")
  expect_identical(sub("^#  Checksum   : ", "", got[at + 1L]), tflspec:::.body_hash(pw_code()))
  # the hash leaves out the Generated and Checksum lines: a new date is the same body
  expect_identical(tflspec:::.body_hash(got), tflspec:::.body_hash(pw_code()))
})

test_that("a program's state: missing, current, generated (the definition moved on), edited", {
  root <- tempfile("pw"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  f <- file.path(root, "programs", "tfl", "T-1.R")
  expect_identical(tflspec:::.ard_program_state(pw_code(), f), "missing")
  tflspec:::.put_program(pw_code(), f, root)
  expect_identical(tflspec:::.ard_program_state(pw_code(), f), "current")
  expect_identical(tflspec:::.ard_program_state(pw_code("x <- 2"), f), "generated")
  writeLines(c(readLines(f), "y <- 3"), f)
  expect_identical(tflspec:::.ard_program_state(pw_code(), f), "edited")
  # an earlier tflplanner's program with no checksum: generated, not edited
  old <- file.path(root, "old.R")
  writeLines(c("#  Generated  : tflplanner 0.1", "x <- 1"), old)
  expect_identical(tflspec:::.ard_program_state(pw_code(), old), "generated")
})

test_that("the same code again is unchanged; new code is written; a hand edit is kept in programs/.edited/", {
  root <- tempfile("pw"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  f <- file.path(root, "programs", "tfl", "T-1.R")
  tflspec:::.put_program(pw_code(), f, root)
  expect_identical(tflspec:::.put_program(pw_code(), f, root), "unchanged")
  expect_identical(tflspec:::.put_program(pw_code("x <- 2"), f, root), "written")
  writeLines(c(readLines(f), "y <- 3"), f)
  expect_identical(tflspec:::.put_program(pw_code("x <- 2"), f, root), "rewritten")
  kept <- list.files(file.path(root, "programs", ".edited"), full.names = TRUE)
  expect_length(kept, 1L)
  expect_match(basename(kept), "^tfl_T-1_[0-9]{8}-[0-9]{6}[.]R$")
  expect_identical(utils::tail(readLines(kept), 1L), "y <- 3")
  expect_false("y <- 3" %in% readLines(f))
})
