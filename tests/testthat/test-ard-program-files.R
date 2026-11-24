# The ARD programs' writer (R/ard_program_files.R, moved from tflplanner)
# (the fixture programs-family/out/*/ard is not compared: its arguments --
# standards, attached, date -- are not reproduced here)

apf_spec <- function(ids = c("T1", "T2")) {
  tfl_ard_spec(list(
    datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL",
                             where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = ids, analysis_id = paste0("AGE", seq_along(ids)),
                          method = "continuous", population_id = "SAF",
                          by = "TRT01A", variables = "AGE")))
}
apf_date <- as.Date("2026-11-25")
apf_gen <- "tflspec test"
apf_save <- function(spec, root) {
  tflspec:::.save_ard_programs(spec, root, date = apf_date, generator = apf_gen)
}
apf_dir <- function(root) file.path(root, "programs", "ard")

test_that("a save writes the setup and one program per output, with tfl_ard_program()'s text", {
  root <- tempfile("apf"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  sp <- apf_spec()
  got <- apf_save(sp, root)
  expect_identical(basename(got$file), c("ard_setup.R", "T1.R", "T2.R"))
  expect_identical(got$status, rep("written", 3L))
  expect_setequal(list.files(apf_dir(root)), c("ard_setup.R", "T1.R", "T2.R"))
  # the checksum line is the writer's: the rest is the generator's text
  same <- function(f, code) {
    expect_identical(tflspec:::.body_hash(readLines(f)), tflspec:::.body_hash(code))
  }
  same(file.path(apf_dir(root), "ard_setup.R"),
       tfl_ard_program(sp, NULL, date = apf_date, generator = apf_gen))
  for (id in c("T1", "T2")) {
    same(file.path(apf_dir(root), paste0(id, ".R")),
         tfl_ard_program(sp, id, dir = root, date = apf_date, generator = apf_gen))
  }
})

test_that("a second save with the same inputs writes nothing new", {
  root <- tempfile("apf"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  sp <- apf_spec()
  apf_save(sp, root)
  again <- apf_save(sp, root)
  expect_identical(again$status, rep("unchanged", 3L))
})

test_that("a program of an output no longer defined is removed; an edited one is copied first; one's own is left", {
  root <- tempfile("apf"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  apf_save(apf_spec(), root)
  own <- file.path(apf_dir(root), "mine.R")
  writeLines("x <- 1", own)
  # T2 edited by hand, then dropped from the definition
  t2 <- file.path(apf_dir(root), "T2.R")
  writeLines(c(readLines(t2), "y <- 3"), t2)
  got <- apf_save(apf_spec("T1"), root)
  expect_identical(got$status[basename(got$file) == "T2.R"], "removed")
  expect_false(file.exists(t2))
  kept <- list.files(file.path(root, "programs", ".edited"))
  expect_length(kept, 1L)
  expect_match(kept, "^ard_T2_[0-9]{8}-[0-9]{6}[.]R$")
  expect_true(file.exists(own))
  expect_false("mine.R" %in% basename(got$file))
  expect_true(file.exists(file.path(apf_dir(root), "T1.R")))
})

test_that("an old programs/make_ard.R is removed", {
  root <- tempfile("apf"); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "programs"))
  old <- file.path(root, "programs", "make_ard.R")
  writeLines("x <- 1", old)
  got <- apf_save(apf_spec(), root)
  expect_false(file.exists(old))
  expect_identical(got$status[got$file == old], "removed")
})
