# The generated programs' folders and packages (options tflspec.paths and
# tflspec.attached, #178)

test_that("a path through the variable of the folder that holds it", {
  withr::local_options(tflspec.paths = NULL)
  expect_identical(.path_code("data/adam/adsl.rds"), "\"data/adam/adsl.rds\"")
  withr::local_options(tflspec.paths = c(path_data = "data", path_adam = "data/adam/",
                                         path_ard = "output/ard"))
  # the folder that holds it most closely
  expect_identical(.path_code("data/adam/adsl.rds"),
                   "file.path(path_adam, \"adsl.rds\")")
  expect_identical(.path_code("data/sdtm/dm.rds"), "file.path(path_data, \"sdtm/dm.rds\")")
  expect_identical(.path_code("output/ard"), "path_ard")
  # not under any: as it is (a name that only starts the same is not under it)
  expect_identical(.path_code("datasets/x.rds"), "\"datasets/x.rds\"")
  expect_identical(.reader("data/adam/adsl.xpt"),
                   "haven::read_xpt(file.path(path_adam, \"adsl.xpt\"))")
})

test_that("no `pkg::` for an attached package, in the code only", {
  withr::local_options(tflspec.attached = NULL)
  x <- c("ard <- cards::ard_summary(d) # cards::ard_summary, as cards says",
         "y <- \"cards::not_code\"",
         "z <- cardx::ard_stats_t_test(d)\nw <- cards:::internal(1)",
         "")
  expect_identical(.drop_attached_ns(x), x)
  withr::local_options(tflspec.attached = c("cards", "dplyr"))
  expect_identical(.drop_attached_ns(x), c(
    "ard <- ard_summary(d) # cards::ard_summary, as cards says",
    "y <- \"cards::not_code\"",
    "z <- cardx::ard_stats_t_test(d)\nw <- cards:::internal(1)",
    ""))
  # its attributes kept; code that does not parse left as it is
  cl <- structure(c("a <- dplyr::n()", "b <- 1"), class = "tfl_code")
  expect_identical(.drop_attached_ns(cl), structure(c("a <- n()", "b <- 1"),
                                                    class = "tfl_code"))
  expect_identical(.drop_attached_ns("a <- dplyr::n("), "a <- dplyr::n(")
})

opt_spec <- function() {
  tfl_ard_spec(list(
    study = data.frame(key = c("id", "output"), value = c("USUBJID", "output/ard/ard.rds")),
    datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL",
                             where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = "T1", analysis_id = c("AGE", "SEX"),
                          method = c("continuous", "categorical"),
                          population_id = "SAF", by = "TRT01A",
                          variables = c("AGE", "SEX"))))
}

test_that("an ARD program with the setup's folders and packages: the same ARD", {
  skip_if_not_installed("cards")
  sp <- opt_spec()
  plain <- tfl_ard_code(sp, save = FALSE)
  # unset: as before
  withr::with_options(list(tflspec.paths = NULL, tflspec.attached = NULL),
                      expect_identical(tfl_ard_code(sp, save = FALSE), plain))
  withr::local_options(tflspec.paths = c(path_adam = "data/adam", path_ard = "output/ard"),
                       tflspec.attached = "cards")
  code <- tfl_ard_code(sp, save = FALSE)
  expect_true(any(grepl("readRDS(file.path(path_adam, \"adsl.rds\"))", code, fixed = TRUE)))
  expect_false("library(cards)" %in% code)
  lines <- unlist(strsplit(code, "\n", fixed = TRUE))
  calls <- lines[!grepl("^[[:space:]]*#", lines)]
  expect_false(any(grepl("cards::", calls, fixed = TRUE)))
  expect_true(any(grepl("ard_tabulate(", code, fixed = TRUE)))
  # the ARD program attaches the others it calls itself (dplyr, tflspec)
  expect_true(all(c("library(dplyr)", "library(tflspec)") %in% code))
  expect_true(any(grepl("bind_rows(", code, fixed = TRUE)))
  expect_false(any(grepl("dplyr::", calls, fixed = TRUE)))
  # with save: the study ARD through its folder's variable
  full <- tfl_ard_code(sp)
  expect_true(any(full == "saveRDS(ard, file.path(path_ard, \"ard.rds\"))"))
  # it runs where the setup defines the folder and attaches cards: the same
  # ARD as the program written plainly
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "data", "adam"), recursive = TRUE)
  saveRDS(as.data.frame(cards::ADSL), file.path(dir, "data", "adam", "adsl.rds"))
  run <- function(code, env) {
    withr::with_dir(dir, eval(parse(text = code), envir = env))
    env$ard
  }
  e1 <- new.env(parent = asNamespace("cards"))
  e1$path_adam <- "data/adam"
  e2 <- new.env(parent = globalenv())
  expect_identical(as.data.frame(run(code, e1)), as.data.frame(run(plain, e2)))
  # tfl_build_ard() runs the program by itself: neither option applies
  b <- tfl_build_ard(sp, dir = dir, save = FALSE)
  expect_identical(nrow(b), nrow(run(plain, new.env(parent = globalenv()))))
})

test_that("a listing's data is read through the folder's variable too", {
  withr::local_options(tflspec.paths = c(path_adam = "data/adam"),
                       tflspec.attached = "dplyr")
  cat <- data.frame(dataset = "ADAE", path = "data/adam/adae.rds")
  expect_identical(tfl_read_data_code(cat, "ADAE"),
                   "adae <- readRDS(file.path(path_adam, \"adae.rds\"))")
})
