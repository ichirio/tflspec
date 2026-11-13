# The study's rounding is one options() call in the study's setup file:
# tfl_study_setup_code() writes it; option tflspec.study_setup leaves it out of
# the ARD and report setups (#782 step 1).

ssc_study <- function(rounding = NULL) {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  if (!is.null(rounding)) {
    it <- tfl_item("plan_digits", list(rounding = rounding))
    it$from <- "defaults"
    st$defaults$content <- c(st$defaults$content, list(it))
  }
  st
}
ssc_ard_spec <- function(rounding = NULL) {
  sp <- exact_spec(list(method = "categorical", population_id = "SAF",
                        by = "TRT01A", variables = "SEX"))
  if (!is.null(rounding)) {
    sp$study[nrow(sp$study) + 1L, "key"] <- "rounding"
    sp$study[nrow(sp$study), "value"] <- rounding
  }
  sp
}
ssc_flag <- function(value) {
  old <- options(tflspec.study_setup = value)
  old
}

test_that("tfl_study_setup_code() gives the rounding as one options() call", {
  sas <- tfl_study_setup_code(ssc_study("sas"))
  expect_length(sas, 2L)
  expect_match(sas[1L], "^# the study's rounding")
  expect_identical(sas[2L], paste0("options(rtfreporter.rounding = \"sas\", ",
                                   "cards.round_type = \"round-half-up\")"))
  r <- tfl_study_setup_code(ssc_study("r"))
  expect_true(grepl("rtfreporter.rounding = \"r\"", r[2L], fixed = TRUE))
  expect_true(grepl("cards.round_type = \"round-to-even\"", r[2L], fixed = TRUE))
  expect_identical(tfl_study_setup_code(ssc_study()), character())
  # the sheets' spec
  wb <- function(v) tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = v, stringsAsFactors = FALSE))
  expect_identical(tfl_study_setup_code(wb("sas")), sas)
  expect_error(tfl_study_setup_code(1), "study spec")
})

test_that("tflspec.study_setup leaves the rounding out of the report setup", {
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  setup <- tfl_report_setup_code(ssc_study("sas"))
  expect_false(any(grepl("rounding|round_type", setup)))
  # the other options stay
  st <- ssc_study("sas")
  st$options <- list(rtfreporter.font = "Arial")
  setup <- tfl_report_setup_code(st)
  expect_true(any(grepl("rtfreporter.font", setup, fixed = TRUE)))
  expect_false(any(grepl("rounding|round_type", setup)))
  # the workbook's route
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = "sas", stringsAsFactors = FALSE))
  expect_false(any(grepl("rounding|round_type", tfl_report_setup_code(sp))))
  # a report's rounding that is the study's stays out of its program
  out <- tfl_output("T-9", "table", population = "SAF", content = list(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_digits", list(AGE = 0))))
  code <- tfl_content_code(tfl_resolve_output(out, ssc_study("sas")))
  expect_false(any(grepl("rounding", code, fixed = TRUE)))
})

test_that("tflspec.study_setup leaves cards' rounding out of the ARD setup", {
  skip_if_not_installed("cards")
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  setup <- tfl_ard_code(ssc_ard_spec("sas"), part = "setup")
  expect_false(any(grepl("round_type", setup, fixed = TRUE)))
  options(tflspec.study_setup = NULL)
  expect_true(any(grepl("round_type", tfl_ard_code(ssc_ard_spec("sas"), part = "setup"),
                        fixed = TRUE)))
})

test_that("without the option, or FALSE, the code is as it was", {
  skip_if_not_installed("cards")
  old <- options(tflspec.study_setup = NULL)
  on.exit(options(old), add = TRUE)
  st <- ssc_study("sas")
  a_rep <- tfl_report_setup_code(st)
  a_ard <- tfl_ard_code(ssc_ard_spec("r"), part = "setup")
  a_ard_all <- tfl_ard_code(ssc_ard_spec("r"))
  options(tflspec.study_setup = FALSE)
  expect_identical(tfl_report_setup_code(st), a_rep)
  expect_identical(tfl_ard_code(ssc_ard_spec("r"), part = "setup"), a_ard)
  expect_identical(tfl_ard_code(ssc_ard_spec("r")), a_ard_all)
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", a_rep, fixed = TRUE)))
  expect_true(any(grepl("round-to-even", a_ard, fixed = TRUE)))
})

# ---- the study's input data (#782 step 2) -----------------------------------

ssc_yaml_study <- function(data_lines) {
  d <- tempfile("ssc")
  dir.create(d)
  file.copy(system.file("extdata", "spec-yaml", package = "tflspec"), d, recursive = TRUE)
  sp <- file.path(d, "spec-yaml")
  writeLines(data_lines, file.path(sp, "data.yml"))
  tfl_read_study_spec(sp)
}
ssc_data_yaml <- c("datasets:",
                   "  - dataset: ADAE", "    path: data/adam/adae.rds",
                   "    derive: AEDUR = AENDY - ASTDY + 1",
                   "  - dataset: ADSL", "    path: data/adam/adsl.xpt")
ssc_sheets <- function(rounding = NULL) {
  tfl_ard_spec(list(
    study = data.frame(key = c("id", if (!is.null(rounding)) "rounding"),
                       value = c("USUBJID", rounding)),
    datasets = data.frame(dataset = c("ADAE", "ADSL"), path = c("adae.rds", "adsl.rds"),
                          derive = c("AEDUR = AENDY - ASTDY + 1", NA)),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = data.frame(output_id = "F-1", data_id = "adae_saf", from = "ADAE",
                               population_id = "SAF"),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")))
}

test_that("tfl_study_setup_code() reads the study's datasets: the YAML study", {
  st <- ssc_yaml_study(ssc_data_yaml)
  code <- tfl_study_setup_code(st)
  expect_identical(code[1L], "# the study's input data (every ARD, listing and figure program uses them)")
  expect_identical(code[-1L], c(
    "adsl <- haven::read_xpt(\"data/adam/adsl.xpt\")",
    "adae <- readRDS(\"data/adam/adae.rds\") |> dplyr::mutate(AEDUR = AENDY - ASTDY + 1)"))
  # the rounding first, as it was
  st$study$settings$rounding <- "sas"
  both <- tfl_study_setup_code(st)
  expect_identical(both[1:2], tfl_study_setup_code(ssc_study("sas"))[1:2])
  expect_identical(both[3L], "")
  expect_identical(both[-(1:3)], code)
  # the folder variables of tflspec.paths
  old <- options(tflspec.paths = c(path_adam = "data/adam"))
  on.exit(options(old), add = TRUE)
  expect_true("adsl <- haven::read_xpt(file.path(path_adam, \"adsl.xpt\"))" %in%
                tfl_study_setup_code(st))
  options(old)
  # no dataset, no rounding: nothing
  expect_identical(tfl_study_setup_code(ssc_study()), character())
})

test_that("tfl_study_setup_code() reads the study's datasets: the sheets", {
  code <- tfl_study_setup_code(ssc_sheets())
  expect_identical(code[-1L], c(
    "adsl <- readRDS(\"adsl.rds\")",
    "adae <- readRDS(\"adae.rds\") |> dplyr::mutate(AEDUR = AENDY - ASTDY + 1)"))
  expect_identical(tfl_study_setup_code(ssc_sheets("r"))[-(1:3)], code)
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "id", value = "USUBJID", stringsAsFactors = FALSE))
  expect_identical(tfl_study_setup_code(sp), character())
})

test_that("tflspec.study_setup: ARD, listing and figure programs start from the objects", {
  x <- ssc_sheets()
  cat <- x$datasets
  sp <- tfl_listing_spec(data.frame(output_id = "L-1", dataset = "ADAE", sort = "USUBJID"),
                         data.frame(output_id = "L-1", vars = "USUBJID"))
  unset <- list(ard = tfl_ard_code(x), data = tfl_ard_code(x, "F-1", part = "data"),
                listing = tfl_listing_code(sp, "L-1", cat))
  expect_true(any(grepl("readRDS(", unset$ard, fixed = TRUE)))
  expect_true(any(grepl("readRDS(", unset$listing, fixed = TRUE)))
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  on <- list(ard = tfl_ard_code(x), data = tfl_ard_code(x, "F-1", part = "data"),
             listing = tfl_listing_code(sp, "L-1", cat))
  for (k in names(on)) {
    expect_false(any(grepl("^[a-z_.]+ <- (readRDS|haven::read_xpt)[(]", on[[k]])), info = k)
  }
  expect_true(any(grepl("^adsl <- order_levels\\(adsl", on$ard)))
  expect_false(any(grepl("^adsl <- adsl$", on$ard)))
  expect_true(any(grepl("^adae <- order_levels\\(adae", on$data)))
  expect_false(any(grepl("mutate(AEDUR", on$ard, fixed = TRUE)))
  expect_true(any(grepl("^adae_saf <- ", on$data)))
  expect_true(any(grepl("^data <- adae", on$listing)))
  expect_false(any(grepl("^adae <- ", on$listing)))
  # off again: as it was
  options(tflspec.study_setup = NULL)
  expect_identical(tfl_ard_code(x), unset$ard)
  expect_identical(tfl_ard_code(x, "F-1", part = "data"), unset$data)
  expect_identical(tfl_listing_code(sp, "L-1", cat), unset$listing)
})

test_that("tflspec's own runs read the files themselves, the option or not", {
  skip_if_not_installed("cards")
  d <- tempfile("ssc")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  adsl <- data.frame(USUBJID = paste0("S", 1:4), SAFFL = "Y", TRT01A = c("A", "A", "B", "B"),
                     SEX = c("F", "M", "F", "M"))
  saveRDS(adsl, file.path(d, "adsl.rds"))
  x <- tfl_ard_spec(list(
    datasets = data.frame(dataset = "ADSL", path = "adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")))
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  ard <- suppressMessages(tfl_build_ard(x, dir = d, save = FALSE))
  expect_true(nrow(ard) > 0L)
  expect_true(isTRUE(getOption("tflspec.study_setup")))
})
