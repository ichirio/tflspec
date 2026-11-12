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
