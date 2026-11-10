# The study's rounding is written once, in the setup every report sources
# (options(rtfreporter.rounding = )); a report's program writes its own
# plan_digits(rounding = ) only when it differs from the study's.

sr_study <- function(rounding = "sas") {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  it <- tfl_item("plan_digits", list(rounding = rounding))
  it$from <- "defaults"
  st$defaults$content <- c(st$defaults$content, list(it))
  st
}
sr_output <- function(content = list()) {
  tfl_output("T-9", "table", population = "SAF", content = c(list(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_digits", list(AGE = 0))), content))
}

test_that("the study's rounding is the setup's option, not the report's code", {
  st <- sr_study("sas")
  setup <- tfl_report_setup_code(st)
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", setup, fixed = TRUE)))
  code <- tfl_content_code(tfl_resolve_output(sr_output(), st))
  expect_false(any(grepl("rounding", code, fixed = TRUE)))
  expect_true(any(grepl("plan_digits(AGE = 0)", code, fixed = TRUE)))
  # the run in hand keeps it: the same numbers with no option set
  ro <- tfl_resolve_output(sr_output(), st)
  r <- unlist(lapply(Filter(function(it) it$fn == "plan_digits", ro$content), function(it) it$args$rounding))
  expect_identical(r, "sas")
})

test_that("changing the study's rounding changes the setup only; a report's own rounding stays", {
  a <- sr_study("sas")
  b <- sr_study("r")
  expect_false(identical(tfl_report_setup_code(a), tfl_report_setup_code(b)))
  expect_identical(tfl_content_code(tfl_resolve_output(sr_output(), a)),
                   tfl_content_code(tfl_resolve_output(sr_output(), b)))
  # a report that differs from the study says so in its own code
  own <- sr_output(list(tfl_item("plan_digits", list(rounding = "r"))))
  code <- tfl_content_code(tfl_resolve_output(own, a))
  expect_true(any(grepl("rounding = \"r\"", code, fixed = TRUE)))
  # a study without one: no option
  st0 <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  expect_false(any(grepl("rtfreporter.rounding", tfl_report_setup_code(st0), fixed = TRUE)))
})

test_that("the workbook's study rounding: in the setup, not in each table's code", {
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = "sas", stringsAsFactors = FALSE))
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", tfl_report_setup_code(sp), fixed = TRUE)))
  expect_false(any(grepl("rounding", tfl_table_code(sp, "T1", normalize = FALSE), fixed = TRUE)))
})

# ---- cards formats the ARD's numbers: the study's rounding reaches it ----

sr_ard_spec <- function(rounding = NULL) {
  sp <- exact_spec(list(method = "categorical", population_id = "SAF",
                        by = "TRT01A", variables = "SEX"))
  if (!is.null(rounding)) {
    sp$study[nrow(sp$study) + 1L, "key"] <- "rounding"
    sp$study[nrow(sp$study), "value"] <- rounding
  }
  sp
}
# the numbers cards formats under the options a setup's lines set
sr_cards_under <- function(setup, x, digits = 1) {
  # (a setup line may set rtfreporter's option too: both put back)
  old <- options(cards.round_type = NULL, rtfreporter.rounding = NULL)
  on.exit(options(old), add = TRUE)
  for (l in grep("cards.round_type", setup, value = TRUE, fixed = TRUE)) eval(str2lang(l))
  cards::label_round(digits)(x)
}

test_that("the setup a program writes sets cards' rounding with the study's", {
  st_sas <- tfl_report_setup_code(sr_study("sas"))
  st_r <- tfl_report_setup_code(sr_study("r"))
  expect_true(any(grepl("cards.round_type = \"round-half-up\"", st_sas, fixed = TRUE)))
  expect_true(any(grepl("cards.round_type = \"round-to-even\"", st_r, fixed = TRUE)))
  expect_false(any(grepl("cards.round_type", tfl_report_setup_code(
    tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))), fixed = TRUE)))
  # the workbook's study rounding
  wb <- function(v) tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = v, stringsAsFactors = FALSE))
  expect_true(any(grepl("round-to-even", tfl_report_setup_code(wb("r")), fixed = TRUE)))
  expect_true(any(grepl("round-half-up", tfl_report_setup_code(wb("sas")), fixed = TRUE)))
  # and the numbers that setup gives: a tie at 1 decimal, and at none
  expect_identical(sr_cards_under(st_r, c(0.25, 2.5), 0), c("0", "2"))
  expect_identical(sr_cards_under(st_r, 0.25), "0.2")
  expect_identical(sr_cards_under(st_sas, c(0.25, 2.45)), c("0.3", "2.5"))
  expect_identical(sr_cards_under(st_sas, 2.5, 0), "3")
})

test_that("the ARD program's setup sets cards' rounding with the study's", {
  none <- tfl_ard_code(sr_ard_spec(), part = "setup")
  expect_false(any(grepl("cards.round_type", none, fixed = TRUE)))
  r <- tfl_ard_code(sr_ard_spec("r"), part = "setup")
  s <- tfl_ard_code(sr_ard_spec("sas"), part = "setup")
  expect_true(any(grepl("cards.round_type = \"round-to-even\"", r, fixed = TRUE)))
  expect_true(any(grepl("cards.round_type = \"round-half-up\"", s, fixed = TRUE)))
  # (the whole program carries it too)
  expect_true(any(grepl("round-to-even", tfl_ard_code(sr_ard_spec("r"), save = FALSE), fixed = TRUE)))
  expect_identical(sr_cards_under(r, c(0.25, 2.5), 0), c("0", "2"))
  expect_identical(sr_cards_under(s, c(0.25, 2.5), 0), c("0", "3"))
  expect_identical(sr_cards_under(r, 0.25), "0.2")
  expect_identical(sr_cards_under(s, 0.25), "0.3")
})

test_that("a YAML study's ARD setup carries its rounding", {
  skip_on_cran()
  for (v in c("sas", "r")) {
    dir <- withr::local_tempdir()
    dir.create(file.path(dir, "spec", "outputs"), recursive = TRUE)
    writeLines(c("study_id: SR-1", "keys:", "  id: USUBJID", "settings:",
                 paste("  rounding:", v)), file.path(dir, "study.yml"))
    writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: data/adam/adsl.rds",
                 "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
               file.path(dir, "spec", "data.yml"))
    writeLines(c("outputs:", "  - id: T-1", "    kind: table"), file.path(dir, "spec", "toc.yml"))
    writeLines(c("id: T-1", "kind: table", "extends: none", "ard:", "  analyses:",
                 "    - id: AGE", "      method: continuous", "      population: SAF",
                 "      by: [ARM]", "      variables: [AGE]", "data:", "  source: ard",
                 "  object: ard", "content:", "  - table_plan: {cols: ARM}"),
               file.path(dir, "spec", "outputs", "T-1.yml"))
    st <- tfl_read_study_spec(dir)
    expect_identical(tflspec:::.study_rounding(st), v)
    setup <- tfl_ard_code(st, part = "setup")
    expect_true(any(grepl(if (v == "r") "cards.round_type = \"round-to-even\"" else
                            "cards.round_type = \"round-half-up\"", setup, fixed = TRUE)))
  }
})

test_that("an in-process run sets cards' rounding and puts the options back", {
  old <- options(rtfreporter.rounding = NULL, cards.round_type = NULL)
  on.exit(options(old), add = TRUE)
  op <- tflspec:::.study_rounding_option(sr_study("r"))
  expect_identical(getOption("rtfreporter.rounding"), "r")
  expect_identical(getOption("cards.round_type"), "round-to-even")
  expect_identical(cards::label_round(1)(0.25), "0.2")
  options(op)
  expect_null(getOption("rtfreporter.rounding"))
  expect_null(getOption("cards.round_type"))
  op <- tflspec:::.study_rounding_option(sr_study("sas"))
  expect_identical(getOption("cards.round_type"), "round-half-up")
  expect_identical(cards::label_round(1)(0.25), "0.3")
  options(op)
  expect_null(getOption("cards.round_type"))
  # a study that says none leaves both
  options(cards.round_type = "round-to-even")
  st <- sr_study("sas")
  st$defaults$content <- NULL
  st$study$settings$rounding <- NULL
  expect_identical(tflspec:::.study_rounding_option(st), list())
  expect_identical(getOption("cards.round_type"), "round-to-even")
  # tfl_build_ard() leaves cards' option as it found it
  options(cards.round_type = NULL)
  sp <- sr_ard_spec("r")
  d <- exact_dir(exact_data())
  suppressMessages(tfl_build_ard(sp, dir = d, save = FALSE))
  expect_null(getOption("cards.round_type"))
})

test_that("the sheets' setup writes the rounding and cards' in one options() call, as the YAML route", {
  for (v in c("sas", "r")) {
    setup <- tfl_report_setup_code(tfl_table_spec(study = c(rounding = v)))
    hit <- grep("rtfreporter.rounding", setup, value = TRUE, fixed = TRUE)
    expect_length(hit, 1L)
    expect_match(hit, "cards.round_type", fixed = TRUE)
    expect_length(grep("cards.round_type", setup, fixed = TRUE), 1L)
  }
})
