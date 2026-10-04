test_that("tfl_check_ard() finds what a report's table reads and an ARD lacks", {
  skip_if_not_installed("cards")
  ard <- cards::ard_summary(cards::ADSL, by = ARM, variables = AGE,
                            statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd")))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "ARM", rows = NA),
    cells = data.frame(output_id = "T1", variable = c("continuous", "AGE"),
                       row = c("Mean (SD)", "n"),
                       template = c("{mean} ({sd})", "{N}")))
  ok <- tfl_check_ard(ard, sp, output_id = "T1")
  expect_identical(ok$level[ok$level != "note"], character())

  less <- ard[ard$stat_name != "sd", ]
  p <- tfl_check_ard(less, sp, output_id = "T1")
  expect_true(any(p$check == "statistics" & grepl("{sd}", p$message, fixed = TRUE)))

  other <- cards::ard_summary(cards::ADSL, by = SEX, variables = BMIBL)
  p <- tfl_check_ard(other, sp, output_id = "T1")
  expect_true(any(p$check == "cols" & grepl("ARM", p$message)))
  expect_true(any(p$check == "cells" & grepl("AGE", p$message)))

  # the shape
  expect_identical(tfl_check_ard(list(1))$level, "error")
  p <- tfl_check_ard(data.frame(variable = "A", stat = 1))
  expect_match(p$message, "stat_name")
  p <- tfl_check_ard(data.frame(group1 = "ARM", variable = "AGE",
                                stat_name = "n", stat = 1))
  expect_true(any(grepl("group1_level", p$message)))
  # without a spec, the shape only
  expect_identical(tfl_check_ard(ard)$level[tfl_check_ard(ard)$level != "note"],
                   character())
})

test_that("report$ard_source is blank or import:<file>", {
  ok <- tfl_table_spec(report = data.frame(output_id = c("T1", "T2"),
                                           ard_source = c(NA, "import:cro.json")))
  expect_s3_class(ok, "tfl_table_spec")
  expect_error(tfl_table_spec(report = data.frame(output_id = "T1",
                                                  ard_source = "cro.json")),
               "import:<file>")
})

test_that("tfl_check_ard_function() tries a function of one's own", {
  skip_if_not_installed("cards")
  good <- function(data, by, variables, ...) {
    cards::ard_summary(data, by = {{ by }}, variables = {{ variables }},
                       statistic = ~ cards::continuous_summary_fns(c("N", "mean")))
  }
  p <- tfl_check_ard_function(good, cards::ADSL, by = ARM, variables = AGE,
                              stat_names = c("N", "mean"))
  expect_identical(p$level[p$level != "note"], character())
  expect_s3_class(attr(p, "ard"), "card")
  p <- tfl_check_ard_function(good, cards::ADSL, by = ARM, variables = AGE,
                              stat_names = c("N", "sd"))
  expect_true(any(p$check == "statistics" & grepl("sd", p$message)))
  plain <- function(data, ...) data.frame(x = 1)
  p <- tfl_check_ard_function(plain, cards::ADSL)
  expect_true(any(p$check == "result" & p$level == "error"))
  boom <- function(data, ...) stop("no such column")
  p <- tfl_check_ard_function(boom, cards::ADSL)
  expect_identical(p$check, "call")
  expect_match(p$message, "no such column")
})
