# The `digits` sheet: each statistic's decimals written once, a variable's
# exceptions over them; a table of stat_fmt is not rounded here.

dg_spec <- function(digits = NULL, tables = NULL, cells = NULL) {
  tfl_table_spec(
    tables = tables %||% data.frame(output_id = "T1", cols = "TRT", rows = "group = variable"),
    cells = cells %||% data.frame(
      output_id = NA, variable = c("continuous", "categorical"),
      row = c("Mean (SD)", NA), template = c("{mean} ({sd})", "{n} ({p})")),
    digits = digits)
}
code_of <- function(sp) paste(tfl_table_code(sp, "T1", pipe = "|>"), collapse = "\n")

test_that("a statistic's decimals fill the tokens that say none", {
  sp <- dg_spec(data.frame(output_id = NA, variable = NA,
                           statistic = c("mean", "sd", "n", "p"), digits = c(1, 2, 0, 1)))
  code <- code_of(sp)
  expect_match(code, '"Mean (SD)" = "{mean:.1f} ({sd:.2f})"', fixed = TRUE)
  # p is a percent
  expect_match(code, 'categorical = "{n:.0f} ({p:.1f%})"', fixed = TRUE)
})

test_that("a variable's exception is that variable's own rows, the rest unchanged", {
  sp <- dg_spec(data.frame(output_id = NA, variable = c(NA, NA, "WEIGHTBL"),
                           statistic = c("mean", "sd", "mean"), digits = c(1, 2, 2)))
  code <- code_of(sp)
  expect_match(code, 'continuous = list("Mean (SD)" = "{mean:.1f} ({sd:.2f})")', fixed = TRUE)
  expect_match(code, 'WEIGHTBL = list("Mean (SD)" = "{mean:.2f} ({sd:.2f})")', fixed = TRUE)
  # a categorical exception goes to the categorical rows
  sp2 <- dg_spec(data.frame(output_id = NA, variable = "SEX", statistic = "p", digits = 0))
  expect_match(code_of(sp2), 'SEX = "{n} ({p:.0f%})"', fixed = TRUE)
  # a report's own rule replaces the default of the same statistic
  sp3 <- dg_spec(data.frame(output_id = c(NA, "T1"), variable = NA,
                            statistic = "mean", digits = c(1, 3)))
  expect_match(code_of(sp3), "{mean:.3f}", fixed = TRUE)
})

test_that("a template's own format and a row's digits win over the rule", {
  sp <- dg_spec(
    data.frame(output_id = NA, variable = NA, statistic = c("mean", "sd"), digits = c(1, 2)),
    cells = data.frame(output_id = NA, variable = "continuous",
                       row = c("Mean", "SD"), template = c("{mean:.3f}", "{sd}"),
                       digits = c(NA, "4")))
  code <- code_of(sp)
  expect_match(code, 'Mean = "{mean:.3f}"', fixed = TRUE)
  expect_match(code, 'SD = "{sd:.4f}"', fixed = TRUE)
})

test_that("a table of the ARD's own text (stat_fmt) is not rounded here", {
  sp <- dg_spec(
    data.frame(output_id = NA, variable = NA, statistic = c("mean", "sd"), digits = c(1, 2)),
    tables = data.frame(output_id = "T1", cols = "TRT", rows = "group = variable",
                        value = "stat_fmt"))
  code <- code_of(sp)
  expect_match(code, '"Mean (SD)" = "{mean} ({sd})"', fixed = TRUE)
  expect_false(grepl(".1f", code, fixed = TRUE))
})

test_that("no digits sheet: the templates as written (as before)", {
  expect_match(code_of(dg_spec()), '"Mean (SD)" = "{mean} ({sd})"', fixed = TRUE)
})

test_that("a digits row says its statistic and a whole number", {
  expect_error(dg_spec(data.frame(output_id = NA, variable = NA, statistic = NA, digits = 1)),
               "needs a `statistic`")
  expect_error(dg_spec(data.frame(output_id = NA, variable = NA, statistic = "mean", digits = 1.5)),
               "whole number")
  expect_error(dg_spec(data.frame(output_id = NA, variable = NA, statistic = c("mean", "mean"),
                                  digits = 1)), "two rows")
})

test_that("the rules print in the table", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_summary(variables = c(AGE, BMIBL),
                                             statistic = ~ cards::continuous_summary_fns(c("mean", "sd"))))
  sp <- dg_spec(
    data.frame(output_id = NA, variable = c(NA, NA, "BMIBL"), statistic = c("mean", "sd", "mean"),
               digits = c(1, 2, 3)),
    tables = data.frame(output_id = "T1", cols = "TRT", rows = "group = variable"),
    cells = data.frame(output_id = NA, variable = "continuous", row = "Mean (SD)",
                       template = "{mean} ({sd})"))
  data <- rtfreporter::normalize_ard(ard)
  pg <- suppressMessages(rtfreporter::as_rtftables(tfl_table_plan(data, sp, "T1")))
  d <- pg[[1]]$data
  age <- unlist(d[d$group == "AGE" & d$label %in% "Mean (SD)", -(1:2)])
  bmi <- unlist(d[d$group == "BMIBL" & d$label %in% "Mean (SD)", -(1:2)])
  expect_true(all(grepl("^[0-9]+\\.[0-9] \\([0-9]+\\.[0-9]{2}\\)$", age)))
  expect_true(all(grepl("^[0-9]+\\.[0-9]{3} \\([0-9]+\\.[0-9]{2}\\)$", bmi)))
})
