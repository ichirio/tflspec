# tfl_ai_check(): an answer through tflspec's own checks, in its context.

ai_answer <- function(task, ...) tfl_ai_parse(c("```yaml", ..., "```"), task)

ai_toc <- function(...) {
  ai_answer("toc", "tflspec_ai: {task: toc, version: 1}", "toc:", ...,
            "assumptions: []")
}

ai_fig <- function(..., id = "F-14-2-1") {
  ai_answer("figure",
            sprintf("tflspec_ai: {task: figure, version: 1, output_id: %s}", id),
            ..., "assumptions: []")
}

# a figure's context: its report's analysis set (`common: population`), which
# no step of the answer keeps
ai_fig_ctx <- function(...) {
  tfl_ai_context("figure", output_id = "F-14-2-1",
                 population = list(id = "FAS", where = 'FASFL == "Y"'), ...)
}

ai_km <- c(
  "template: km_simple",
  "data:",
  "- {step: read, dataset: ADTTE}",
  "- {step: param, value: OS}",
  "- {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}",
  "layers:",
  "- {layer: km_curve}"
)

test_that("a good toc answer has no problems", {
  a <- ai_toc("- {output_id: T-14-1-1, type: table, title: Demographics}",
              "- {output_id: F-14-2-1, type: figure, title: Survival}")
  expect_identical(nrow(tfl_ai_check(a, tfl_ai_context("toc"))), 0L)
  expect_identical(nrow(tfl_ai_check(a)), 0L)
})

test_that("a toc's id twice, a row with no id, a kind guessed: on their rows", {
  a <- ai_toc("- {output_id: T-1, type: table, title: A}",
              "- {output_id: T-1, type: table, title: B}")
  p <- tfl_ai_check(a, tfl_ai_context("toc"))
  expect_identical(p$where, c("toc row 1 field output_id", "toc row 2 field output_id"))
  expect_match(p$problem, "'T-1' is given twice")
  expect_identical(unique(p$severity), "error")

  a <- ai_toc("- {output_id: T-1, type: table, title: A}",
              "- {title: B, population: Safety}")
  p <- tfl_ai_check(a, tfl_ai_context("toc"))
  expect_identical(p$where, "toc row 2 field output_id")
  expect_match(p$problem, "no output_id")

  a <- ai_toc("- {output_id: T-1, type: table, title: A}",
              "- {section: 14.2 Efficacy}")
  p <- tfl_ai_check(a, tfl_ai_context("toc"))
  expect_identical(p$where, "toc row 2 field output_id")
  expect_identical(p$severity, "warning")
  expect_match(p$problem, "read as a section heading and left out")

  a <- ai_toc("- {output_id: T-1, type: chart, title: A}",
              "- {output_id: L-16-1, title: B}")
  p <- tfl_ai_check(a, tfl_ai_context("toc"))
  expect_identical(p$where, c("toc row 1 field type", "toc row 2 field type"))
  expect_identical(p$severity, c("warning", "warning"))
  expect_match(p$problem[1], "'chart' is not table, figure or listing; read as table")
  expect_match(p$problem[2], "blank .* read as listing")
})

test_that("a toc column outside the context's columns is an error", {
  a <- ai_toc("- {output_id: T-1, type: table, title: A, program: t1.R}")
  p <- tfl_ai_check(a, tfl_ai_context("toc", toc_fields = c("type", "title")))
  expect_identical(p$where, "toc row 1 field program")
  expect_match(p$problem, "its columns are output_id, type, title")
  # without a context every field of tfl_read_toc() is a column
  expect_identical(nrow(tfl_ai_check(a)), 0L)
})

test_that("the parse problems come first and an unread answer is not checked", {
  a <- tfl_ai_parse("no block here", "toc")
  p <- tfl_ai_check(a, tfl_ai_context("toc"))
  expect_identical(p$where, "answer")
  a <- ai_toc("- {output_id: T-1, type: table, colour: red}")
  expect_identical(tfl_ai_check(a)$where, "toc row 1 field colour")
})

test_that("a good figure answer has no problems, with the data too", {
  a <- ai_fig(ai_km)
  ctx <- ai_fig_ctx(datasets = tfl_example_adam()["ADTTE"])
  expect_identical(nrow(tfl_ai_check(a, ctx)), 0L)
  expect_identical(nrow(tfl_ai_check(a, ctx, adam = tfl_example_adam())), 0L)
})

test_that("a figure's problems keep tfl_check_fig_design()'s part and field", {
  bad <- sub("- {layer: km_curve}", "- {layer: km_curve, colour: red}", ai_km,
             fixed = TRUE)
  p <- tfl_ai_check(ai_fig(bad), ai_fig_ctx())
  expect_identical(p$where, "layers[1] km_curve field colour")
  expect_identical(p$problem, "is not a field of km_curve")
  expect_identical(p$severity, "error")

  bad <- c(ai_km, "- {layer: pie}")
  p <- tfl_ai_check(ai_fig(bad))
  expect_identical(p$where, "layers[2] pie field layer")
  expect_identical(p$problem, "unknown layer")
})

test_that("a figure for another report, a PARAMCD the data lacks", {
  ctx <- ai_fig_ctx()
  p <- tfl_ai_check(ai_fig(ai_km, id = "F-14-2-9"), ctx)
  expect_identical(p$where, "tflspec_ai")
  expect_match(p$problem, "for F-14-2-9; the prompt asked for F-14-2-1")

  bad <- sub("value: OS", "value: TTDE", ai_km, fixed = TRUE)
  p <- tfl_ai_check(ai_fig(bad), ctx, adam = tfl_example_adam())
  expect_identical(p$where, "data[2] param field value")
  expect_identical(p$problem, "no PARAMCD TTDE")
})

test_that("what defines a figure: the report's analysis set, never a step, and its parameter (A2)", {
  ctx <- ai_fig_ctx(datasets = tfl_example_adam()["ADTTE"])
  at3 <- function(step) append(ai_km, step, after = 4L)
  # a step that keeps the set by its flag, or by a condition on it: an error
  # on that step (the program joins the records to the report's subjects)
  p <- tfl_ai_check(ai_fig(at3("- {step: flag, variable: FASFL}")), ctx)
  expect_identical(p$where, "data[3] flag")
  expect_identical(p$severity, "error")
  expect_match(p$problem, "keeps the rows of the analysis set by the records' own FASFL", fixed = TRUE)
  expect_match(p$problem, "its data's `population:`", fixed = TRUE)
  p <- tfl_ai_check(ai_fig(at3("- {step: filter, expr: 'FASFL == \"Y\" & AVAL > 0'}")), ctx)
  expect_identical(p$where, "data[3] filter")
  expect_identical(p$severity, "error")
  # a record flag stays: no problem
  expect_identical(nrow(tfl_ai_check(ai_fig(at3("- {step: flag, variable: ANL01FL}")), ctx)), 0L)
  # the report names no analysis set: a warning to ask it, not a step to add
  ctx0 <- tfl_ai_context("figure", output_id = "F-14-2-1",
                         datasets = tfl_example_adam()["ADTTE"])
  p <- tfl_ai_check(ai_fig(ai_km), ctx0)
  expect_identical(p$where, "data")
  expect_identical(p$severity, "warning")
  expect_match(p$problem, "the report has no analysis set")
  # (a set by its label only is a set the report names)
  ctx1 <- tfl_ai_context("figure", output_id = "F-14-2-1", population = "Full Analysis Set",
                         datasets = tfl_example_adam()["ADTTE"])
  expect_identical(nrow(tfl_ai_check(ai_fig(ai_km), ctx1)), 0L)
  # a dataset of parameters with no parameter kept
  p <- tfl_ai_check(ai_fig(ai_km[!grepl("param", ai_km)]), ctx)
  expect_match(p$problem, "reads ADTTE, a dataset of parameters")
})

test_that("a template or a dataset outside the context is a warning", {
  ctx <- ai_fig_ctx(templates = c("km_risk_table", "km_ci"),
                        datasets = data.frame(dataset = "ADSL", variable = "USUBJID"))
  p <- tfl_ai_check(ai_fig(ai_km), ctx)
  expect_identical(p$where, c("template", "data[1] read field dataset"))
  expect_identical(p$severity, c("warning", "warning"))
  expect_match(p$problem[2], "ADTTE is not among the datasets given (ADSL)",
               fixed = TRUE)
})

test_that("tfl_ai_check() checks its arguments", {
  a <- ai_fig(ai_km)
  expect_error(tfl_ai_check(list()), "tfl_ai_answer")
  expect_error(tfl_ai_check(a, list(task = "figure")), "tfl_ai_context")
  expect_error(tfl_ai_check(a, tfl_ai_context("toc")), "the context is for the toc task")
})
