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

ai_km <- c(
  "template: km_simple",
  "data:",
  "- {step: read, dataset: ADTTE}",
  "- {step: param, value: OS}",
  "stats:",
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
  ctx <- tfl_ai_context("figure", output_id = "F-14-2-1",
                        datasets = tfl_example_adam()["ADTTE"])
  expect_identical(nrow(tfl_ai_check(a, ctx)), 0L)
  expect_identical(nrow(tfl_ai_check(a, ctx, adam = tfl_example_adam())), 0L)
})

test_that("a figure's problems keep tfl_check_fig_design()'s part and field", {
  bad <- sub("- {layer: km_curve}", "- {layer: km_curve, colour: red}", ai_km,
             fixed = TRUE)
  p <- tfl_ai_check(ai_fig(bad), tfl_ai_context("figure", output_id = "F-14-2-1"))
  expect_identical(p$where, "layers[1] km_curve field colour")
  expect_identical(p$problem, "is not a field of km_curve")
  expect_identical(p$severity, "error")

  bad <- c(ai_km, "- {layer: pie}")
  p <- tfl_ai_check(ai_fig(bad))
  expect_identical(p$where, "layers[2] pie field layer")
  expect_identical(p$problem, "unknown layer")
})

test_that("a figure for another report, a PARAMCD the data lacks", {
  ctx <- tfl_ai_context("figure", output_id = "F-14-2-1")
  p <- tfl_ai_check(ai_fig(ai_km, id = "F-14-2-9"), ctx)
  expect_identical(p$where, "tflspec_ai")
  expect_match(p$problem, "for F-14-2-9; the prompt asked for F-14-2-1")

  bad <- sub("value: OS", "value: TTDE", ai_km, fixed = TRUE)
  p <- tfl_ai_check(ai_fig(bad), ctx, adam = tfl_example_adam())
  expect_identical(p$where, "data[2] param field value")
  expect_identical(p$problem, "no PARAMCD TTDE")
})

test_that("a template or a dataset outside the context is a warning", {
  ctx <- tfl_ai_context("figure", output_id = "F-14-2-1",
                        templates = c("km_risk_table", "km_ci"),
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
