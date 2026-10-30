# A report's own data: with a figure template (#446): the report's steps
# (its data stage), then the template's call steps; the call stays on.

# ADTTE's TTDISC made from ADSL (as test-tpl-fig-km.R's)
owndata_ttdisc <- function(adsl) {
  s <- adsl[!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT), ]
  data.frame(STUDYID = s$STUDYID, USUBJID = s$USUBJID, TRT01A = s$TRT01A,
             PARAMCD = "TTDISC", PARAM = "Time to Permanent Discontinuation of Treatment (days)",
             STARTDT = s$TRTSDT, ADT = s$TRTEDT,
             AVAL = as.numeric(s$TRTEDT - s$TRTSDT) + 1,
             CNSR = as.integer(!s$EOTSTT %in% "DISCONTINUED"), stringsAsFactors = FALSE)
}

test_that("own data steps come before the template's call steps; the same figure", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(adsl), file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(tflspec:::.ai_chr(owndata_ttdisc(adsl)), file.path(d, "adam", "adtte.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "OWN-1")
  data <- lapply(c(adsl = "adsl", adtte = "adtte"), function(x)
    as.data.frame(haven::read_xpt(file.path(d, "adam", paste0(x, ".xpt")))))
  st <- tfl_read_study_spec(d)
  args <- list(by = "TRT01A", time_unit = "weeks", x_max = 52, x_step = 4, y_type = "risk")
  # the template's own data step (the layout km's; its types take the report's)
  o1 <- tfl_output("F-9-tpl", "figure", population = "SAF", label = "KM", extends = "defaults",
                   template = list(name = "km", args = c(args, list(param = "TTDISC"))))
  r1 <- tfl_resolve_output(o1, st)
  n_call <- attr(r1, "template")$call_steps
  expect_gt(n_call, 0L)
  steps <- r1$data$steps
  own_steps <- utils::head(steps, length(steps) - n_call)
  # the same data stage, said by the report
  o2 <- tfl_output("F-9-own", "figure", population = "SAF", label = "KM", extends = "defaults",
                   template = list(name = "km", args = args),
                   data = list(steps = own_steps))
  r2 <- tfl_resolve_output(o2, st)
  expect_identical(length(r2$data$steps), length(steps))
  expect_identical(utils::tail(r2$data$steps, n_call), utils::tail(steps, n_call))
  expect_identical(vapply(r2$data$steps, `[[`, "", "step"), vapply(steps, `[[`, "", "step"))
  expect_true(attr(r2, "template")$ok)
  x <- tplfig_check(o2, d, data)
  expect_match(paste(x$a, collapse = "\n"), "\"km\"", fixed = TRUE)
  p1 <- tplfig_png(suppressMessages(tfl_content_run(r1, data)))
  expect_identical(tplfig_png(suppressMessages(tfl_content_run(r2, data))), p1)
})

test_that("a report's own steps other than the template's: theirs, then the call's", {
  skip_if_not_installed("ggsurvfit")
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  own <- list(list(step = "read", dataset = "ADTTE"),
              list(step = "param", value = "PFS"))
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "km_cumulative", args = list(by = "TRT01A")),
                  data = list(steps = own))
  ro <- tfl_resolve_output(o, s)
  n <- attr(ro, "template")$call_steps
  expect_identical(utils::head(ro$data$steps, 2L), own)
  expect_length(ro$data$steps, 2L + n)
  # (the subjects with an empty group left out, the time's unit: the call's first steps)
  expect_identical(ro$data$steps[[3L]]$step, "filter")
  expect_identical(ro$data$steps[[4L]]$step, "time_unit")
  expect_identical(ro$data$steps[[5L]]$step, "survfit")
})

test_that("no own steps (the layout km), or a table template: as before; a KM type wants them", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "km_cumulative", args = list(by = "TRT01A")))
  # (a KM type: its data stage the report's -- none said, no report)
  expect_error(tfl_resolve_output(o, s), "its own `data:` steps", fixed = TRUE)
  o$data <- list(steps = list(list(step = "read", dataset = "ADTTE")))
  expect_error(tfl_resolve_output(o, s), "keep no parameter", fixed = TRUE)
  o$data <- list(steps = list(list(step = "read", dataset = "ADSL")))
  expect_true(attr(tfl_resolve_output(o, s), "template")$ok)
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "km", args = list(by = "TRT01A")))
  ro <- tfl_resolve_output(o, s)
  ex <- tfl_template_expand("km", list(by = "TRT01A"))
  expect_identical(length(ro$data$steps), length(ex$data$steps))
  expect_true(attr(ro, "template")$ok)
})

test_that("a report's own step of the template's call (a survfit): a clear stop at the check (#517)", {
  skip_if_not_installed("ggsurvfit")
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  own <- list(list(step = "read", dataset = "ADTTE"),
              list(step = "param", value = "PFS"),
              list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "TRT01A"))
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "km_survival", args = list(by = "TRT01A")),
                  data = list(steps = own))
  expect_error(tfl_resolve_output(o, s),
               "F-9 uses template km_survival, which makes its own survfit step", fixed = TRUE)
  expect_error(tfl_resolve_output(o, s), "remove survfit; keep plot and layers", fixed = TRUE)
})
