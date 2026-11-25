# The series template km_cards: a KM figure whose content is ggsurvfit's
# cards (the code SPEC form), its data the figure's steps.  Fictional data
# only.

kmc_study <- list(settings = list(rounding = "sas"))

kmc_report <- function(...) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c("id: F-9", "kind: figure", "label: Kaplan-Meier",
               "template:", "  name: tflspec_v1/km_cards",
               "  data:", "    by: TRT01P", "    param: OS", ...), f)
  .series_read_yaml(f)
}

kmc_fns <- function(o) vapply(o$content, function(it) .schema_fn_name(it$fn), "")

test_that("km_cards resolves: its data the figure's steps filled in, its content the cards", {
  o <- tfl_template_resolve(kmc_report(), study = kmc_study)
  expect_s3_class(o, "tfl_output")
  expect_identical(o$kind, "figure")
  expect_null(o$figure)
  expect_null(o$ard)
  steps <- o$data$steps
  expect_identical(vapply(steps, `[[`, "", "step"), c("read", "param", "flag", "time_unit", "survfit"))
  expect_identical(steps[[2L]]$value, "OS")
  expect_identical(steps[[3L]]$variable, "FASFL")
  expect_identical(steps[[5L]]$by, "TRT01P")
  expect_identical(steps[[4L]]$variable, "AVAL")
  expect_identical(steps[[4L]]$unit, "months")
  expect_identical(kmc_fns(o), c("ggsurvfit", "add_censor_mark", "scale_colour_tfl", "scale_y_continuous",
                                 "coord_cartesian", "labs", "theme_tfl", "theme", "add_risktable"))
  # each card passes the schema
  for (it in o$content) expect_identical(nrow(tfl_schema_check(it)), 0L, info = it$fn)
  # a report's own data rows have no place beside the steps
  bad <- kmc_report()
  bad$data <- list(list(id = "adsl_sub", from = "ADSL"))
  expect_error(tfl_template_resolve(bad, study = kmc_study), "a report adds no data rows")
  # the parameter is required
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c("id: F-9", "kind: figure", "template:", "  name: tflspec_v1/km_cards",
               "  data:", "    by: TRT01P"), f)
  expect_error(tfl_template_resolve(.series_read_yaml(f), study = kmc_study), "param")
})

test_that("km_cards' program is the card chain, ggsurvfit() + ... + add_risktable()", {
  o <- tfl_template_resolve(kmc_report(), study = kmc_study)
  code <- tfl_content_code(o)
  expect_true(any(grepl("fit <- survfit2(Surv(AVAL, CNSR == 0) ~ TRT01P", code, fixed = TRUE)))
  expect_true(any(grepl("filter(PARAMCD == \"OS\", FASFL == \"Y\")", code, fixed = TRUE)))
  expect_true(any(grepl("tte_time(\"months\")", code, fixed = TRUE)))
  expect_true(any(code == "content <- ggsurvfit(x = fit, linewidth = 0.3) +"))
  expect_true(any(code == "  add_censor_mark(shape = 3, size = 3, stroke = 0.6) +"))
  expect_true(any(grepl("^  add_risktable\\(risktable_stats = \"n.risk\"\\)$", code)))
  expect_true(any(grepl("labs(x = \"Time (Months)\", y = \"Survival Probability\")", code, fixed = TRUE)))
  expect_true(any(grepl("legend.position = \"bottom\"", code, fixed = TRUE)))
  expect_false(any(grepl("legend.background", code, fixed = TRUE)))
  expect_no_error(parse(text = code, keep.source = FALSE))
  # risk_table: false drops the card; x_breaks set the axis and the risk table's times
  o2 <- tfl_template_resolve(kmc_report("  content:", "    risk_table: false",
                                        "    x_breaks: [0, 6, 12]", "    legend: inside"),
                             study = kmc_study)
  expect_false("add_risktable" %in% kmc_fns(o2))
  expect_true("scale_x_continuous" %in% kmc_fns(o2))
  code2 <- tfl_content_code(o2)
  expect_false(any(grepl("add_risktable", code2, fixed = TRUE)))
  expect_true(any(grepl("scale_x_continuous(breaks = c(0, 6, 12))", code2, fixed = TRUE)))
  expect_true(any(grepl("legend.position = \"inside\"", code2, fixed = TRUE)))
  expect_true(any(grepl("legend.background = element_rect(", code2, fixed = TRUE)))
  o3 <- tfl_template_resolve(kmc_report("    time_unit: days", "  content:", "    x_breaks: [0, 100, 200]"),
                             study = kmc_study)
  code3 <- tfl_content_code(o3)
  expect_true(any(grepl("add_risktable(risktable_stats = \"n.risk\", times = c(0, 100, 200))",
                        code3, fixed = TRUE)))
  expect_true(any(grepl("tte_time(\"days\")", code3, fixed = TRUE)))
  expect_false(any(grepl("30.4375", code3, fixed = TRUE)))
  expect_true(any(grepl("Time (Days)", code3, fixed = TRUE)))
})

test_that("km_cards' x label follows each of the four time units", {
  for (u in c("days", "weeks", "months", "years")) {
    o <- tfl_template_resolve(kmc_report(paste0("    time_unit: ", u)), study = kmc_study)
    code <- tfl_content_code(o)
    expect_true(any(grepl(paste0("Time (", tools::toTitleCase(u), ")"), code, fixed = TRUE)), info = u)
  }
})

test_that("km_cards' figure builds on the package's sample data", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  adam <- tfl_example_adam()
  data <- stats::setNames(adam, tolower(names(adam)))
  param <- unique(adam$ADTTE$PARAMCD)[1L]
  for (extra in list(character(), c("  content:", "    risk_table: false", "    legend: inside"))) {
    f <- tempfile(fileext = ".yml")
    writeLines(c("id: F-9", "kind: figure", "template:", "  name: tflspec_v1/km_cards",
                 "  data:", "    by: TRT01P", paste0("    param: ", param), extra), f)
    o <- tfl_template_resolve(.series_read_yaml(f), study = kmc_study)
    unlink(f)
    p <- suppressWarnings(suppressMessages(tfl_content_run(o, data)))
    expect_true(inherits(p, "ggplot") || inherits(p, "ggsurvfit"))
    expect_no_error(suppressWarnings(suppressMessages(ggplot2::ggplot_build(p))))
  }
})
