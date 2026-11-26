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
  # (the analysis set is the data's, the series' efficacy set: no step keeps it)
  expect_identical(o$data$population, "FAS")
  expect_identical(vapply(steps, `[[`, "", "step"), c("read", "param", "derive", "survfit"))
  expect_identical(steps[[2L]]$value, "OS")
  expect_identical(steps[[4L]]$by, "TRT01P")
  expect_match(as.character(steps[[3L]]$expr), "months = 30.4375)[[\"months\"]]", fixed = TRUE)
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
  expect_true(any(grepl("filter(PARAMCD == \"OS\")", code, fixed = TRUE)))
  expect_false(any(grepl("FASFL", code, fixed = TRUE)))
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
  expect_true(any(grepl("months = 30.4375)[[\"days\"]]", code3, fixed = TRUE)))
  expect_true(any(grepl("Time (Days)", code3, fixed = TRUE)))
})

test_that("km_cards' set is its data's: the records joined to the set's subjects, no flag step (A2)", {
  o <- tfl_template_resolve(kmc_report(), study = kmc_study)
  expect_identical(.report_set(o), "FAS")
  # the subjects as the report program makes them, from a study's
  # populations (.fig_subjects_part()), and the program's read of ADTTE
  # joined to them, as every A2 figure's
  x <- tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADTTE"), path = c("adsl.rds", "adtte.rds")),
    populations = data.frame(population_id = c("FAS", "SAF"), dataset = "ADSL",
                             where = c("FASFL == \"Y\"", "SAFFL == \"Y\"")),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")))
  sj <- .fig_subjects_part(x, "F-9", .report_set(o))
  expect_identical(sj$subjects$object, "adsl_sub")
  expect_match(paste(unlist(sj$code), collapse = "\n"), "FASFL == \"Y\"", fixed = TRUE)
  r <- .fig_steps_code(o$data$steps, subjects = sj$subjects)
  # (one line, however the writer breaks a long call: no space after a "(")
  txt <- gsub("\\(\\s+", "(", gsub("\\s+", " ", paste(unlist(r$code), collapse = "\n")))
  expect_match(txt, "inner_join(select(adsl_sub, USUBJID), by = \"USUBJID\"", fixed = TRUE)
  expect_false(grepl("FASFL", txt, fixed = TRUE))
})

test_that("km_cards' set: the argument population -- its default, a value, the report's data map (A2)", {
  # the default: the series' efficacy set, an argument as any other
  o <- tfl_template_resolve(kmc_report(), study = kmc_study)
  expect_identical(o$data$population, "FAS")
  expect_identical(attr(o, "args")$population, "FAS")
  expect_identical(attr(o, "origins")[["population"]], "series")
  # a value given as the template's argument
  o <- tfl_template_resolve(kmc_report("    population: SAF"), study = kmc_study)
  expect_identical(o$data$population, "SAF")
  expect_identical(attr(o, "origins")[["population"]], "report")
  # the canon's place, the report's data map: the argument's value
  rep <- kmc_report()
  rep$data <- list(population = "SAF")
  o <- tfl_template_resolve(rep, study = kmc_study)
  expect_identical(o$data$population, "SAF")
  expect_identical(.report_set(o), "SAF")
  expect_identical(attr(o, "origins")[["population"]], "report")
  # `common: population` (until it goes): the same
  rep <- kmc_report()
  rep$common <- list(population = "SAF")
  expect_identical(tfl_template_resolve(rep, study = kmc_study)$data$population, "SAF")
  # the set given twice, two values: it stops
  rep <- kmc_report("    population: ITT")
  rep$data <- list(population = "SAF")
  expect_error(tfl_template_resolve(rep, study = kmc_study), "write it once, on the report's data")
  # a data map says the set only
  rep <- kmc_report()
  rep$data <- list(population = "SAF", source = "code")
  expect_error(tfl_template_resolve(rep, study = kmc_study), "says its analysis set only")
})

test_that("km_cards' figure builds on the package's sample data", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("haven")
  adam <- tfl_example_adam()
  data <- stats::setNames(adam, tolower(names(adam)))
  param <- unique(adam$ADTTE$PARAMCD)[1L]
  # the study of the sample data (its sets: ADSL's flags, FAS among them):
  # the report's analysis set (the series' FAS) has its subjects made from
  # it, as its program does (canon 11-15); tfl_content_run() is given it
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("ADSL", "ADTTE")) {
    haven::write_xpt(.ai_chr(adam[[x]]), file.path(d, "adam", paste0(tolower(x), ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "KMC-1")
  st <- tfl_read_study_spec(d)
  for (extra in list(character(), c("  content:", "    risk_table: false", "    legend: inside"))) {
    f <- tempfile(fileext = ".yml")
    writeLines(c("id: F-9", "kind: figure", "template:", "  name: tflspec_v1/km_cards",
                 "  data:", "    by: TRT01P", paste0("    param: ", param), extra), f)
    o <- tfl_template_resolve(.series_read_yaml(f), study = kmc_study)
    unlink(f)
    expect_identical(o$data$population, "FAS")
    p <- suppressWarnings(suppressMessages(tfl_content_run(o, data, study = st)))
    expect_true(inherits(p, "ggplot") || inherits(p, "ggsurvfit"))
    expect_no_error(suppressWarnings(suppressMessages(ggplot2::ggplot_build(p))))
  }
})
