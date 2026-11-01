# tfl_fill_widths(): a study's reports say their column widths

test_that("a table with no widths gets them written, with an assumption; one with widths is left", {
  skip_if_not_installed("rtfreporter")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "adam"))
  saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
  mk <- function(id, extra = list()) {
    o <- tfl_output(id, "table", population = "SAF")
    o$extends <- list("defaults")
    o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
                                       population = "SAF", by = list("TRT01A"),
                                       variables = list("AGE"))))
    o$data <- list(source = "ard")
    o$content <- c(list(tfl_item("table_plan", list(cols = "TRT01A",
                                                    rows = list(group = "variable"))),
                        tfl_item("plan_cells", list(continuous = list(
                          n = "{N:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})")))), extra)
    tfl_write_output(o, file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  mk("T-1")
  mk("T-2", list(tfl_item("plan_columns", list(widths = list(row_label = 2, .values = 1)))))
  before <- readLines(file.path(d, "spec", "outputs", "T-2.yml"))
  set <- suppressMessages(tfl_fill_widths(d))
  expect_identical(set, "T-1")
  y <- yaml::read_yaml(file.path(d, "spec", "outputs", "T-1.yml"))
  pc <- Filter(function(it) identical(names(it), "plan_columns"), y$content)
  expect_length(pc, 1L)
  w <- pc[[1L]]$plan_columns$widths
  expect_true(length(w) >= 2L && all(unlist(w) > 0))
  # the one with widths: as it was
  expect_identical(readLines(file.path(d, "spec", "outputs", "T-2.yml")), before)
  # its assumption, once; no AI file made
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  wa <- Filter(function(x) identical(x$item, "widths"), ass)
  expect_identical(vapply(wa, `[[`, "", "output_id"), "T-1")
  expect_false(file.exists(file.path(d, "ai", "widths.yml")))
  # again: nothing to do
  expect_length(suppressMessages(tfl_fill_widths(d)), 0L)
  expect_error(tfl_fill_widths(d, output_id = "T-9"), "no report 'T-9'")
})

test_that("a table whose widths come from the study's defaults is left alone, quietly", {
  skip_if_not_installed("rtfreporter")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "adam"))
  saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
  # the defaults give every table its widths
  f <- file.path(d, "spec", "defaults.yml")
  y <- readLines(f, encoding = "UTF-8")
  at <- grep("^  - plan_blanks:", y)
  writeLines(append(y, "  - plan_columns: {widths: {row_label: 2, .values: 1}}", after = at), f)
  o <- tfl_output("T-3", "table", population = "SAF")
  o$extends <- list("defaults")
  o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
                                     population = "SAF", by = list("TRT01A"),
                                     variables = list("AGE"))))
  o$data <- list(source = "ard")
  o$content <- list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
                    tfl_item("plan_cells", list(continuous = list(n = "{N:.0f}"))))
  p <- file.path(d, "spec", "outputs", "T-3.yml")
  tfl_write_output(o, p)
  before <- readLines(p)
  # its file says no widths, the resolved report has them: nothing to do, no message
  expect_no_message(set <- tfl_fill_widths(d, output_id = "T-3"))
  expect_length(set, 0L)
  expect_identical(readLines(p), before)
  expect_false(file.exists(file.path(d, "spec", "_assumptions.yml")) &&
                 any(vapply(yaml::read_yaml(file.path(d, "spec", "_assumptions.yml")),
                            function(x) identical(x$item, "widths"), NA)))
})
