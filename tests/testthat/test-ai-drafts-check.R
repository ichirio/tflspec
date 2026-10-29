# The person's check of the agent's drafts, after the comparison of
# 2026-10-29 (one bad draft stopped every report; the check passed what the
# build refused; a figure's page lost; footnotes over the study's footer;
# an unquoted comma unseen): each report on its own, the check as strict as
# the build, the draft's own file kept.

dc_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                 adam = "adam", quiet = TRUE, size = 3L)
  file.copy(list.files(system.file("ai", "example", "drafts", package = "tflspec"), full.names = TRUE),
            file.path(d, "ai", "spec", "outputs"), overwrite = TRUE)
  d
}
dc_draft <- function(d, id) file.path(d, "ai", "spec", "outputs", paste0(id, ".yml"))

test_that("one report's mistake stops that report alone: the ARD definition a report at a time", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- dc_study(withr::local_tempdir())
  # T-14-3-1's analysis reads its data and names a population: refused
  o <- tfl_read_output(dc_draft(d, "T-14-3-1"))
  o$ard$analysis_data <- list(list(id = "ae_sub", from = "ADAE", population = "SAF", where = "TRTEMFL == \"Y\""))
  o$ard$analyses[[1L]]$data <- "ae_sub"
  tfl_write_output(o, dc_draft(d, "T-14-3-1"))
  withr::local_options(tflspec.outputs_dir = file.path("ai", "spec", "outputs"))
  st <- tfl_read_study_spec(d)
  expect_error(tfl_ard_code(st, output_id = "T-14-3-1", save = FALSE), "not both")
  expect_error(tfl_ard_code(st, save = FALSE), "not both")
  # the other reports' programs are written all the same
  expect_no_error(tfl_ard_code(st, output_id = "T-14-1-1", save = FALSE))
  expect_no_error(tfl_ard_code(st, output_id = "T-14-3-5", save = FALSE))
  # the check says it of that report, and of no other
  p <- tfl_check_spec(st)
  bad <- p[p$level == "error", , drop = FALSE]
  expect_true(all(bad$output_id == "T-14-3-1"))
  expect_true(any(grepl("not both", bad$message)))
  # the drafts' check too: that request alone is an error
  r <- suppressMessages(tfl_ai_collect_answers(d))
  st_ <- stats::setNames(r$reports$status, r$reports$output_id)
  expect_identical(unname(st_[["T-14-3-1"]]), "error")
  expect_true(all(st_[c("T-14-1-1", "T-14-3-5", "T-14-2-1")] %in% c("ok", "warning")))
})

test_that("an analysis data made by code may name its population: the set's subjects in reach as <dataset>_sub", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- dc_study(withr::local_tempdir())
  o <- tfl_read_output(dc_draft(d, "T-14-3-1"))
  o$ard$analysis_data <- list(list(
    id = "ae_first", label = "each subject's first AE", from = "ADAE", population = "SAF",
    code = tfl_r("dplyr::slice_head(dplyr::group_by(dplyr::filter(adae, USUBJID %in% adsl_sub$USUBJID, TRTEMFL == \"Y\"), USUBJID), n = 1)")))
  o$ard$analyses <- list(list(id = "AE1", method = "categorical", data = "ae_first", by = list("TRT01A"),
                              variables = list("AESEV"), denominator = "column"))
  o$content <- list(tfl_item("table_plan", list(cols = "TRT01A", rows = "AESEV")),
                    tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")))
  tfl_write_output(o, dc_draft(d, "T-14-3-1"))
  withr::local_options(tflspec.outputs_dir = file.path("ai", "spec", "outputs"))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, output_id = "T-14-3-1", save = FALSE)
  expect_true(any(grepl("adsl_sub <- ", code)))
  expect_true(any(grepl("ae_first <- ", code)))
  # the draft's check does not hold the code's columns to the ADaM's variables
  r <- suppressMessages(tfl_ai_collect_answers(d))
  z <- r$reports[r$reports$output_id == "T-14-3-1", ]
  expect_false(grepl("is not a variable", z$problems_text %||% ""))
  expect_true(z$status %in% c("ok", "warning"))
})

test_that("a figure's draft keeps its own file through the check: its page, its footer", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- dc_study(withr::local_tempdir())
  f <- dc_draft(d, "F-14-2-2")
  o <- tfl_read_output(f)
  o$page$titles <- list("1" = list(center = "Figure 14.2.2"), "2" = list(center = "Kaplan-Meier Plot of Overall Survival"))
  o$page$footer <- list("1" = list(left = "CI = confidence interval."))
  tfl_write_output(o, f)
  r <- suppressMessages(tfl_ai_collect_answers(d))
  expect_true(r$reports$status[r$reports$output_id == "F-14-2-2"] %in% c("ok", "warning"))
  back <- tfl_read_output(f)
  expect_identical(back$page$titles[["2"]]$center, "Kaplan-Meier Plot of Overall Survival")
  # the footer line moved below the study's own (spec/defaults.yml: line 1)
  expect_identical(names(back$page$footer), "2")
  expect_identical(back$page$footer[["2"]]$left, "CI = confidence interval.")
  # a table's footer lines too, once: the second check leaves them
  t <- tfl_read_output(dc_draft(d, "T-14-1-1"))
  t$page$footer <- list("1" = list(left = "Age at informed consent."), "2" = list(left = "Fictional data."))
  tfl_write_output(t, dc_draft(d, "T-14-1-1"))
  suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(names(tfl_read_output(dc_draft(d, "T-14-1-1"))$page$footer), c("2", "3"))
  suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(names(tfl_read_output(dc_draft(d, "T-14-1-1"))$page$footer), c("2", "3"))
})

test_that("a page line with a key that is no cell (an unquoted comma): an error that says to quote", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- dc_study(withr::local_tempdir())
  f <- dc_draft(d, "T-14-1-1")
  o <- tfl_read_output(f)
  o$page <- NULL
  tfl_write_output(o, f)
  writeLines(c(readLines(f, encoding = "UTF-8"),
               "page:", "  footer:", "    \"5\": {left: MedDRA version 27.0, fictional dictionary.}"), f)
  r <- suppressMessages(tfl_ai_collect_answers(d))
  z <- r$reports[r$reports$output_id == "T-14-1-1", ]
  expect_identical(z$status, "error")
  expect_match(z$problems_text, "must be quoted", fixed = TRUE)
})
