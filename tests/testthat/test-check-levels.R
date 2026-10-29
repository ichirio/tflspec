# The check's three levels (ERROR / WARNING / OK, 2026-10-29), a report at a
# time, what is to confirm apart, and the text that asks the agent to fix a
# draft; a file that cannot be read stops no other report.

cl_study <- function(root) {
  skip_if_not_installed("haven")
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  chr <- function(x) if (is.factor(x)) as.character(x) else x
  haven::write_xpt(as.data.frame(lapply(adsl, chr)), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "EX-1")
  an <- list(id = "AGE", method = "cards::ard_summary", data = "adsl_saf", by = "TRT01A", variables = "AGE")
  ok <- tfl_output("T-OK", "table", population = "SAF", label = "Age",
    ard = list(analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF")),
               analyses = list(an)),
    content = list(tfl_item("table_plan", list(cols = "TRT01A", rows = "variable")),
                   tfl_item("plan_cells", list(continuous = "{mean} ({sd})"))),
    page = list(titles = list("1" = list(center = "Table 1"))))
  tfl_write_output(ok, file.path(d, "spec", "outputs", "T-OK.yml"))
  d
}

test_that("a file that cannot be read is an error of that report alone; the levels by report", {
  skip_on_cran()
  d <- cl_study(withr::local_tempdir())
  writeLines(c("id: T-BAD", "kind: table", "label: [unclosed"), file.path(d, "spec", "outputs", "T-BAD.yml"))
  # a report whose content item the schema refuses
  o <- tfl_read_output(file.path(d, "spec", "outputs", "T-OK.yml"))
  o$id <- "T-ERR"
  o$content[[1L]]$args$nope <- 1
  o$ai <- list(confirm = list(list(where = "content", item = "plan_cells", why = "the denominator")))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-ERR.yml"))
  p <- tfl_check_spec(d)
  expect_setequal(attr(p, "ids"), c("T-OK", "T-ERR", "T-BAD"))
  expect_true(any(p$output_id == "T-BAD" & p$part == "file" & p$level == "error" &
                    grepl("cannot be read", p$message)))
  expect_true(any(p$output_id == "T-ERR" & p$level == "error" & grepl("nope", p$item)))
  expect_true(any(p$output_id == "T-ERR" & p$level == "check"))
  expect_false(any(p$output_id == "T-OK" & p$level %in% c("error", "warning")))
  r <- tfl_check_reports(p)
  lv <- stats::setNames(r$level, r$output_id)
  expect_identical(lv[["T-OK"]], "OK")
  expect_identical(lv[["T-ERR"]], "ERROR")
  expect_identical(lv[["T-BAD"]], "ERROR")
  expect_identical(r$confirms[r$output_id == "T-ERR"], 1L)
  expect_match(r$confirm[r$output_id == "T-ERR"], "the denominator", fixed = TRUE)
  expect_true(is.na(r$confirm[r$output_id == "T-OK"]))
  # the text for the agent: the ERROR reports, their file, their problems
  fx <- tfl_check_fix_text(p)
  expect_setequal(names(fx), c("T-ERR", "T-BAD"))
  expect_match(fx[["T-ERR"]], "spec/outputs/T-ERR.yml", fixed = TRUE)
  expect_match(fx[["T-ERR"]], "ai/instructions-table.md", fixed = TRUE)
  expect_match(fx[["T-ERR"]], "- ERROR (content, table_plan(nope))", fixed = TRUE)
  expect_false(grepl("the denominator", fx[["T-ERR"]], fixed = TRUE))
  expect_match(fx[["T-BAD"]], "cannot be read", fixed = TRUE)
  ja <- tfl_check_fix_text(p, language = "ja", output_id = "T-BAD")
  expect_identical(names(ja), "T-BAD")
  expect_match(ja[["T-BAD"]], "下書き `spec/outputs/T-BAD.yml`", fixed = TRUE)
  expect_identical(length(tfl_check_fix_text(p, output_id = "T-OK")), 0L)
  # the study's reports still read for the programs: the bad file left out, with a warning
  expect_warning(outs <- tfl_read_outputs(d, skip_bad = TRUE), "T-BAD.yml")
  expect_setequal(names(outs), c("T-OK", "T-ERR"))
  expect_error(tfl_read_outputs(d))
})
