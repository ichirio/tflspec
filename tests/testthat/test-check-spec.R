# tfl_check_spec(): the ARD definition, each report's content and a figure's
# design against the installed packages, in one table

cs_study <- function() {
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "CS-1")
  d
}

test_that("a clean report: no rows; the versions come with it", {
  skip_if_not_installed("haven")
  d <- cs_study()
  tfl_write_output(tfl_output("T-DM", "table", population = "SAF",
                              template = list(name = "demog", args = list(population = "SAF"))),
                   file.path(d, "spec", "outputs", "T-DM.yml"))
  p <- tfl_check_spec(d)
  expect_named(p, c("output_id", "part", "item", "level", "message"))
  expect_identical(nrow(p[p$level == "error", ]), 0L)
  v <- attr(p, "versions")
  expect_identical(v$package, c("tflspec", "rtfreporter", "cards", "cardx", "ggplot2"))
  expect_identical(v$installed[1L], format(utils::packageVersion("tflspec")))
})

test_that("an argument cards does not have, a function it does not have, an item rtfreporter does not take", {
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- cs_study()
  an <- function(id, method, args = NULL) Filter(Negate(is.null), list(
    id = id, method = method, data = "adsl_saf", by = "TRT01A", variables = "AGE", args = args))
  o <- tfl_output("T-AGE", "table", population = "SAF", label = "Age",
    ard = list(analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF")),
               analyses = list(an("AGE", "cards::ard_summary", "nope = 1"))),
    content = list(tfl_item("table_plan", list(cols = "TRT01A", nope = 2))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-AGE.yml"))
  p <- tfl_check_spec(d, "T-AGE")
  w <- p[p$part == "ard", ]
  expect_true(any(w$output_id == "T-AGE" & w$item == "AGE" & w$level == "note" &
                    grepl("has no argument `nope`", w$message)))
  expect_true(any(p$part == "content" & p$item == "table_plan(nope)"))
  # a function cards does not have: an error of the ARD definition
  o$ard$analyses <- list(an("AGE", "cards::ard_nonexistent"))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-AGE.yml"))
  p2 <- tfl_check_spec(d, "T-AGE")
  expect_true(any(p2$part == "ard" & p2$level == "error" & grepl("has no ard_nonexistent", p2$message)))
  expect_error(tfl_check_spec(d, "T-NONE"), "no report")
})

test_that("build = TRUE: a report the checks pass is built; one that cannot be made has a build error row (#519)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("ggsurvfit")
  d <- cs_study()
  tfl_write_output(tfl_output("T-DM", "table", population = "SAF",
                              template = list(name = "demog", args = list(population = "SAF"))),
                   file.path(d, "spec", "outputs", "T-DM.yml"))
  # a KM of ADTTE, which the study does not have: read, it cannot be made
  tfl_write_output(tfl_output("F-KM", "figure", population = "SAF", extends = "defaults",
                              template = list(name = "km_survival", args = list(by = "TRT01A")),
                              data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                                       list(step = "param", value = "PFS")))),
                   file.path(d, "spec", "outputs", "F-KM.yml"))
  p0 <- suppressWarnings(tfl_check_spec(d))
  expect_false("build" %in% p0$part)
  p <- suppressWarnings(tfl_check_spec(d, build = TRUE))
  b <- p[p$part == "build", ]
  expect_identical(b$output_id, "F-KM")
  expect_identical(b$level, "error")
  expect_match(b$message, "the report cannot be made: ", fixed = TRUE)
  expect_false(any(p$output_id %in% "T-DM" & p$level == "error"))
  r <- tfl_check_reports(p)
  expect_identical(r$level[r$output_id == "T-DM"], "OK")
  expect_identical(r$level[r$output_id == "F-KM"], "ERROR")
  expect_true("F-KM" %in% names(tfl_check_fix_text(p)))
  # on its own
  b2 <- suppressWarnings(tfl_check_build(d))
  expect_identical(b2$output_id, "F-KM")
  expect_identical(attr(b2, "ids"), c("F-KM", "T-DM"))
  expect_identical(nrow(suppressWarnings(tfl_check_build(d, output_id = "T-DM"))), 0L)
  expect_error(tfl_check_build(d, output_id = "NOPE"), "no report")
})
