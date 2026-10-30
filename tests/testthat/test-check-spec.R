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

test_that("page: titles under a study header that prints the label, title and population: a warning (#518)", {
  skip_if_not_installed("haven")
  d <- cs_study()
  writeLines(c("page:", "  header:", "    '4': {center: '{OUTPUT_LABEL}'}", "    '5': {center: '{OUTPUT_TITLE}'}",
               "    '6': {center: '({OUTPUT_POPULATION})'}"),
             file.path(d, "spec", "defaults.yml"))
  o <- tfl_output("T-DM", "table", population = "SAF", label = "Demographics", extends = "defaults",
                  template = list(name = "demog", args = list(population = "SAF")),
                  page = list(titles = list("1" = list(center = "Table 14.1.1"),
                                            "2" = list(center = "Demographics"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-DM.yml"))
  p <- tfl_check_spec(d)
  w <- p[p$part == "page", ]
  expect_identical(nrow(w), 1L)
  expect_identical(w$level, "warning")
  expect_match(w$message, "{OUTPUT_LABEL}, {OUTPUT_TITLE}, {OUTPUT_POPULATION}", fixed = TRUE)
  expect_match(w$message, "Remove `page: titles`", fixed = TRUE)
  # the report's own tokens fill the header: its label, its analysis set's label
  st <- tfl_read_study_spec(d)
  tk <- tfl_report_tokens(o, study = st)
  expect_identical(unname(tk["OUTPUT_TITLE"]), "Demographics")
  expect_identical(unname(tk["OUTPUT_POPULATION"]), "Safety Population")
  # without its titles: no warning
  o$page <- NULL
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-DM.yml"))
  expect_false("page" %in% tfl_check_spec(d)$part)
  # the agent's instructions say so
  s <- tflspec:::.ai_spec_settings(d)
  expect_identical(s$titles_in_header, c("OUTPUT_LABEL", "OUTPUT_TITLE", "OUTPUT_POPULATION"))
  expect_match(tflspec:::.ai_titles_text(s$titles_in_header), "no `page: titles`", fixed = TRUE)
  expect_match(tflspec:::.ai_titles_text(character()), "`page: titles`, from the shell", fixed = TRUE)
})

test_that("build = TRUE applies the cells' formats: a Python pattern stops the build with the R form (#aitest-03)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- cs_study()
  tfl_write_output(tfl_output("T-DM", "table", population = "SAF",
                              template = list(name = "demog", args = list(population = "SAF",
                                                                           count_cell = "{n} ({p:.1%})"))),
                   file.path(d, "spec", "outputs", "T-DM.yml"))
  p <- suppressWarnings(tfl_check_spec(d, build = TRUE))
  b <- p[p$part == "build", ]
  expect_identical(b$output_id, "T-DM")
  expect_match(b$message, "the table's cells: ", fixed = TRUE)
  expect_match(b$message, "Unknown format spec '.1'", fixed = TRUE)
  expect_match(b$message, "A Python form? Write `{p:.1f%}`", fixed = TRUE)
})

test_that("the sheets carry the report's own tokens under a study header that prints them; the TOC's title when there is no label (#518)", {
  skip_if_not_installed("haven")
  d <- cs_study()
  writeLines(c("page:", "  header:", "    '4': {center: '{OUTPUT_LABEL}'}", "    '5': {center: '{OUTPUT_TITLE}'}",
               "    '6': {center: '<{OUTPUT_POPULATION}>'}"),
             file.path(d, "spec", "defaults.yml"))
  tfl_write_output(tfl_output("T-DM", "table", population = "SAF", label = "Demographics", extends = "defaults",
                              template = list(name = "demog", args = list(population = "SAF"))),
                   file.path(d, "spec", "outputs", "T-DM.yml"))
  sh <- suppressWarnings(tfl_sheets_from_yaml(d))
  tk <- sh$table$tokens
  expect_identical(tk$value[tk$output_id %in% "T-DM" & tk$name == "OUTPUT_TITLE"], "Demographics")
  expect_identical(tk$value[tk$output_id %in% "T-DM" & tk$name == "OUTPUT_POPULATION"], "Safety Population")
  # the sheets' way gives the same as the report's
  r <- tfl_report_tokens(sh$table, "T-DM")
  expect_identical(unname(r[c("OUTPUT_LABEL", "OUTPUT_TITLE", "OUTPUT_POPULATION")]),
                   c("T-DM", "Demographics", "Safety Population"))
  # no label: the TOC's title
  st <- tfl_read_study_spec(d)
  st$toc <- list(outputs = list(list(id = "T-X", title = "From the TOC")))
  own <- tflspec:::.page_own_tokens(tfl_output("T-X", "table", population = "SAF"), st)
  expect_identical(own$OUTPUT_TITLE, "From the TOC")
})
