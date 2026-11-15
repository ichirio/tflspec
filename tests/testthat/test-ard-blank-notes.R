# In study$adam a blank is NA: a test for a blank text in an R field of the
# ARD definition is a note, naming the place and the is.na() to write.

ab_spec <- function(where = "SAFFL == \"Y\"", derive = NA, post = NA, code = NA,
                    pop_where = "SAFFL == \"Y\"", rec = NA, ds_derive = NA, a_where = NA) {
  x <- list(
    datasets = data.frame(dataset = c("ADSL", "ADLB"), path = c("adsl.rds", "adlb.rds"),
                          derive = c(NA, ds_derive)),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = pop_where),
    analysis_data = data.frame(output_id = "T-1", data_id = "adlb_worst", from = "ADLB",
                               population_id = "SAF", where = where, derive = derive, code = code),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          data = "adlb_worst", by = "TRT01A", variables = "SEX",
                          where = a_where, post = post))
  if (!is.na(rec)) x$records <- data.frame(record_id = "TOX", dataset = "ADLB", where = rec)
  tfl_ard_spec(x)
}
ab_notes <- function(...) {
  msg <- character()
  withCallingHandlers(ab_spec(...), warning = function(w) {
    msg <<- c(msg, strsplit(conditionMessage(w), "\n  ", fixed = TRUE)[[1L]])
    invokeRestart("muffleWarning")
  })
  msg
}

test_that("== \"\", != \"\" and nzchar() in a where say is.na()", {
  expect_identical(
    ab_notes(where = "LBTOXL != \"\""),
    "T-1 / adlb_worst / where: LBTOXL != \"\": in study$adam a blank is NA -- write !is.na(LBTOXL)")
  expect_match(ab_notes(where = "LBTOXL == ''"), "write is.na(LBTOXL)", fixed = TRUE)
  expect_match(ab_notes(where = "'' != LBTOXL"), "write !is.na(LBTOXL)", fixed = TRUE)
  expect_match(ab_notes(where = "nzchar(LBTOXL)"), "write !is.na(LBTOXL)", fixed = TRUE)
  expect_match(ab_notes(where = "!nzchar(LBTOXL)"), "write is.na(LBTOXL)", fixed = TRUE)
  expect_match(ab_notes(where = "!nzchar( LBTOXL )"), "!nzchar( LBTOXL ): ", fixed = TRUE)
  two <- ab_notes(where = "A != \"\" & B == \"\"")
  expect_length(two, 2L)
})

test_that("every R field of the definition is looked at", {
  expect_match(ab_notes(pop_where = "SAFFL != \"\""), "^SAF / where: SAFFL", perl = TRUE)
  expect_match(ab_notes(derive = "X = ifelse(Y == \"\", 1, 2)"),
               "T-1 / adlb_worst / derive: Y == \"\"", fixed = TRUE)
  expect_match(ab_notes(where = NA, code = "filter(nzchar(Z))"), "T-1 / adlb_worst / code: nzchar(Z)", fixed = TRUE)
  expect_match(ab_notes(post = "filter(AETERM != \"\")"), "T-1 / A1 / post: AETERM != \"\"",
               fixed = TRUE)
  expect_match(ab_notes(a_where = "AEBODSYS != \"\""), "T-1 / A1 / where: AEBODSYS", fixed = TRUE)
  expect_match(ab_notes(rec = "AEREL != \"\""), "TOX / where: AEREL != \"\"", fixed = TRUE)
  expect_match(ab_notes(ds_derive = "Z = Y == \"\""), "ADLB / derive: Y == \"\"", fixed = TRUE)
})

test_that("is.na() and a value that is no blank test give no note", {
  expect_identical(ab_notes(where = "!is.na(LBTOXL)"), character())
  expect_identical(ab_notes(where = "is.na(LBTOXL) | LBTOXL == \"Y\"", a_where = "SEX == \"F\""),
                   character())
  expect_identical(ab_notes(where = NA, code = "x |> dplyr::filter(!is.na(Z))", post = "filter(n >= 2)"),
                   character())
})

test_that("tfl_check_spec() gives the note, not an error", {
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "CS-1")
  o <- tfl_output("T-AGE", "table", common = list(population = "SAF"), label = "Age",
    ard = list(analysis_data = list(list(id = "adsl_saf", from = "ADSL", population = "SAF",
                                         where = "TRT01A != \"\"")),
               analyses = list(list(id = "AGE", method = "cards::ard_summary", data = "adsl_saf",
                                    by = "TRT01A", variables = "AGE"))),
    content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-AGE.yml"))
  p <- tfl_check_spec(d, "T-AGE")
  w <- p[p$part == "ard" & grepl("a blank is NA", p$message), ]
  expect_identical(w$level, "note")
  expect_match(w$message, "write !is.na(TRT01A)", fixed = TRUE)
  expect_false(any(p$level == "error"))
})
