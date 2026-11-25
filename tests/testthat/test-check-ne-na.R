# tfl_check_spec(adam = ): `var != value` where the data have NA in var

ne_df <- function() {
  data.frame(USUBJID = paste0("S", 1:5),
             SEX = c("F", "M", NA, NA, "M"),
             SAFFL = c("Y", "Y", "Y", "N", "N"),
             AGE = c(30, 0, NA, 50, 60),
             stringsAsFactors = FALSE)
}

test_that("a condition with `!=` where the variable has NA: the count and the form that keeps them", {
  df <- ne_df()
  m <- tflspec:::.ne_na_messages('SEX != "F"', df, "ADSL")
  expect_length(m, 1L)
  expect_match(m, "`SEX != \"F\"` drops 2 rows of ADSL where SEX is NA", fixed = TRUE)
  expect_match(m, "`!SEX %in% \"F\"`", fixed = TRUE)
  # a number; the value on the left; one row
  expect_match(tflspec:::.ne_na_messages("AGE != 0", df, "ADSL"), "drops 1 row of ADSL where AGE is NA", fixed = TRUE)
  expect_match(tflspec:::.ne_na_messages('"F" != SEX', df, "ADSL"), "drops 2 rows", fixed = TRUE)
  expect_match(tflspec:::.ne_na_messages('"F" != SEX', df, "ADSL"), "`!SEX %in% \"F\"`", fixed = TRUE)
  expect_match(tflspec:::.ne_na_messages("AGE != -1", df, "ADSL"), "`!AGE %in% -1`", fixed = TRUE)
})

test_that("inside & and |: only the rows the whole condition drops are counted", {
  df <- ne_df()
  # & : the NA rows of SAFFL == "N" are dropped by the other part anyway
  m <- tflspec:::.ne_na_messages('SAFFL == "Y" & SEX != "F"', df, "ADSL")
  expect_match(m, "drops 1 row of ADSL", fixed = TRUE)
  expect_match(m, "`!SEX %in% \"F\"`", fixed = TRUE)
  # | : the NA row where the other part is TRUE is kept already
  m <- tflspec:::.ne_na_messages('SEX != "F" | SAFFL == "N"', df, "ADSL")
  expect_match(m, "drops 1 row of ADSL", fixed = TRUE)
  # nothing dropped by it: no message
  expect_length(tflspec:::.ne_na_messages('SAFFL == "N" & SEX != "F" & SAFFL == "Y"', df, "ADSL"), 0L)
  # two of them: one message each when each alone drops a row (S3 has both NA);
  # in `&` the AGE part drops nothing the SEX part does not (row S4 SEX NA, AGE 50)
  expect_length(tflspec:::.ne_na_messages('SEX != "F" | AGE != 0', df, "ADSL"), 2L)
  expect_length(tflspec:::.ne_na_messages('SEX != "F" & AGE != 0', df, "ADSL"), 1L)
  # nested in a call
  expect_length(tflspec:::.ne_na_messages('(SEX != "F") & !is.na(AGE)', df, "ADSL"), 1L)
})

test_that("no NA, `%in%` forms, a vector, a variable not in the data, text that is not R: no message", {
  df <- ne_df()
  expect_length(tflspec:::.ne_na_messages('SAFFL != "N"', df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages('!SEX %in% "F"', df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages('!SEX %in% c("F", "M")', df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages('SEX == "M"', df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages('(SEX != "F" | is.na(SEX))', df, "ADSL"), 0L)
  # a vector value, two names, a name against a name, NA
  expect_length(tflspec:::.ne_na_messages('SEX != c("F", "M")', df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages("SEX != AGE", df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages("SEX != NA", df, "ADSL"), 0L)
  # a variable the data do not have; text that does not parse
  expect_length(tflspec:::.ne_na_messages('NOPE != "F"', df, "ADSL"), 0L)
  expect_length(tflspec:::.ne_na_messages('SEX != "F', df, "ADSL"), 0L)
  # the text is looked at as R, not as text: a quoted `!=` is no condition
  expect_length(tflspec:::.ne_na_messages('SEX == "a != b"', df, "ADSL"), 0L)
})

test_that("a condition that cannot be run here: the NA count of the variable", {
  df <- ne_df()
  m <- tflspec:::.ne_na_messages('some_fn(AGE) > 1 & SEX != "F"', df, "ADSL")
  expect_match(m, "drops 2 rows of ADSL where SEX is NA", fixed = TRUE)
})

ne_study <- function() {
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "NE-1")
  where <- 'SEX != "F"'
  o <- tfl_output("T-NE", "table", common = list(population = "SAF"), label = "Age",
    ard = list(analysis_data = list(list(id = "adsl_sub", from = "ADSL", population = "SAF", where = where)),
               analyses = list(list(id = "AGE", method = "cards::ard_summary", data = "adsl_sub",
                                    by = "TRT01A", variables = "AGE"),
                               list(id = "AGE2", method = "cards::ard_summary", data = "adsl_sub",
                                    by = "TRT01A", variables = "AGE", where = 'AGE != 0'))),
    content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-NE.yml"))
  d
}

test_that("tfl_check_spec(adam = ): a warning row of the report, counted by tfl_check_reports()", {
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- ne_study()
  adam <- list(ADSL = ne_df())
  base <- tfl_check_spec(d, "T-NE")
  p <- tfl_check_spec(d, "T-NE", adam = adam)
  w <- p[grepl("drops", p$message, fixed = TRUE), ]
  expect_identical(nrow(w), 2L)
  expect_true(all(w$output_id == "T-NE" & w$part == "ard" & w$level == "warning"))
  i <- grepl("analysis data adsl_sub", w$item, fixed = TRUE)
  expect_match(w$message[i], "analysis data adsl_sub: `where` is `SEX != \"F\"`: `SEX != \"F\"` drops 2 rows of ADSL where SEX is NA", fixed = TRUE)
  expect_match(w$message[i], "write `!SEX %in% \"F\"`", fixed = TRUE)
  expect_match(w$message[!i], "analysis AGE2", fixed = TRUE)
  expect_match(w$message[!i], "drops 1 row of ADSL where AGE is NA", fixed = TRUE)
  # the rows the data add are these and no other
  expect_identical(nrow(p), nrow(base) + 2L)
  rep <- tfl_check_reports(p)
  expect_identical(rep$warnings[rep$output_id == "T-NE"], tfl_check_reports(base)$warnings + 2L)
  expect_identical(rep$level[rep$output_id == "T-NE"], "WARNING")
})

test_that("tfl_check_spec(adam = ): no data, no NA, another dataset or no such variable: the result is as without", {
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- ne_study()
  base <- tfl_check_spec(d, "T-NE")
  expect_identical(tfl_check_spec(d, "T-NE", adam = NULL), base)
  # data with no NA in SEX and AGE
  ok <- ne_df()
  ok$SEX[is.na(ok$SEX)] <- "M"
  ok$AGE[is.na(ok$AGE)] <- 1
  expect_identical(tfl_check_spec(d, "T-NE", adam = list(ADSL = ok)), base)
  # the dataset the condition applies to is not in the data
  expect_identical(tfl_check_spec(d, "T-NE", adam = list(ADAE = ne_df())), base)
  # the variable is not in the dataset
  expect_identical(tfl_check_spec(d, "T-NE", adam = list(ADSL = ne_df()[, c("USUBJID", "SAFFL")])), base)
  # the form that keeps the NA rows
  o <- tfl_read_output(file.path(d, "spec", "outputs", "T-NE.yml"))
  o$ard$analysis_data[[1L]]$where <- '!SEX %in% "F"'
  o$ard$analyses[[2L]]$where <- '!AGE %in% 0'
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-NE.yml"))
  expect_identical(nrow(tfl_check_spec(d, "T-NE", adam = list(ADSL = ne_df()))),
                   nrow(tfl_check_spec(d, "T-NE")))
})
