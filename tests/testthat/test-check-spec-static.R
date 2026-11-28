# tfl_check_spec(): the report as written, before any build (#534)

test_that("a cell template in Python's percent form: an error with the tflspec form", {
  o <- list(kind = "table", content = list(
    tfl_item("plan_cells", list(categorical = "{n:.0f} ({p:.1%})"))),
    template = list(name = "shift", args = list(cell = "{n} ({p:.1f%})")))
  p <- .check_static_rows(o, "T1")
  expect_identical(nrow(p), 1L)
  expect_identical(p$level, "error")
  expect_match(p$message, "\"{n:.0f} ({p:.1f%}%)\"", fixed = TRUE)
  expect_match(p$item, "plan_cells", fixed = TRUE)
  # the tflspec form and plain counts: nothing
  o$content <- list(tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")))
  expect_identical(nrow(.check_static_rows(o, "T1")), 0L)
})

test_that("a figure: an argument off with its layer added; a time axis with no unit", {
  o <- list(kind = "figure", template = list(name = "km_survival", args = list(
    censor_marks = FALSE, risk_table = FALSE, x_label = "Time")),
    layers = list(list(layer = "censor_mark", shape = "plus"), list(layer = "risk_table")))
  p <- .check_static_rows(o, "F1")
  expect_identical(p$level, c("error", "error", "warning"))
  expect_match(p$message[1L], "`censor_marks: false` and a `censor_mark` layer", fixed = TRUE)
  expect_match(p$message[3L], "\"Time (Months)\"", fixed = TRUE)
  o$template$args <- list(censor_marks = TRUE, x_label = "Time (Months)")
  expect_identical(nrow(.check_static_rows(o, "F1")), 0L)
  # a template by name alone
  expect_identical(nrow(.check_static_rows(list(kind = "figure", template = "km_simple"), "F2")), 0L)
})

test_that("a listing whose every column has a width: a note", {
  col <- function(v, w = NULL) tfl_item("listing_col", Filter(Negate(is.null),
                                                              list(vars = v, width = w)))
  o <- list(kind = "listing", content = list(tfl_item("rtf_listing", list(cols = list(
    col("USUBJID", 12), col("AEDECOD", 20), col("ASTDT", 12))))))
  p <- .check_static_rows(o, "L1")
  expect_identical(p$level, "note")
  o$content[[1L]]$args$cols[[3L]] <- col("ASTDT")
  expect_identical(nrow(.check_static_rows(o, "L1")), 0L)
})

test_that("in tfl_check_spec(): the percent form of a template's cell", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "CS-1")
  tfl_write_output(tfl_output("T-DM", "table", common = list(population = "SAF"),
    template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
    content = list(tfl_item("plan_cells", list(categorical = "{n} ({p:.1%})")))),
    file.path(d, "spec", "outputs", "T-DM.yml"))
  p <- tfl_check_spec(d)
  e <- p[p$level == "error", ]
  expect_true(any(grepl("{n} ({p:.1f%}%)", e$message, fixed = TRUE)))
})

test_that("plan_digits() by the values of a variable no analysis groups its rows by: an error", {
  an <- list(list(id = "BIGN", method = "categorical", variables = "TRT01A"),
             list(id = "VALUE", method = "continuous", by = c("TRT01A", "PARAMCD", "AVISIT"),
                  variables = "AVAL"))
  ro <- function(...) list(kind = "table", ard = list(analyses = an),
                           content = list(tfl_item("plan_digits", list(...))))
  p <- .check_digit_key_rows(ro(PARAM = list(`Alanine Aminotransferase (U/L)` = 1)), "T1")
  expect_identical(p$level, "error")
  expect_identical(p$item, "plan_digits(PARAM)")
  expect_match(p$message, "no analysis groups its rows by PARAM", fixed = TRUE)
  # a row key with its codes, a variable's or a kind's statistics, a column: fine
  expect_null(.check_digit_key_rows(ro(PARAMCD = list(ALT = 1, BILI = list(mean = 2, sd = 3)),
                                       AVAL = list(mean = 1, sd = 2), continuous = list(mean = 1),
                                       group1 = 0, default = 1), "T1"))
  # a code SPEC by the label: its key is the label's variable
  an[[2L]]$by <- c("TRT01A", "PARAM", "AVISIT")
  expect_null(.check_digit_key_rows(ro(PARAM = list(`Alanine Aminotransferase (U/L)` = 1)), "T1"))
  # the ARD's columns unknown (an analysis written as code): not judged
  an[[2L]] <- list(id = "X", method = "custom", code = "my_ard(data)")
  expect_null(.check_digit_key_rows(ro(PARAM = list(ALT = 1)), "T1"))
})

test_that("in tfl_check_spec(): plan_digits() by a variable the template's analyses do not have", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "CS-1")
  tfl_write_output(tfl_output("T-DM", "table", common = list(population = "SAF"),
    template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
    content = list(tfl_item("plan_digits", list(RACE = list(ASIAN = 1))))),
    file.path(d, "spec", "outputs", "T-DM.yml"))
  p <- tfl_check_spec(d)
  e <- p[p$level == "error", ]
  expect_true(any(e$item == "plan_digits(RACE)"))
})
