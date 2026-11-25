# The old quick KM (tfl_fig_km(), the plot list sheets): AVAL's unit from
# AVALU by tte_time(), "as_is" AVAL as it is, and the number at risk from the
# KM table's ARD converted from the data's own unit as the curve is

km_code_text <- function(code) paste(unlist(code), collapse = "\n")

# the script of a quick KM spec with its time_unit option set to `unit`
km_code_unit <- function(unit, ...) {
  spec <- attr(tfl_fig_km(...), "spec")
  o <- spec$options
  o$value[o$key == "time_unit"] <- unit
  spec$options <- o
  km_code_text(tfl_fig_code(spec))
}

test_that("as_is keeps AVAL as it is: no tte_time() step", {
  code <- km_code_unit("as_is")
  expect_false(grepl("tte_time(", code, fixed = TRUE))
  expect_match(code, "km_fit <- survfit2(", fixed = TRUE)
  expect_match(code, 'labs(x = "Time", ', fixed = TRUE)
  # with the table's ARD: its times are used as they are, no unit kept
  code <- km_code_unit("as_is", ard = "km_table_ard")
  expect_false(grepl("tte_time(", code, fixed = TRUE))
  expect_false(grepl("km_unit", code, fixed = TRUE))
  expect_match(code, "  time   = as.numeric(unlist(km_ard$variable_level)),\n", fixed = TRUE)
  expect_no_error(parse(text = code))
})

test_that("days_to_months converts AVAL by tte_time(\"months\")", {
  code <- km_code_text(tfl_fig_km(time_unit = "months"))
  expect_match(code, 'tte_time("months")', fixed = TRUE)
  expect_false(grepl("km_unit", code, fixed = TRUE))
  expect_identical(km_code_unit("days_to_months"), code)
  expect_no_error(parse(text = code))
})

test_that("the risk table from the ARD converts its times from the data's unit", {
  for (style in c("risk_table", "single_arm")) {
    code <- km_code_text(tfl_fig_km(style = style, time_unit = "months", ard = "km_table_ard"))
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    unit_at <- grep('^km_unit <- if \\("AVALU" %in% names\\(km_df\\)\\) km_df\\$AVALU\\[1\\] else "DAYS"', lines)
    tte_at <- grep('tte_time("months")', lines, fixed = TRUE)
    risk_at <- grep('risk_df$time <- tte_time(data.frame(AVAL = risk_df$time, AVALU = km_unit), "months")$AVAL',
                    lines, fixed = TRUE)
    expect_length(unit_at, 1L)
    expect_length(tte_at, 1L)
    expect_length(risk_at, 1L)
    # the unit is taken before the curve's data is converted, the ARD's
    # times after the ARD is read and before the curve's count is checked
    expect_true(unit_at < tte_at)
    expect_true(risk_at > grep("km_ard  <- km_table_ard", lines, fixed = TRUE))
    expect_true(risk_at < grep("sr <- summary(km_fit, times = sort(unique(risk_df$time))", lines, fixed = TRUE))
    expect_false(grepl("/ 30.4375", code, fixed = TRUE))
    expect_no_error(parse(text = code))
  }
  # another time column is in days: divided, as the curve's is
  code <- km_code_text(tfl_fig_km(time = "TTEDY", time_unit = "months", ard = "km_table_ard"))
  expect_false(grepl("tte_time(", code, fixed = TRUE))
  expect_false(grepl("km_unit", code, fixed = TRUE))
  expect_match(code, "mutate(TTEDY = TTEDY / 30.4375)", fixed = TRUE)
  expect_match(code, "as.numeric(unlist(km_ard$variable_level)) / 30.4375", fixed = TRUE)
  expect_no_error(parse(text = code))
})

test_that("the ARD's number at risk agrees with the curve in each AVALU", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  months <- c(0, 6, 12, 18, 24)
  for (u in c("DAYS", "MONTHS")) {
    tte <- adam$ADTTE
    if (u == "MONTHS") tte$AVAL <- tte$AVAL / 30.4375
    tte$AVALU <- u
    adam_u <- adam
    adam_u$ADTTE <- tte
    d <- tte[tte$PARAMCD == "OS" & tte$FASFL == "Y", ]
    # the table's ARD: its times in the data's own unit
    km_table_ard <- cardx::ard_survival_survfit(
      survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = d),
      times = if (u == "DAYS") months * 30.4375 else months)
    code <- km_code_text(tfl_fig_km(adam_u, param = "OS", ard = "km_table_ard",
                                    x_max = 24, x_by = 6))
    e <- new.env()
    e$adtte <- tte
    e$km_table_ard <- km_table_ard
    w <- character()
    withCallingHandlers(
      suppressMessages(eval(parse(text = sub('file.path("output", "km.png")',
                                             'file.path(tempdir(), "km.png")',
                                             code, fixed = TRUE)), envir = e)),
      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
    expect_false(any(grepl("Figure check", w)), label = u)
    expect_identical(e$km_unit, u)
    expect_equal(sort(unique(e$risk_df$time)), months, label = u)
  }
})
