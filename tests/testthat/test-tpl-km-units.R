# The KM templates on real ADaM: AVAL's unit from AVALU, the subjects with an
# empty group, the p-value clear of the legend

# tflspec's adtte (ggsurvfit's, attached by an earlier test, has no TRT01A)
adtte <- tflspec::adtte

# A time_unit step's program on `df`, run: the data after it (its program
# calls the study helper tte_time())
km_unit_run <- function(df, unit, variable = "AVAL") {
  code <- .fig_steps_code(list(list(step = "time_unit", variable = variable, unit = unit)))$code
  env <- new.env(parent = .fig_pkg_env("dplyr"))
  eval(parse(text = .fig_unit_guard()), env)
  eval(parse(text = tfl_helpers_code()), env)
  env$df <- df
  eval(parse(text = code), env)
  env$df
}

test_that("time_unit on AVAL: the time from its unit, AVALU, by tte_time()", {
  # the step is one pipe step on the study helper, not a block of code
  code <- .fig_steps_code(list(list(step = "time_unit", variable = "AVAL", unit = "months")))$code
  expect_true(any(grepl("tte_time(\"months\")", code, fixed = TRUE)))
  expect_false(any(grepl("AVALU|case_when|days_per", code)))
  # another variable: a division by the constant, as before
  oth <- .fig_steps_code(list(list(step = "time_unit", variable = "TRTDURD", unit = "weeks")))$code
  expect_true(any(grepl("TRTDURD / days_per_week", oth, fixed = TRUE)))
  expect_false(any(grepl("tte_time", oth, fixed = TRUE)))
  skip_if_not_installed("dplyr")
  d <- function(aval, avalu) data.frame(AVAL = aval, AVALU = avalu)
  # days -> weeks, months
  expect_equal(km_unit_run(d(c(14, 70), "DAYS"), "weeks")$AVAL, c(2, 10))
  expect_equal(km_unit_run(d(c(30.4375, 365.25), "DAYS"), "months")$AVAL, c(1, 12))
  # months -> months: unchanged
  expect_equal(km_unit_run(d(c(0.59, 23.92), "MONTHS"), "months")$AVAL, c(0.59, 23.92))
  # weeks -> months, years -> months, months -> days
  expect_equal(km_unit_run(d(4, "WEEKS"), "months")$AVAL, 28 / 30.4375)
  expect_equal(km_unit_run(d(c(1, 2.5), "YEARS"), "months")$AVAL, c(12, 30))
  expect_equal(km_unit_run(d(2, "MONTHS"), "days")$AVAL, 60.875)
  # any case; AVALU says the new unit after it
  x <- km_unit_run(d(c(7, 1), c("days", "Weeks")), "weeks")
  expect_equal(x$AVAL, c(1, 1))
  expect_identical(x$AVALU, c("WEEKS", "WEEKS"))
  # no AVALU: the time in days, as before
  expect_equal(km_unit_run(data.frame(AVAL = c(60.875, 365.25)), "months")$AVAL, c(2, 12))
  expect_equal(km_unit_run(data.frame(AVAL = 21), "weeks")$AVAL, 3)
  # `days` is a unit like the others, not "no conversion": months -> days
  expect_equal(km_unit_run(d(c(1, 2), "MONTHS"), "days")$AVAL, c(30.4375, 60.875))
  # a unit it does not know (or none): stops, a wrong time is the worst
  expect_error(km_unit_run(d(c(1, 2), c("HOURS", "HOURS")), "months"), "AVALU HOURS")
  expect_error(km_unit_run(d(1, NA), "months"), "AVALU is missing")
})

test_that("time_unit on AVAL: two units in the data stop the program", {
  skip_if_not_installed("dplyr")
  d <- function(aval, avalu) data.frame(AVAL = aval, AVALU = avalu)
  expect_error(km_unit_run(d(c(30.4375, 3, 1, 14), c("DAYS", "MONTHS", "YEARS", "WEEKS")), "months"),
               "two or more units")
  expect_error(km_unit_run(d(c(1, 2), c("DAYS", "MONTHS")), "days"), "two or more units")
  # (a parameter's own unit: the step is after the parameter filter)
  both <- d(c(1, 2, 30.4375), c("MONTHS", "MONTHS", "DAYS"))
  both$PARAMCD <- c("OS", "OS", "TTSKIN")
  expect_equal(km_unit_run(both[both$PARAMCD == "OS", ], "days")$AVAL, c(30.4375, 60.875))
  expect_equal(km_unit_run(both[both$PARAMCD == "TTSKIN", ], "months")$AVAL, 1)
  # another variable: a time in days
  expect_equal(km_unit_run(data.frame(TRTDURD = 70, AVALU = "MONTHS"), "weeks", "TRTDURD")$TRTDURD, 10)
})

test_that("km_survival on tflspec::adtte OS (AVAL in months): an axis of 0 to 24 months", {
  skip_if_not_installed("ggsurvfit")
  df <- adtte[adtte$PARAMCD == "OS", ]
  expect_true(all(df$AVALU == "MONTHS"))
  p <- suppressWarnings(tfl_tpl(df, "km_survival", by = TRT01A))
  b <- ggplot2::ggplot_build(p)
  tm <- unlist(lapply(b$data, function(x) x$x))
  expect_gt(max(tm), 20)
  expect_lte(max(tm), 24)
  # the medians are the data's own, in months
  keep <- !is.na(df$TRT01A)
  fit <- survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = df[keep, ])
  want <- unname(summary(fit)$table[, "median"])
  pd <- p$data
  got <- vapply(split(pd, as.character(pd$strata))[c("Drug X", "Control")], function(x) {
    min(x$time[x$estimate <= 0.5])
  }, 1)
  expect_equal(unname(got), want, tolerance = 1e-8)
  expect_true(all(want > 1 & want < 24))
  expect_identical(p$labels$x, "Time (Months)")
})

test_that("the subjects with an empty group: left out, with a warning, in the program too", {
  skip_if_not_installed("ggsurvfit")
  df <- adtte[adtte$PARAMCD == "OS", ]
  df$TRT01A <- as.character(df$TRT01A)
  expect_identical(sum(is.na(df$TRT01A)), 2L)
  # (a blank is NA in study$adam)
  df$TRT01A[which(!is.na(df$TRT01A))[1L]] <- NA
  for (nm in c("km_survival", "km_cumulative")) {
    expect_warning(p <- tfl_tpl(df, nm, by = TRT01A),
                   sprintf("%s: 3 subjects with an empty TRT01A were left out", nm), fixed = TRUE)
    # only the named groups: the curves (and so the rows at risk)
    expect_setequal(unique(as.character(p$data$strata)), c("Drug X", "Control"))
  }
  # none empty: no warning
  ok <- df[!is.na(df$TRT01A), ]
  expect_no_warning(tfl_tpl(ok, "km_survival", by = TRT01A))
  # the program (B): the filter with its comment, and the warning
  x <- tfl_template_expand("km_survival", list(by = "TRT01A"))
  code <- paste(.fig_steps_code(x$data$steps)$code, collapse = "\n")
  expect_match(code, "# the subjects with an empty TRT01A have no curve: left out, with a warning",
               fixed = TRUE)
  expect_match(code, "filter(!is.na(TRT01A))", fixed = TRUE)
  expect_match(code, paste0("warning(\"km_survival: \", n_before - nrow(df), ",
                            "\" subjects with an empty TRT01A were left out\", call. = FALSE)"),
               fixed = TRUE)
})

test_that("the p-value: in a corner the curves and the legend leave free", {
  pv <- function(name, ...) {
    x <- tfl_template_expand(name, list(by = "TRT01A", ...))
    it <- Filter(function(it) identical(it$args$fn, "add_pvalue"), x$content)[[1L]]
    it$args$args
  }
  # survival: the bottom left (the legend inside at the top right)
  expect_identical(pv("km_survival"),
                   list(location = "annotation", x = 0, y = 0.05, hjust = 0, vjust = 0))
  # ... the top right (ggsurvfit's) when the legend is at the bottom left
  expect_identical(pv("km_survival", legend = "inside_bl"), list(location = "annotation"))
  expect_identical(pv("km_survival", legend = "inside_bl", x_max = 24),
                   list(location = "annotation", x = 24, hjust = 1))
  # cumulative: the bottom right (ggsurvfit's), at an axis's end; the top
  # left when the legend is at the bottom right
  expect_identical(pv("km_cumulative"), list(location = "annotation"))
  expect_identical(pv("km_cumulative", x_max = 52), list(location = "annotation", x = 52, hjust = 1))
  expect_identical(pv("km_cumulative", legend = "inside_br"),
                   list(location = "annotation", x = 0, y = 0.95, hjust = 0, vjust = 1))
  # a caption: no place
  expect_identical(pv("km_survival", pvalue_location = "caption"), list(location = "caption"))
})
