# tfl_formats(): one formats map, checked once, as the ARD or plan_digits()
# takes it; a parameter's own formats (by PARAMCD) and significant digits
# (4s) on the ARD's route (canon 2026-11-18).  cards' sample data only.

test_that("tfl_formats(): the ARD's text and plan_digits()' arguments", {
  f <- list(N = 0, mean = 1, sd = 2L, "AGE:sd" = 3, AUCLST = list(mean = 2, sd = "4s"))
  expect_identical(tfl_formats(f), "N=0 | mean=1 | sd=2 | AGE:sd=3 | AUCLST:mean=2 | AUCLST:sd=4s")
  expect_identical(tfl_formats(NULL), NA_character_)
  expect_identical(tfl_formats(list()), NA_character_)
  # a name is a parameter when by has PARAMCD and it is no variable
  pd <- tfl_formats(f, "plan_digits", by = c("TRT01A", "PARAMCD"), variables = "AVAL")
  expect_identical(pd$continuous, list(N = 0L, mean = 1L, sd = 2L))
  expect_identical(pd$PARAMCD, list(AGE = list(sd = 3L), AUCLST = list(mean = 2L, sd = "4s")))
  # else a variable's own
  pd <- tfl_formats(f, "plan_digits", by = "TRT01A", variables = c("AGE", "AVAL"))
  expect_identical(pd$AGE, list(sd = 3L))
  expect_identical(pd$AUCLST, list(mean = 2L, sd = "4s"))
  expect_null(pd$PARAMCD)
  pd <- tfl_formats(f, "plan_digits", by = "PARAMCD", variables = c("AGE", "AVAL"))
  expect_identical(pd$AGE, list(sd = 3L))
  expect_identical(pd$PARAMCD, list(AUCLST = list(mean = 2L, sd = "4s")))
  # the text the analyses sheet holds, read back
  expect_identical(tfl_formats("mean=1 | ALB:sd=4s"), "mean=1 | ALB:sd=4s")
  expect_identical(tfl_formats(list("mean=1", "ALB:sd=4s")), "mean=1 | ALB:sd=4s")
})

test_that("tfl_formats() stops what a route cannot say", {
  expect_error(tfl_formats(list(mean = "4x")), "mean: a format is")
  expect_error(tfl_formats(list(mean = 1.5)), "whole number")
  expect_error(tfl_formats(list(1, 2)), "is a map")
  expect_error(tfl_formats(list(ALB = list(1))), "ALB: is a map")
  expect_error(tfl_formats(list(p = "xx.x%"), "plan_digits"), "p: plan_digits\\(\\) takes")
  expect_identical(tfl_formats(list(p = "xx.x%", p.value = "pvalue")), "p=xx.x% | p.value=pvalue")
  expect_error(tfl_formats(list(mean = "0s")), "mean: a format is")
})

test_that("4s is fmt_signif() in the ARD program; the ARD check takes it", {
  expect_identical(tflspec:::.fmt_r("4s"), "function(x) rtfreporter::fmt_signif(x, 4L)")
  expect_identical(tflspec:::.fmt_r("12s"), "function(x) rtfreporter::fmt_signif(x, 12L)")
  expect_true(all(tflspec:::.fmt_ok(c("4s", "1s", "xx.x", "2", "pvalue"))))
  expect_false(any(tflspec:::.fmt_ok(c("0s", "s", "4.5s"))))
})

# A spec of one by_visit-like analysis of cards' ADLB: AVAL by the arm, the
# parameter and the visit, with `formats`
fmt_lb_spec <- function(formats, by = "TRTA | PARAMCD | AVISIT", variables = "AVAL", post = NA) {
  S <- .ard_spec_sheets
  tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                                list(dataset = "ADLB", path = "adam/ADLB.rds")), S$datasets),
    populations = exact_sheet(list(list(population_id = "SAF", dataset = "ADSL",
                                        where = "SAFFL == \"Y\"")), S$populations),
    analyses = exact_sheet(list(list(
      output_id = "T", analysis_id = "LB", population_id = "SAF", dataset = "ADLB",
      method = "continuous", by = by, variables = variables,
      statistics = "N | mean | sd | median | min | max", formats = formats, post = post)),
      S$analyses)))
}

fmt_lb_pick <- function(a, param, stat) {
  a <- as.data.frame(a)
  lv <- function(v) vapply(v, function(z) if (length(z)) as.character(z[[1L]]) else NA_character_, "")
  k <- which(lv(a$group2_level) == param & a$stat_name == stat)
  list(stat = unlist(a$stat[k]), fmt = unlist(a$stat_fmt[k]))
}

test_that("a parameter's own formats (PARAMCD in by): its rows only, after the format step", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  params <- sort(unique(as.character(adam$ADLB$PARAMCD)))
  p1 <- params[1L]
  sp <- fmt_lb_spec(sprintf("mean=1 | sd=2 | %s:mean=3 | %s:sd=4s", p1, p1))
  code <- paste(tfl_ard_code(sp, save = FALSE), collapse = "\n")
  expect_match(code, sprintf(paste0("update_ard_fmt_fun(stat_names = \"mean\", fmt_fun = 3L, ",
                                    "filter = group2_level == \"%s\")"), p1), fixed = TRUE)
  expect_match(code, sprintf(paste0("update_ard_fmt_fun(stat_names = \"sd\", fmt_fun = ",
                                    "function(x) rtfreporter::fmt_signif(x, 4L), ",
                                    "filter = group2_level == \"%s\")"), p1), fixed = TRUE)
  expect_match(code, "apply_fmt_fun(replace = TRUE)", fixed = TRUE)
  expect_silent(parse(text = code))
  # (the parameter's own is not a variable's: the call's fmt_fun has none)
  expect_false(grepl(paste0(p1, " ~"), code, fixed = TRUE))
  a <- suppressMessages(suppressWarnings(tfl_build_ard(sp, dir = dir, save = FALSE)))
  dec <- function(x, d) cards::label_round(d)(x)
  m <- fmt_lb_pick(a, p1, "mean")
  expect_gt(length(m$fmt), 0L)
  expect_identical(m$fmt, vapply(m$stat, dec, "", d = 3L))
  s <- fmt_lb_pick(a, p1, "sd")
  expect_identical(s$fmt, rtfreporter::fmt_signif(unlist(s$stat), 4L))
  # every other parameter: the statistics' formats
  for (p in params[-1L]) {
    m <- fmt_lb_pick(a, p, "mean")
    expect_identical(m$fmt, vapply(m$stat, dec, "", d = 1L))
    s <- fmt_lb_pick(a, p, "sd")
    expect_identical(s$fmt, vapply(s$stat, dec, "", d = 2L))
  }
})

test_that("without PARAMCD in by, name:stat stays a variable's own", {
  skip_if_not_installed("cards")
  code <- paste(tfl_ard_code(fmt_lb_spec("mean=1 | AVAL:sd=3", by = "TRTA | AVISIT"),
                             save = FALSE), collapse = "\n")
  expect_false(grepl("update_ard_fmt_fun(stat_names", code, fixed = TRUE))
  expect_match(code, "sd = 3L", fixed = TRUE)
  # PARAMCD in by, but the name is a variable of the analysis: the variable's
  code <- paste(tfl_ard_code(fmt_lb_spec("mean=1 | AVAL:sd=3"), save = FALSE), collapse = "\n")
  expect_false(grepl("update_ard_fmt_fun(stat_names", code, fixed = TRUE))
})

test_that("4s on the ARD's route: the statistic's stat_fmt is fmt_signif()'s", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  sp <- fmt_lb_spec("mean=3s | sd=2", by = "TRTA | PARAMCD")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  a <- suppressMessages(suppressWarnings(tfl_build_ard(sp, dir = dir, save = FALSE)))
  for (p in unique(as.character(adam$ADLB$PARAMCD))[1:2]) {
    m <- fmt_lb_pick(a, p, "mean")
    expect_identical(m$fmt, rtfreporter::fmt_signif(unlist(m$stat), 3L))
  }
})
