# A series figure template's report program: tfl_content_code() writes the
# call of the figure content function the template's `figure:` names
# (.fig_content_table()).  Fictional data only.

sfc_study <- list(settings = list(rounding = "sas"))

sfc_report <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(text, f)
  .series_read_yaml(f)
}

sfc_km <- function() {
  sfc_report(c(
    "id: F-1", "kind: figure", "label: Kaplan-Meier",
    "template:", "  name: tflspec_v1/km_simple",
    "  analysis:", "    by: TRT01A", "  data:", "    time_unit: months"))
}

sfc_forest <- function() {
  sfc_report(c(
    "id: F-2", "kind: figure", "label: Forest",
    "template:", "  name: tflspec_v1/forest_hr", "  data:", "    subgroup_vars: [SEX]",
    "  analysis:", "    by: TRT01A", "    comparison: Drug X",
    "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"))
}

sfc_adtte <- function() {
  set.seed(20261117)
  n <- 40L
  data.frame(
    TRT01A = rep(c("Control", "Drug X"), each = n / 2L),
    SEX = rep(c("F", "M"), times = n / 2L),
    AVAL = round(c(stats::rexp(n / 2L, 1 / 200), stats::rexp(n / 2L, 1 / 320)) + 1),
    CNSR = stats::rbinom(n, 1L, 0.3),
    stringsAsFactors = FALSE)
}

test_that("every series figure template's figure: is in the content table, table and tfl_figure_fn() agree", {
  tb <- .fig_content_table()
  expect_identical(names(tb), names(.fig_content_fns()))
  for (k in names(tb)) expect_identical(tfl_figure_fn(k), .fig_content_fns()[[k]])
  dir <- dirname(.template_file("tflspec_v1/km_simple"))
  figs <- character()
  for (f in list.files(dir, pattern = "[.]yml$", full.names = TRUE)) {
    t <- .series_read_yaml(f)
    if (!is.null(t$figure)) figs <- c(figs, t$figure)
  }
  expect_setequal(figs, names(tb))
  # the params a table lists are the function's own
  for (k in names(tb)) {
    fn <- tfl_figure_fn(k)
    expect_true("params" %in% names(formals(fn)))
  }
})

test_that("a km_simple report's program calls km_curves() with its resolved arguments", {
  o <- tfl_template_resolve(sfc_km(), study = sfc_study)
  expect_identical(o$figure, "km_curves")
  code <- tfl_content_code(o)
  expect_identical(code, tfl_content_code(o, template = "call"))
  expect_true(any(grepl("^# template tflspec_v1/km_simple", code)))
  expect_true(any(code == "km_fit <- ggsurvfit::survfit2("))
  expect_true(any(code == "  survival::Surv(AVAL, CNSR == 0) ~ TRT01A,"))
  expect_true(any(code == "  data = adtte_sub"))
  expect_true(any(code == "content <- tflspec::km_curves("))
  expect_true(any(code == "  km_fit = km_fit,"))
  expect_true(any(code == "  ard = ard,"))
  expect_true(any(code == "    time_unit = \"months\","))
  expect_true(any(code == "    legend = \"bottom\""))
  # one argument a line, parses
  expect_no_error(parse(text = code, keep.source = FALSE))
})

test_that("a forest_hr report's program calls forest_hr() on the ARD with its comparison", {
  o <- tfl_template_resolve(sfc_forest(), study = sfc_study)
  expect_identical(o$figure, "forest_hr")
  code <- tfl_content_code(o)
  # (the look params with a default, forest_hr()'s, written as every
  # template argument with a value is)
  expect_identical(code[-1L], c("content <- tflspec::forest_hr(", "  ard = ard,", "  params = list(",
                                "    comparison = \"Drug X\",", "    size_by_n = FALSE,",
                                "    bold_headings = FALSE", "  )", ")"))
  expect_false(any(grepl("km_fit", code, fixed = TRUE)))
  expect_true(isTRUE(attr(.content_fig_code_out(o), "ard")))
})

test_that("the programs run on fictional data to a ggplot", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  d <- sfc_adtte()
  # KM: adtte_sub from the data part, the ARD's BIGN
  code <- tfl_content_code(tfl_template_resolve(sfc_km(), study = sfc_study))
  env <- new.env(parent = globalenv())
  env$adtte_sub <- d
  bign <- cards::ard_tabulate(d, variables = "TRT01A")
  bign$analysis_id <- "BIGN"
  env$ard <- bign
  eval(parse(text = code, keep.source = FALSE), envir = env)
  expect_s3_class(env$content, "ggplot")
  expect_no_error(ggplot2::ggplot_build(env$content))
  expect_s3_class(env$km_fit, "survfit")
  # forest: the HR ARD, a Cox model a condition row
  conds <- list(c("ALL", "ALL"), c("SEX", "F"), c("SEX", "M"))
  rows <- lapply(conds, function(cnd) {
    sub <- if (cnd[[1L]] == "ALL") d else d[d[[cnd[[1L]]]] == cnd[[2L]], , drop = FALSE]
    a <- cardx::ard_regression(
      survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = sub), exponentiate = TRUE)
    a$group1 <- "subgroup"
    a$group1_level <- list(cnd[[1L]])
    a$group2 <- "subgroup_lvl"
    a$group2_level <- list(cnd[[2L]])
    as.data.frame(a)
  })
  hr <- do.call(rbind, rows)
  hr$analysis_id <- "HR"
  env2 <- new.env(parent = globalenv())
  env2$ard <- hr
  eval(parse(text = tfl_content_code(tfl_template_resolve(sfc_forest(), study = sfc_study)),
             keep.source = FALSE), envir = env2)
  expect_s3_class(env2$content, "ggplot")
  expect_no_error(ggplot2::ggplot_build(env2$content))
  expect_identical(nrow(env2$content$data), 3L)
})

test_that("tfl_content_run() runs a series figure report's program on the objects it reads", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("cards")
  d <- sfc_adtte()
  bign <- cards::ard_tabulate(d, variables = "TRT01A")
  bign$analysis_id <- "BIGN"
  o <- tfl_template_resolve(sfc_km(), study = sfc_study)
  p <- tfl_content_run(o, list(adtte_sub = d, ard = bign))
  expect_s3_class(p, "ggplot")
})

test_that("a series figure naming a figure content the table lacks stops", {
  o <- tfl_template_resolve(sfc_km(), study = sfc_study)
  o$figure <- "nonesuch"
  expect_error(tfl_content_code(o), "not a figure content")
})

test_that("km_simple's legend choices and y_label default are km_curves()'s", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  t <- .series_read_yaml(.template_file("tflspec_v1/km_simple"))
  expect_identical(t$params$legend$type, "enum[inside|bottom|none]")
  expect_identical(t$params$legend$default, "bottom")
  d <- sfc_adtte()
  fit <- ggsurvfit::survfit2(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = d)
  p <- km_curves(fit)
  expect_identical(ggplot2::get_labs(p)$y, t$params$y_label$default)
  for (lg in c("inside", "bottom", "none")) {
    expect_no_error(ggplot2::ggplot_build(km_curves(fit, params = list(legend = lg))))
  }
})
