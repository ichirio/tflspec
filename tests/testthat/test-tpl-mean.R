test_that("mean_se / mean_ci: the design form; the summary the call's step", {
  for (nm in c("mean_se", "mean_ci")) {
    x <- tfl_template_expand(nm, list(param = "SYSBP", data = "ADVS", population = "SAF"),
                             population = list(list(id = "SAF", dataset = "ADSL",
                                                    where = "SAFFL == \"Y\"")))
    st <- vapply(x$data$steps, `[[`, "", "step")
    expect_identical(st, c("read", "join", "param", "filter", "levels", "summary"), info = nm)
    expect_identical(x$call_steps, 1L)
    expect_identical(x$data$steps[[2L]]$how, "semi")
    expect_identical(vapply(x$content, `[[`, "", "fn"), c("line", "point", "errorbar"))
    expect_identical(x$data$steps[[6L]]$interval, sub("mean_", "", nm))
  }
  expect_identical(tfl_template_params("mean_se")$value$type, "vars")
})

test_that("tpl_mean_se(): the means of the records it is given", {
  skip_if_not_installed("dplyr")
  df <- advs[advs$PARAMCD == "SYSBP", ]
  df$AVISIT <- stats::reorder(factor(df$AVISIT), df$AVISITN)
  p <- tpl_mean_se(df, group = TRT01A, param = "SYSBP")
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$y, "Mean (+/- SE) SYSBP")
  expect_identical(tplfig_png(p), tplfig_png(tpl_mean_se(df, group = "TRT01A", param = "SYSBP")))
  expect_identical(suppressWarnings(tpl_mean_ci(df))$labels$y, "Mean (95% CI)")
})

test_that("in a study: A (the data steps, the call) = B (the design) = the template SPEC", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "advs"), "MEAN-1")
  for (nm in c("mean_se", "mean_ci")) {
    o <- tfl_output(paste0("F-9-", nm), "figure", population = "SAF", label = "Mean SBP",
                    extends = "defaults",
                    template = list(name = nm, args = list(param = "SYSBP", data = "ADVS",
                                                           value = "CHG")),
                    content = list(tfl_item("geom_hline", list(yintercept = 0, linetype = "dashed")),
                                   tfl_item("labs", list(x = "Visit (cycle)"))))
    # (a visit with one record: no SD, its interval NaN -- the preset's)
    x <- suppressWarnings(tplfig_check(o, d, data))
    txt <- paste(x$a, collapse = "\n")
    expect_match(txt, "semi_join(", fixed = TRUE)
    expect_match(txt, "filter(PARAMCD == \"SYSBP\", ", fixed = TRUE)
    expect_match(txt, sprintf(paste0("content <- tfl_tpl(advs_sysbp, \"%s\", param = \"SYSBP\", ",
                                     "value = CHG) +"), nm), fixed = TRUE)
  }
})
