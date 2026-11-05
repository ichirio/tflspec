# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

test_that("km_survival, its band too: ggsurvfit's risk table, a layer of the curves", {
  for (nm in c("none", "band")) {
    x <- tfl_template_expand("km_survival", list(param = "PFS", by = "TRT01A", ci = nm, pvalue = FALSE))
    fns <- vapply(x$content, `[[`, "", "fn")
    expect_false("risk_table" %in% fns, info = nm)
    rt <- x$content[[which(fns == "call")]]$args
    expect_identical(rt$fn, "add_risktable")
    expect_identical(rt$args$stats_label, "Number of Patients at Risk")
    expect_identical("km_ci" %in% fns, nm == "band")
    expect_true("hline" %in% fns)
    expect_identical(x$call_steps, 4L)
    expect_identical(x$plot$add[[1L]]$fn, "scale_x_continuous")
  }
})

test_that("km_survival: a report's own items go on the curves", {
  skip_if_not_installed("ggsurvfit")
  df <- adtte[adtte$PARAMCD == "PFS", ]
  p <- tfl_tpl(df, "km_survival", by = TRT01A, ci = "band") + ggplot2::geom_vline(xintercept = 6)
  expect_s3_class(p, "ggsurvfit")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), NA)))
  expect_identical(tplfig_png(tfl_tpl(df, "km_survival", by = TRT01A)),
                   tplfig_png(tfl_tpl(df, "km_survival", by = "TRT01A")))
})

test_that("in a study: A (the data steps, the call) = B (the design) = the template SPEC", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "KMR-1")
  for (nm in c("km_survival", "km_cumulative")) {
    o <- tfl_output(paste0("F-9-", nm), "figure", population = "SAF", label = "KM",
                    extends = "defaults",
                    data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                             list(step = "param", value = "PFS"))),
                    template = list(name = nm, args = list(by = "TRT01A")),
                    content = list(tfl_item("geom_vline", list(xintercept = 6, linetype = "dotted")),
                                   tfl_item("labs", list(y = "Progression-free"))))
    x <- tplfig_check(o, d, data)
    expect_match(paste(x$a, collapse = "\n"),
                 sprintf("content <- tflspec::tfl_tpl(adtte_pfs, \"%s\", by = TRT01A) +", nm), fixed = TRUE)
  }
})

test_that("km_survival, km_cumulative: a report's plot: risk_title is the risk table's heading", {
  for (nm in c("km_survival", "km_cumulative")) {
    title_of <- function(plot = NULL) {
      x <- tfl_template_expand(nm, list(param = "PFS", by = "TRT01A", plot = plot))
      rt <- Filter(function(it) identical(it$args$fn, "add_risktable"), x$content)
      rt[[1L]]$args$args$stats_label
    }
    expect_identical(title_of(), "Number of Patients at Risk")
    expect_identical(title_of(list(x_label = "Months")), "Number of Patients at Risk")
    expect_identical(title_of(list(risk_title = "Number at risk")), "Number at risk")
  }
})

test_that("in a study: km_survival with plot: risk_title, the code carries it and rebuilds the same", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "KMR-2")
  mk <- function(id, plot) {
    o <- tfl_output(id, "figure", population = "SAF", label = "KM",
                    extends = "defaults",
                    data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                             list(step = "param", value = "PFS"))),
                    template = list(name = "km_survival", args = list(by = "TRT01A")))
    o$plot <- plot
    o
  }
  x <- tplfig_check(mk("F-9-rt", list(risk_title = "Number at risk")), d, data)
  expect_match(paste(x$a, collapse = "
"), "risk_title = \"Number at risk\"", fixed = TRUE)
  y <- tplfig_check(mk("F-9-rd", NULL), d, data)
  expect_false(grepl("risk_title", paste(y$a, collapse = "
"), fixed = TRUE))
})
