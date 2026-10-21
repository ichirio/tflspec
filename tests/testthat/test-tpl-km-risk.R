test_that("km_risk_table / km_ci: ggsurvfit's risk table, a layer of the curves", {
  for (nm in c("km_risk_table", "km_ci")) {
    x <- tfl_template_expand(nm, list(param = "PFS", group = "TRT01A"))
    fns <- vapply(x$content, `[[`, "", "fn")
    expect_false("risk_table" %in% fns, info = nm)
    rt <- x$content[[which(fns == "call")]]$args
    expect_identical(rt$fn, "add_risktable")
    expect_identical(rt$args$stats_label, "Number of Patients at Risk")
    expect_identical("km_ci" %in% fns, nm == "km_ci")
    expect_true("hline" %in% fns)
    expect_identical(x$call_steps, 2L)
    expect_identical(x$plot$add[[1L]]$fn, "scale_x_continuous")
  }
})

test_that("tpl_km_ci(): a report's own items go on the curves", {
  skip_if_not_installed("ggsurvfit")
  df <- adtte[adtte$PARAMCD == "PFS", ]
  p <- tpl_km_ci(df, group = TRT01A) + ggplot2::geom_vline(xintercept = 6)
  expect_s3_class(p, "ggsurvfit")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), NA)))
  expect_identical(tplfig_png(tpl_km_risk_table(df, group = TRT01A)),
                   tplfig_png(tpl_km_risk_table(df, group = "TRT01A")))
})

test_that("in a study: A (the data steps, the call) = B (the design) = the template SPEC", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "KMR-1")
  for (nm in c("km_risk_table", "km_ci")) {
    o <- tfl_output(paste0("F-9-", nm), "figure", population = "SAF", label = "KM",
                    extends = "defaults",
                    template = list(name = nm, args = list(param = "PFS", group = "TRT01A")),
                    content = list(tfl_item("geom_vline", list(xintercept = 6, linetype = "dotted")),
                                   tfl_item("labs", list(y = "Progression-free"))))
    x <- tplfig_check(o, d, data)
    expect_match(paste(x$a, collapse = "\n"),
                 sprintf("content <- tfl_tpl(adtte_pfs, \"%s\", group = TRT01A) +", nm), fixed = TRUE)
  }
})
