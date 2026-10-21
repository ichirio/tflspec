# The km layout and its display templates (km_disc: FDA STF Figure 1),
# called as a table's are: tfl_tpl(<data>, "<name>", ...).

# ADTTE's parameter TTDISC (time to permanent discontinuation of
# treatment) made from ADSL: the first dose to the last, CNSR 0 when the
# treatment ended early
tplkm_ttdisc <- function(adsl) {
  s <- adsl[!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT), ]
  data.frame(STUDYID = s$STUDYID, USUBJID = s$USUBJID, TRT01A = s$TRT01A,
             PARAMCD = "TTDISC", PARAM = "Time to Permanent Discontinuation of Treatment (days)",
             STARTDT = s$TRTSDT, ADT = s$TRTEDT,
             AVAL = as.numeric(s$TRTEDT - s$TRTSDT) + 1,
             CNSR = as.integer(!s$EOTSTT %in% "DISCONTINUED"), stringsAsFactors = FALSE)
}

# the layers' data of a figure (its build), to compare two figures
tplkm_pd <- function(p) {
  lapply(ggplot2::ggplot_build(p)$data, function(d) d[setdiff(names(d), "PANEL")])
}

test_that("km_simple / km_risk_table / km_ci: the same expansion as before the km layout", {
  before <- readRDS(test_path("fixtures", "tpl-km-before.rds"))
  for (nm in c("km_simple", "km_risk_table", "km_ci")) {
    x <- tfl_template_expand(nm, list(param = "PFS", group = "TRT01A"))
    for (k in c("content", "plot", "data", "call_steps")) {
      expect_identical(x[[k]], before[[nm]][[k]], info = paste(nm, k))
    }
    expect_identical(tfl_templates()$from[tfl_templates()$name == nm], "km")
  }
})

test_that("the former tpl_km_*() draw as before; tfl_tpl() draws the same", {
  skip_if_not_installed("ggsurvfit")
  # (compared within this R: a figure's built data differs a little between
  # ggplot2 / ggsurvfit versions and platforms, so no stored copy)
  df <- adtte[adtte$PARAMCD == "PFS", ]
  old <- list(km_simple = tpl_km_simple(df, group = TRT01A),
              km_risk_table = tpl_km_risk_table(df, group = TRT01A),
              km_ci = tpl_km_ci(df, group = TRT01A))
  for (nm in names(old)) {
    want <- tplkm_pd(old[[nm]])
    expect_equal(tplkm_pd(tfl_tpl(df, nm, group = TRT01A)), want, tolerance = 1e-8, info = nm)
    expect_equal(tplkm_pd(tfl_tpl(df, nm, group = "TRT01A")), want, tolerance = 1e-8, info = nm)
  }
  # the other figures: tfl_tpl() as their functions
  sb <- advs[advs$PARAMCD == "SYSBP", ]
  sb$AVISIT <- stats::reorder(factor(sb$AVISIT), sb$AVISITN)
  expect_identical(tplkm_pd(suppressWarnings(tfl_tpl(sb, "mean_se", group = TRT01A, param = "SYSBP"))),
                   tplkm_pd(suppressWarnings(tpl_mean_se(sb, group = TRT01A, param = "SYSBP"))))
  wf <- adtr[adtr$AVISIT == "WEEK 12", ]
  expect_identical(tplkm_pd(tfl_tpl(wf, "waterfall", value = PCHG)),
                   tplkm_pd(tpl_waterfall(wf, value = PCHG)))
})

test_that("km: the arguments and what the design has for them", {
  x <- function(...) tfl_template_expand("km", list(param = "PFS", ...))
  fns <- function(e) vapply(e$content, `[[`, "", "fn")
  calls <- function(e) unlist(lapply(e$content, function(it) if (identical(it$fn, "call")) it$args$fn))
  steps <- function(e) vapply(e$data$steps, `[[`, "", "step")
  # the layout's defaults: the curves, the censor marks, the number at risk
  e <- x()
  expect_identical(fns(e), c("km_curve", "censor_mark", "call"))
  expect_identical(calls(e), "add_risktable")
  # the confidence interval: a band, or bars at the axis's times
  expect_true("km_ci" %in% fns(x(ci = "band")))
  e <- x(ci = "bars")
  expect_identical(calls(e), c("geom_errorbar", "add_risktable"))
  expect_match(e$data$steps[[length(e$data$steps)]]$code, "summary(fit, times = x_breaks", fixed = TRUE)
  expect_match(x(ci = "bars", ci_times = c(4, 8))$data$steps[[6L]]$code, "times = c(4, 8)", fixed = TRUE)
  # the y axis: the risk, in %
  e <- x(y_type = "risk", y_percent = TRUE)
  expect_identical(e$content[[1L]]$args$type, "risk")
  expect_identical(e$plot$y_label, "Cumulative Percentage of Subjects")
  expect_null(e$plot$y_by)
  expect_identical(e$plot$add[[2L]]$fn, "scale_y_continuous")
  # the p-value, the median's line, no censor marks, no risk table
  expect_identical(calls(x(pvalue = TRUE)), c("add_risktable", "add_pvalue"))
  expect_true("hline" %in% fns(x(median_line = TRUE)))
  expect_false("censor_mark" %in% fns(x(censor_marks = FALSE)))
  e <- x(risk_table = FALSE)
  expect_null(calls(e))
  expect_identical(e$call_steps, 1L)
  # the axis: its end and step
  e <- x(x_max = 52, x_by = 4)
  expect_identical(e$data$steps[[length(e$data$steps)]]$code, "x_breaks <- seq(0, 52, by = 4)")
  # the data steps: the event, the unit, no parameter (ADSL)
  e <- tfl_template_expand("km", list(data = "ADSL", time = "TRTDURD", time_unit = "weeks",
                                      event = "EOTSTT == \"DISCONTINUED\""))
  expect_identical(steps(e), c("read", "derive", "time_unit", "survfit", "code"))
  expect_identical(e$data$steps[[2L]]$expr, "as.integer(!(EOTSTT == \"DISCONTINUED\"))")
  expect_identical(e$data$steps[[4L]]$censor, "CNSR")
  expect_identical(e$data$steps[[3L]]$unit, "weeks")
  # the escape hatch: any other setting
  expect_identical(x(plot = list(dpi = 150))$plot$dpi, 150)
  # the parameters: their stage and tier
  p <- tfl_template_params("km")
  st <- vapply(p, `[[`, "", "stage")
  expect_identical(sort(names(st)[st == "data"]),
                   c("data", "event", "param", "population", "subset", "where"))
  expect_true(all(c("group", "ci", "risk_table", "x_max", "y_label") %in%
                    names(p)[vapply(p, `[[`, "", "tier") == "usual"]))
})

test_that("km_disc: FDA STF Figure 1's defaults; its variants'", {
  p <- tfl_template_params("km_disc")
  expect_identical(p$y_type$default, "risk")
  expect_true(p$y_percent$default)
  expect_identical(p$time_unit$default, "weeks")
  expect_identical(p$ci$default, "bars")
  expect_true(p$pvalue$default)
  # the variants on it: km_lfu's own label, km_disc's axis
  l <- tfl_template_params("km_lfu")
  expect_identical(l$y_type$default, "risk")
  expect_identical(l$param$default, "TTLFU")
  expect_match(l$y_label$default, "Lost to Follow-Up", fixed = TRUE)
  a <- tfl_template_params("km_ae_onset")
  expect_null(a$param$default)
  expect_identical(a$ci$default, "bars")
  t <- tfl_templates()
  expect_identical(t$from[t$name == "km_lfu"], "km_disc")
  expect_true(t$ai[t$name == "km_disc"])
})

test_that("tfl_tpl(): a figure's call; the data step's parameters are not its", {
  skip_if_not_installed("ggsurvfit")
  df <- tplkm_ttdisc(adsl)
  df$AVAL <- df$AVAL / 7
  p <- tfl_tpl(df, "km_disc", group = TRT01A, x_max = 52, x_by = 4)
  expect_s3_class(p, "ggsurvfit")
  b <- ggplot2::ggplot_build(p)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), NA)))
  # the y axis: 1 - S(t), from 0 up
  expect_identical(b$data[[1L]]$y[1L], 0)
  expect_error(tfl_tpl(df, "km_disc", param = "TTDISC"), "the program's data step")
  expect_error(tfl_tpl(df, "km_disc", colour = "red"), "no parameter")
  expect_error(tfl_tpl(df, "km_disc", TRT01A), "by name")
  # the escape hatch: `plot =`
  q <- tfl_tpl(df, "km_disc", group = TRT01A, plot = list(title = "Figure 1"))
  expect_identical(q$labels$title, "Figure 1")
  # a report's own items `+`-ed after it
  r <- p + ggplot2::geom_vline(xintercept = 12)
  expect_true(any(vapply(r$layers, function(l) inherits(l$geom, "GeomVline"), NA)))
})

test_that("in a study: km_disc from ADSL and from ADTTE; A = B = the template's; PNG and RTF", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  # (ADTTE: its TTDISC made from ADSL)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(adsl), file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(tflspec:::.ai_chr(tplkm_ttdisc(adsl)), file.path(d, "adam", "adtte.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "KMD-1")
  data <- lapply(c(adsl = "adsl", adtte = "adtte"), function(x)
    as.data.frame(haven::read_xpt(file.path(d, "adam", paste0(x, ".xpt")))))
  own <- list(tfl_item("labs", list(caption = "Log-rank test.")))
  o1 <- tfl_output("F-9-disc", "figure", population = "SAF", label = "KM", extends = "defaults",
                   template = list(name = "km_disc", args = list(group = "TRT01A", x_max = 52, x_by = 4)),
                   content = own)
  x <- tplfig_check(o1, d, data)
  txt <- paste(x$a, collapse = "\n")
  expect_match(txt, "filter(PARAMCD == \"TTDISC\")", fixed = TRUE)
  expect_match(txt, "AVAL / days_per_week", fixed = TRUE)
  expect_match(txt, "\"km_disc\", group = TRT01A, x_max = 52, x_by = 4) +", fixed = TRUE)
  expect_match(txt, "labs(caption = \"Log-rank test.\")", fixed = TRUE)
  items <- paste(tfl_content_code(x$ro, template = "items"), collapse = "\n")
  expect_match(items, "ggsurvfit(fit, type = \"risk\"", fixed = TRUE)
  expect_match(items, "geom_errorbar(", fixed = TRUE)
  expect_match(items, "scales::label_percent()", fixed = TRUE)
  expect_match(items, "add_pvalue(", fixed = TRUE)
  # ADSL: no parameter, the event, the time
  o2 <- tfl_output("F-9-adsl", "figure", population = "SAF", label = "KM", extends = "defaults",
                   template = list(name = "km_disc", args = list(
                     data = "ADSL", param = NULL, time = "TRTDURD",
                     event = "EOTSTT == \"DISCONTINUED\"")))
  y <- tplfig_check(o2, d, data)
  txt <- paste(y$a, collapse = "\n")
  expect_match(txt, "mutate(CNSR = as.integer(!(EOTSTT == \"DISCONTINUED\")))", fixed = TRUE)
  expect_match(txt, "\"km_disc\", time = TRTDURD)", fixed = TRUE)
  expect_false(grepl("PARAMCD", txt, fixed = TRUE))
  # the report's whole program's end: its PNG and RTF
  st <- tfl_read_study_spec(d)
  png <- file.path(d, "F-9-disc.png")
  fig <- suppressMessages(tfl_content_run(x$ro, data))
  ggplot2::ggsave(png, fig, width = 9, height = 6, dpi = 72)
  rtf <- file.path(d, "F-9-disc.rtf")
  suppressMessages(rtfreporter::generate_rtfreport(tfl_report(x$ro, content = fig, study = st), rtf,
                                                   overwrite = TRUE, program = "p.R"))
  expect_gt(file.size(png), 1000)
  expect_true(any(grepl("\\pngblip", readLines(rtf, warn = FALSE), fixed = TRUE)))
})

test_that("the study's figure style: a km parameter's study value, in the call", {
  skip_if_not_installed("ggsurvfit")
  st <- tfl_fig_style()
  st$settings$value[st$settings$key == "censor_shape" & st$settings$type %in% "km"] <- "circle"
  op <- options(tflspec.fig_style = st)
  on.exit(options(op))
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "km_disc", args = list(group = "TRT01A")))
  ro <- tfl_resolve_output(o, s)
  tp <- attr(ro, "template")
  expect_true("censor_shape" %in% tp$study)
  expect_identical(tp$args$censor_shape, "circle")
  expect_match(paste(tfl_content_code(ro), collapse = "\n"), "censor_shape = \"circle\"", fixed = TRUE)
  # tflspec's own style: no study value
  options(op)
  tp <- attr(tfl_resolve_output(o, s), "template")
  expect_false("censor_shape" %in% tp$study)
})

test_that("a company's variant of km: its root called, its defaults written out", {
  skip_if_not_installed("ggsurvfit")
  op <- options(tflspec.template_source = "acmetpl")
  on.exit({
    options(op)
    rm(list = "acme_km", envir = tflspec:::.tpl_registry)
  })
  tfl_template_variant("acme_km", from = "km_disc", defaults = list(ci = "band", x_by = 8))
  df <- tplkm_ttdisc(adsl)
  expect_s3_class(tfl_tpl(df, "acme_km", group = TRT01A), "ggsurvfit")
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "acme_km", args = list(group = "TRT01A")))
  code <- paste(tfl_content_code(tfl_resolve_output(o, s)), collapse = "\n")
  expect_match(code, "# template acme_km (a variant of km_disc, acmetpl", fixed = TRUE)
  expect_match(code, ", \"km\",", fixed = TRUE)
  expect_match(code, "ci = \"band\"", fixed = TRUE)
  expect_match(code, "y_type = \"risk\"", fixed = TRUE)
  expect_false(grepl("param = ", code, fixed = TRUE))
})
