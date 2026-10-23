# The km layout and its report types (km_survival, km_cumulative), called
# as a table's are: tfl_tpl(<data>, "<name>", ...); FDA STF Figures 1, 4
# and 13 as report definitions on km_cumulative.

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

test_that("km_survival / km_cumulative: the report types' defaults; the look hidden", {
  want <- list(
    km_survival = list(y_type = "survival", median_line = TRUE, risk_table = TRUE, ci = "none",
                       pvalue = TRUE, censor_marks = TRUE, legend = "inside"),
    km_cumulative = list(y_type = "risk", y_percent = TRUE, ci = "bars", risk_table = TRUE,
                         median_line = FALSE, pvalue = TRUE, censor_marks = FALSE,
                         legend = "bottom"))
  tt <- tfl_templates()
  look <- c("censor_shape", "censor_size", "censor_stroke", "median_linetype", "median_colour",
            "median_linewidth", "risk_title", "risk_height", "risk_size", "linewidth", "palette",
            "theme", "base_size", "ci_alpha")
  for (nm in names(want)) {
    p <- tfl_template_params(nm)
    for (k in names(want[[nm]])) expect_identical(p[[k]]$default, want[[nm]][[k]], info = paste(nm, k))
    expect_identical(tt$from[tt$name == nm], "km")
    expect_true(tt$ai[tt$name == nm])
    # the study's variable required, no default; the look and y_type not the type's
    expect_true(p$by$required)
    expect_null(p$by$default)
    shown <- strsplit(tt$params[tt$name == nm], ", ", fixed = TRUE)[[1L]]
    expect_false(any(c(look, "y_type") %in% shown), info = nm)
    expect_true(all(c("by", "time", "censor", "time_unit", "risk_statistics", "x_step", "y_step",
                      "conf_level", "linetype_by", "legend_title") %in% shown), info = nm)
    expect_false(any(c(look, "y_type") %in% tfl_tpl_args(nm)$param), info = nm)
    expect_true(all(vapply(p[c(look, "y_type")], function(x) isTRUE(x$hidden), NA)), info = nm)
  }
  # the layout keeps them
  expect_true(all(c(look, "y_type") %in% strsplit(tt$params[tt$name == "km"], ", ", fixed = TRUE)[[1L]]))
  # the former templates and FDA STF's: gone (report definitions now)
  gone <- c("km_simple", "km_risk_table", "km_ci", "km_disc", "km_ae_onset", "km_lfu")
  expect_false(any(gone %in% tt$name))
  expect_error(tfl_template_expand("km_simple", list(param = "PFS", by = "TRT01A")), "km_simple")
  # the expansion: the defaults' layers
  fns <- function(e) vapply(e$content, `[[`, "", "fn")
  calls <- function(e) unlist(lapply(e$content, function(it) if (identical(it$fn, "call")) it$args$fn))
  s <- tfl_template_expand("km_survival", list(param = "PFS", by = "TRT01A"))
  expect_identical(fns(s), c("km_curve", "censor_mark", "hline", "call", "call"))
  expect_identical(calls(s), c("add_risktable", "add_pvalue"))
  expect_null(s$content[[1L]]$args$type)
  expect_identical(s$plot$legend, "inside")
  k <- tfl_template_expand("km_cumulative", list(param = "TTDISC", by = "TRT01A", time_unit = "weeks"))
  expect_identical(fns(k), c("km_curve", "call", "call", "call"))
  expect_identical(calls(k), c("geom_errorbar", "add_risktable", "add_pvalue"))
  expect_identical(k$content[[1L]]$args$type, "risk")
  expect_identical(k$plot$legend, "bottom")
  expect_identical(k$plot$y_label, "Cumulative Percentage of Subjects")
  # `by`: required
  expect_error(tfl_template_expand("km_survival", list(param = "PFS")), "by")
})

test_that("the former km_simple / km_risk_table / km_ci: km_survival with arguments, the same expansion", {
  before <- readRDS(test_path("fixtures", "tpl-km-before.rds"))
  # (the p-value off: the former templates had none)
  as_of <- list(km_simple = list(risk_table = FALSE, median_line = FALSE, pvalue = FALSE),
                km_risk_table = list(pvalue = FALSE),
                km_ci = list(ci = "band", pvalue = FALSE))
  for (nm in names(as_of)) {
    x <- tfl_template_expand("km_survival", c(list(param = "PFS", by = "TRT01A"), as_of[[nm]]))
    # (the items say the template they are from)
    want <- before[[nm]]
    want$content <- lapply(want$content, function(it) {
      it$from <- "template:km_survival"
      it
    })
    for (k in c("content", "plot", "data", "call_steps")) {
      expect_identical(x[[k]], want[[k]], info = paste(nm, k))
    }
  }
})

test_that("tfl_tpl(): a column bare or as text draws the same; the other figures as their functions", {
  skip_if_not_installed("ggsurvfit")
  # (compared within this R: a figure's built data differs a little between
  # ggplot2 / ggsurvfit versions and platforms, so no stored copy)
  adtte <- tflspec::adtte  # (not ggsurvfit's, attached before tflspec)
  df <- adtte[adtte$PARAMCD == "PFS", ]
  for (nm in c("km_survival", "km_cumulative")) {
    expect_equal(tplkm_pd(tfl_tpl(df, nm, by = TRT01A)), tplkm_pd(tfl_tpl(df, nm, by = "TRT01A")),
                 tolerance = 1e-8, info = nm)
  }
  # the type's arguments: the layout's with its defaults
  expect_equal(tplkm_pd(tfl_tpl(df, "km_survival", by = TRT01A, ci = "band")),
               tplkm_pd(tfl_tpl(df, "km", by = TRT01A, ci = "band", median_line = TRUE, pvalue = TRUE,
                                legend = "inside")), tolerance = 1e-8)
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
  x <- function(...) tfl_template_expand("km", list(param = "PFS", by = "TRT01A", ...))
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
  e <- x(x_max = 52, x_step = 4)
  expect_identical(e$data$steps[[length(e$data$steps)]]$code, "x_breaks <- seq(0, 52, by = 4)")
  # the data steps: the event, the unit, no parameter (ADSL)
  e <- tfl_template_expand("km", list(data = "ADSL", by = "TRT01A", time = "TRTDURD", time_unit = "weeks",
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
  expect_true(all(c("ci", "risk_table", "x_max", "x_step", "y_label") %in%
                    names(p)[vapply(p, `[[`, "", "tier") == "usual"]))
  expect_identical(p$by$tier, "required")
  expect_false(any(c("group", "x_by", "y_by", "risk_stats") %in% names(p)))
})

test_that("tfl_tpl(): a figure's call; the data step's parameters are not its", {
  skip_if_not_installed("ggsurvfit")
  df <- tplkm_ttdisc(adsl)
  df$AVAL <- df$AVAL / 7
  p <- tfl_tpl(df, "km_cumulative", by = TRT01A, time_unit = "weeks", x_max = 52, x_step = 4)
  expect_s3_class(p, "ggsurvfit")
  b <- ggplot2::ggplot_build(p)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), NA)))
  # the y axis: 1 - S(t), from 0 up
  expect_identical(b$data[[1L]]$y[1L], 0)
  expect_error(tfl_tpl(df, "km_cumulative", by = TRT01A, param = "TTDISC"), "the program's data step")
  expect_error(tfl_tpl(df, "km_cumulative", by = TRT01A, colour = "red"), "no parameter")
  expect_error(tfl_tpl(df, "km_cumulative", TRT01A), "by name")
  expect_error(tfl_tpl(df, "km_cumulative"), "by")
  # the escape hatch: `plot =`
  q <- tfl_tpl(df, "km_cumulative", by = TRT01A, plot = list(title = "Figure 1"))
  expect_identical(q$labels$title, "Figure 1")
  # a report's own items `+`-ed after it
  r <- p + ggplot2::geom_vline(xintercept = 12)
  expect_true(any(vapply(r$layers, function(l) inherits(l$geom, "GeomVline"), NA)))
})

test_that("in a study: km_cumulative (STF Figure 1's sample) from ADTTE and from ADSL; A = B; PNG and RTF", {
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
  # the report definition's sample, its axis's end and step added
  o1 <- tfl_read_output(system.file("extdata", "report-definitions", "stf_f01_km_disc.yml",
                                    package = "tflspec"))
  o1$id <- "F-9-disc"
  o1$template$args <- c(o1$template$args, list(x_max = 52, x_step = 4))
  o1$content <- own
  x <- tplfig_check(o1, d, data)
  txt <- paste(x$a, collapse = "\n")
  expect_match(txt, "filter(PARAMCD == \"TTDISC\")", fixed = TRUE)
  expect_match(txt, "AVAL / days_per_week", fixed = TRUE)
  expect_match(txt, paste0("\"km_cumulative\", time_unit = \"weeks\", by = TRT01A, x_max = 52, ",
                           "x_step = 4, x_label = \"Time (Weeks)\""), fixed = TRUE)
  expect_match(txt, "y_label = \"Cumulative Percentage of Subjects Discontinued\") +", fixed = TRUE)
  expect_match(txt, "labs(caption = \"Log-rank test.\")", fixed = TRUE)
  items <- paste(tfl_content_code(x$ro, template = "items"), collapse = "\n")
  expect_match(items, "ggsurvfit(fit, type = \"risk\"", fixed = TRUE)
  expect_match(items, "geom_errorbar(", fixed = TRUE)
  expect_match(items, "scales::label_percent()", fixed = TRUE)
  expect_match(items, "add_pvalue(", fixed = TRUE)
  # ADSL: no parameter, the event, the time
  o2 <- tfl_output("F-9-adsl", "figure", population = "SAF", label = "KM", extends = "defaults",
                   template = list(name = "km_cumulative", args = list(
                     data = "ADSL", by = "TRT01A", time = "TRTDURD", time_unit = "weeks",
                     event = "EOTSTT == \"DISCONTINUED\"")))
  y <- tplfig_check(o2, d, data)
  txt <- paste(y$a, collapse = "\n")
  expect_match(txt, "mutate(CNSR = as.integer(!(EOTSTT == \"DISCONTINUED\")))", fixed = TRUE)
  expect_match(txt, "\"km_cumulative\", time = TRTDURD, time_unit = \"weeks\", by = TRT01A)", fixed = TRUE)
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
                  template = list(name = "km_survival", args = list(by = "TRT01A")))
  ro <- tfl_resolve_output(o, s)
  tp <- attr(ro, "template")
  expect_true("censor_shape" %in% tp$study)
  expect_identical(tp$args$censor_shape, "circle")
  expect_match(paste(tfl_content_code(ro), collapse = "\n"), "censor_shape = \"circle\"", fixed = TRUE)
  # tflspec's own style: no study value
  options(op)
  tp <- attr(tfl_resolve_output(o, s), "template")
  expect_false("censor_shape" %in% tp$study)
  # the look is the study standard's: a report's `template: args:` gives none
  o$template$args$censor_shape <- "plus"
  expect_error(tfl_resolve_output(o, s), "censor_shape")
  o$template$args$censor_shape <- NULL
  o$template$args$y_type <- "risk"
  expect_error(tfl_resolve_output(o, s), "y_type")
})

test_that("a company's variant of km: its root called, its defaults written out", {
  skip_if_not_installed("ggsurvfit")
  op <- options(tflspec.template_source = "acmetpl")
  on.exit({
    options(op)
    rm(list = "acme_km", envir = tflspec:::.tpl_registry)
  })
  tfl_template_variant("acme_km", from = "km_cumulative", defaults = list(ci = "band", x_step = 8))
  df <- tplkm_ttdisc(adsl)
  expect_s3_class(tfl_tpl(df, "acme_km", by = TRT01A), "ggsurvfit")
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure", extends = "none",
                  template = list(name = "acme_km", args = list(by = "TRT01A")))
  code <- paste(tfl_content_code(tfl_resolve_output(o, s)), collapse = "\n")
  expect_match(code, "# template acme_km (a variant of km_cumulative, acmetpl", fixed = TRUE)
  expect_match(code, ", \"km\",", fixed = TRUE)
  expect_match(code, "ci = \"band\"", fixed = TRUE)
  expect_match(code, "y_type = \"risk\"", fixed = TRUE)
  expect_false(grepl("param = ", code, fixed = TRUE))
})

test_that("FDA STF Figures 1, 4 and 13: report definitions on km_cumulative", {
  d <- system.file("extdata", "report-definitions", package = "tflspec")
  want <- c(stf_f01_km_disc = "TTDISC", stf_f04_km_ae_onset = "TTSAE", stf_f13_km_lfu = "TTLFU")
  for (nm in names(want)) {
    o <- tfl_read_output(file.path(d, paste0(nm, ".yml")))
    expect_identical(o$template$name, "km_cumulative", info = nm)
    a <- o$template$args
    expect_identical(a$param, want[[nm]], info = nm)
    e <- tfl_template_expand("km_cumulative", a)
    pr <- Filter(function(x) identical(x$step, "param"), e$data$steps)
    expect_identical(pr[[1L]]$value, want[[nm]], info = nm)
    expect_identical(e$plot$x_label, "Time (Weeks)", info = nm)
    expect_identical(e$plot$y_label, a$y_label, info = nm)
    expect_match(a$y_label, "^Cumulative Percentage of Subjects", info = nm)
  }
})
