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

# ---- the look keys under plot: that do nothing in the types (#681) ---------

# what the Issue lists: the look the study's figure style sets, which a
# report's `plot:` does not reach (theme, palette, base_size and risk_title do)
km_plot_unused <- c("censor_shape", "censor_size", "censor_stroke", "median_linetype",
                    "median_colour", "median_linewidth", "risk_height", "risk_size",
                    "linewidth", "ci_alpha")
km_plot_value <- list(censor_shape = 2, censor_size = 6, censor_stroke = 2, median_linetype = "dotted",
                      median_colour = "red", median_linewidth = 2, risk_height = 0.4, risk_size = 6,
                      linewidth = 2, ci_alpha = 0.6, risk_title = "Number at risk",
                      theme = "minimal", palette = "grey", base_size = 14)

# a report of the type in a study, with the layers each key is about drawn
km_plot_report <- function(nm, plot = NULL) {
  o <- tfl_output(paste0("F-9-", nm), "figure", population = "SAF", label = "KM",
                  extends = "defaults",
                  data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                           list(step = "param", value = "PFS"))),
                  template = list(name = nm, args = list(by = "TRT01A", ci = "band",
                                                         median_line = TRUE, censor_marks = TRUE)))
  if (length(plot)) o$plot <- plot
  o
}

test_that("km_survival, km_cumulative: the look keys under plot: that do nothing are known, from the look", {
  look <- tflspec:::.tpl_km_look
  for (nm in c("km_survival", "km_cumulative")) {
    t <- tflspec:::.tpl_get(nm, "test")
    pl <- stats::setNames(rep(list(1), length(look)), look)
    expect_setequal(tflspec:::.tpl_plot_unused(t, pl), km_plot_unused)
    expect_identical(tflspec:::.tpl_plot_unused(t, pl["theme"]), character(), info = nm)
    expect_identical(tflspec:::.tpl_plot_unused(t, list(x_label = "Months")), character(), info = nm)
    expect_identical(tflspec:::.tpl_plot_unused(t, NULL), character(), info = nm)
  }
  # the layout's look is its arguments: nothing is hidden there, and the other
  # figure templates have no look to hide
  for (nm in c("km", "forest_hr", "waterfall", "mean_ci")) {
    t <- tflspec:::.tpl_get(nm, "test")
    expect_identical(tflspec:::.tpl_plot_unused(t, list(censor_size = 1)), character(), info = nm)
  }
  # each has its sentence
  expect_setequal(names(tflspec:::.tpl_km_look_what), km_plot_unused)
})

test_that("a report's plot: key that does nothing in km_survival, km_cumulative is a warning, the figure builds", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "KMR-3")
  st <- tfl_read_study_spec(d)
  resolve <- function(o) {
    w <- list()
    ro <- withCallingHandlers(suppressMessages(tfl_resolve_output(o, st)),
                              warning = function(c) {
                                w[[length(w) + 1L]] <<- c
                                invokeRestart("muffleWarning")
                              })
    list(ro = ro, w = w)
  }
  draw <- function(ro) tplfig_png(suppressMessages(suppressWarnings(tfl_content_run(ro, data))))
  for (nm in c("km_survival", "km_cumulative")) {
    base <- resolve(km_plot_report(nm))
    expect_length(base$w, 0L)
    png0 <- draw(base$ro)
    for (k in names(km_plot_value)) {
      r <- resolve(km_plot_report(nm, stats::setNames(km_plot_value[k], k)))
      info <- paste(nm, k)
      if (k %in% km_plot_unused) {
        # a warning of the key, once; the figure is the one without it
        expect_length(r$w, 1L)
        expect_s3_class(r$w[[1L]], "tfl_plot_key_unused")
        expect_identical(r$w[[1L]]$key, k)
        expect_match(conditionMessage(r$w[[1L]]),
                     sprintf("^F-9-%s: plot: %s has no effect in template %s: ", nm, k, nm), info = info)
        expect_identical(draw(r$ro), png0, info = info)
      } else {
        # a key that works: no warning, and it changes the figure
        expect_length(r$w, 0L)
        expect_false(identical(draw(r$ro), png0), info = info)
      }
    }
  }
})

test_that("the warning for a plot: key says which key, which template and where the setting is", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "KMR-4")
  st <- tfl_read_study_spec(d)
  said <- function(nm, k) {
    w <- NULL
    suppressMessages(withCallingHandlers(
      tfl_resolve_output(km_plot_report(nm, stats::setNames(km_plot_value[k], k)), st),
      tfl_plot_key_unused = function(c) {
        w <<- c$text
        invokeRestart("muffleWarning")
      }))
    w
  }
  expect_identical(said("km_survival", "censor_size"),
                   "plot: censor_size has no effect in template km_survival: its censor marks' size comes from the study's figure style (censor_size).")
  expect_identical(said("km_cumulative", "linewidth"),
                   "plot: linewidth has no effect in template km_cumulative: its curves' line width comes from the study's figure style (line_width).")
  expect_identical(said("km_survival", "ci_alpha"),
                   "plot: ci_alpha has no effect in template km_survival: its confidence band's transparency is the template's own.")
})

test_that("the draft check and tfl_check_spec() report a plot: key that does nothing as a warning", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "KMR-5")
  # tfl_check_spec(): a row of the report, a warning, the key as its item
  o <- km_plot_report("km_survival", list(censor_size = 6, theme = "minimal"))
  tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  p <- suppressMessages(tfl_check_spec(d))
  w <- p[p$part == "plot", ]
  expect_identical(w$level, "warning")
  expect_identical(w$item, "censor_size")
  expect_identical(w$output_id, o$id)
  expect_match(w$message, "^plot: censor_size has no effect in template km_survival: ")
  expect_false(any(p$level == "error" & p$part == "plot"))
  # the AI draft check: the same, a warning of the answer, which is not blocked
  ans <- tfl_ai_parse(paste(c("```yaml",
    "tflspec_ai: {task: figure, version: 1, output_id: F-9-km_survival}",
    "data: [{step: read, dataset: ADTTE}, {step: param, value: PFS}]",
    "template: {name: km_survival, args: {by: TRT01A}}",
    "plot: {censor_size: 6, theme: minimal}",
    "assumptions: []", "```"), collapse = "\n"), "figure")
  ctx <- tfl_ai_context("figure", output_id = "F-9-km_survival")
  # (with the study, the answer is resolved too: said once, not as an R warning)
  ctx_study <- ctx
  ctx_study$study_dir <- d
  for (cx in list(ctx, ctx_study)) {
    expect_no_warning(q <- suppressMessages(tfl_ai_check(ans, cx)))
    expect_identical(q$where[q$severity == "warning"], "plot censor_size")
    expect_match(q$problem[q$severity == "warning"], "^plot: censor_size has no effect in template km_survival: ")
    # (the draft keeps no analysis set, which is another problem: not about plot:)
    expect_false(any(q$severity == "error" & grepl("^plot", q$where)))
  }
})

test_that("tfl_template_to_spec() says a plot: key that does nothing once", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "KMR-6")
  st <- tfl_read_study_spec(d)
  n <- 0L
  suppressMessages(withCallingHandlers(
    tfl_template_to_spec(km_plot_report("km_survival", list(censor_size = 6)), st),
    tfl_plot_key_unused = function(c) {
      n <<- n + 1L
      invokeRestart("muffleWarning")
    }))
  expect_identical(n, 1L)
})
