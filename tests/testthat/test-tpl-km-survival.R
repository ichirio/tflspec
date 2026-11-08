# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

km_skip <- function() {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("dplyr")
}

test_that("km_survival: the curves of the data it is given, a column bare or as text", {
  km_skip()
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  df <- adtte[adtte$PARAMCD == "PFS" & adtte$USUBJID %in% saf, ]
  p <- tfl_tpl(df, "km_survival", by = TRT01A, time_unit = "days", risk_table = FALSE)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "Time (Days)")
  expect_identical(tplfig_png(p), tplfig_png(tfl_tpl(df, "km_survival", by = "TRT01A",
                                                     time_unit = "days", risk_table = FALSE)))
  # the settings over the template's
  q <- tfl_tpl(df, "km_survival", by = TRT01A, plot = list(x_label = "Days"))
  expect_identical(q$labels$x, "Days")
  expect_error(tfl_tpl("adtte", "km_survival", by = TRT01A), "a data frame")
})

test_that("km_survival and a figure's run attach no package", {
  km_skip()
  was <- c("package:ggsurvfit", "package:dplyr") %in% search()
  df <- adtte[adtte$PARAMCD == "PFS", ]
  p <- tfl_tpl(df, "km_survival", by = TRT01A)
  expect_s3_class(p, "ggplot")
  expect_identical(c("package:ggsurvfit", "package:dplyr") %in% search(), was)
})

test_that("in a study: km_survival, A (the data steps, the call) = B (the design) = the template SPEC", {
  skip_on_cran()
  km_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "KM-1")
  o <- tfl_output("F-9-1", "figure", population = "SAF", label = "KM curves",
                  extends = "defaults",
                  data = list(steps = list(
                    list(step = "read", dataset = "ADTTE"),
                    list(step = "join", dataset = "ADSL", where = "SAFFL == \"Y\"", by = "USUBJID",
                         how = "semi"),
                    list(step = "param", value = "PFS"))),
                  template = list(name = "km_survival",
                                  args = list(by = "TRT01A", time_unit = "days")),
                  content = list(tfl_item("geom_hline", list(yintercept = 0.5, linetype = "dashed")),
                                 tfl_item("labs", list(y = "Probability of no event"))))
  x <- tplfig_check(o, d, data)
  txt <- paste(x$a, collapse = "\n")
  expect_match(txt, "semi_join(", fixed = TRUE)
  expect_match(txt, "filter(PARAMCD == \"PFS\")", fixed = TRUE)
  expect_match(txt, "content <- tflspec::tfl_tpl(adtte_pfs, \"km_survival\", time_unit = \"days\", by = TRT01A) +",
               fixed = TRUE)
  items <- paste(tfl_content_code(x$ro, template = "items"), collapse = "\n")
  expect_match(items, "add_risktable(", fixed = TRUE)
  expect_match(items, "add_pvalue(", fixed = TRUE)
  expect_match(items, "geom_hline(", fixed = TRUE)
  expect_match(txt, "labs(y = \"Probability of no event\")", fixed = TRUE)
})

test_that("the B file in the figure designer: the report's own items seen, saved the same", {
  skip_on_cran()
  km_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "KM-2")
  st <- tfl_read_study_spec(d)
  o <- tfl_read_output(system.file("extdata", "template-spec", "F-14-2-3.yml", package = "tflspec"))
  fb <- file.path(d, "spec", "outputs", "F-14-2-3.yml")
  tfl_write_output(tfl_template_to_spec(o, st), fb)
  # the report file keeps the own items' syntax
  b <- tfl_read_output(fb)
  expect_identical(vapply(b$content, `[[`, "", "fn"),
                   c("km_curve", "censor_mark", "geom_hline", rep("tfl_ard_number", 3L), "labs"))
  # the designer's design of it (the sheets bridge), written and read as
  # the designer keeps it (spec/figures/<id>.yml)
  fd <- tfl_sheets_from_yaml(d)$figures[["F-14-2-3"]]
  design <- do.call(tfl_fig_design, fd[intersect(names(fd), names(formals(tfl_fig_design)))])
  kinds <- vapply(design$layers, `[[`, "", "layer")
  expect_identical(kinds, c("km_curve", "censor_mark", "call", rep("ard_number", 3L)))
  expect_identical(design$layers[[3L]]$fn, "geom_hline")
  expect_identical(design$layers[[3L]]$args$yintercept, 0.5)
  expect_identical(design$layers[[4L]]$label, "Median (days), Drug X: {value}")
  expect_identical(design$layers[[6L]]$analysis_id, "HR")
  expect_identical(design$plot$add[[1L]], list(fn = "labs", args = list(y = "Probability of No Skin Disorder")))
  expect_identical(design$plot$x_label, "Time (Days)")
  f1 <- tempfile(fileext = ".yml")
  f2 <- tempfile(fileext = ".yml")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  tfl_write_fig_design(design, f1)
  back <- tfl_read_fig_design(f1)
  tfl_write_fig_design(back, f2)
  expect_identical(readLines(f2), readLines(f1))
  expect_identical(unclass(back), unclass(design))
  # the design read back writes the report's figure
  expect_identical(as.character(tfl_fig_design_code(back, plot_id = "F-14-2-3", save = FALSE,
                                                    name = "content", order_arms = TRUE)),
                   tflspec:::.content_fig_code(tfl_resolve_output(b, st)))
})

test_that("a report that replaces a template's layer: its design, not the call", {
  km_skip()
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure",
                  data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                           list(step = "param", value = "PFS"))),
                  template = list(name = "km_survival", args = list(by = "TRT01A")),
                  content = list(tfl_item("censor_mark", list(shape = "plus", size = 2, stroke = 0.5))))
  ro <- tfl_resolve_output(o, s)
  expect_false(attr(ro, "template")$ok)
  expect_message(code <- tfl_content_code(ro), "its design is written")
  expect_false(any(grepl("tfl_tpl(", code, fixed = TRUE)))
})

test_that("a figure template's body says the steps its call makes (call_steps)", {
  k <- tfl_template_expand("km_survival", list(param = "PFS", by = "TRT01A", risk_table = FALSE))
  # (the one-parameter check, the two filters, the time's unit, the fit)
  expect_identical(k$call_steps, 5L)
  expect_identical(k$data$steps[[length(k$data$steps)]]$step, "survfit")
  t <- .tpl_get("km_survival", "test")
  suppressMessages(tfl_template("km_nocall", "figure", "km", params = t$params,
                                body = function(a, ctx) {
                                  x <- t$body(a, ctx)
                                  x$call_steps <- NULL
                                  x
                                }))
  on.exit(rm("km_nocall", envir = .tpl_registry))
  expect_error(tfl_template_expand("km_nocall", list(param = "PFS", by = "TRT01A", risk_table = FALSE)),
               "call_steps")
})

test_that("km_survival(legend_median = TRUE): each arm's median (95% CI) in the legend's text", {
  km_skip()
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  df <- adtte[adtte$PARAMCD == "PFS" & adtte$USUBJID %in% saf, ]
  p <- tfl_tpl(df, "km_survival", by = TRT01A, time_unit = "days", risk_table = FALSE,
               legend_median = TRUE)
  expect_s3_class(p, "ggplot")
  lab <- p$scales$get_scales("colour")$labels
  expect_identical(sort(names(lab)), sort(unique(as.character(df$TRT01A))))
  expect_true(all(grepl("^.+: (NE|[0-9.]+) [(](NE|[0-9.]+), (NE|[0-9.]+)[)]$", lab)))
  # without it, the legend's text is the arms' values
  q <- tfl_tpl(df, "km_survival", by = TRT01A, time_unit = "days", risk_table = FALSE)
  expect_s3_class(q$scales$get_scales("colour")$labels, "waiver")
})

# The code that makes the legend's labels, from the template's expansion
km_labels_code <- function(template = "km_survival", ...) {
  k <- tfl_template_expand(template, list(by = "TRT01A", legend_median = TRUE, ...))
  code <- unlist(lapply(k$data$steps, function(st) if (identical(st$step, "code")) st$code))
  grep("legend_labels <- ", unlist(strsplit(code, "\n")), fixed = TRUE, value = TRUE)
}

test_that("legend_median_text: the default text gives the code it always did", {
  old <- paste0('legend_labels <- stats::setNames(sprintf("%s: %s (%s, %s)", arm_of, fmt_med(med[, "median"]),\n',
                '  fmt_med(med[, grep("LCL$", colnames(med))[1]]), fmt_med(med[, grep("UCL$", colnames(med))[1]])), arm_of)')
  k <- tfl_template_expand("km_survival", list(by = "TRT01A", legend_median = TRUE))
  code <- unlist(lapply(k$data$steps, function(st) if (identical(st$step, "code")) st$code))
  expect_true(any(grepl(old, code, fixed = TRUE)))
  for (tpl in c("km_survival", "km_cumulative", "km")) {
    a <- list(by = "TRT01A", legend_median = TRUE, param = if (tpl == "km") "PFS")
    b <- c(a, list(legend_median_text = "{arm}: {median} ({lcl}, {ucl})"))
    expect_identical(tfl_template_expand(tpl, b)$data$steps, tfl_template_expand(tpl, a)$data$steps)
  }
})

test_that("legend_median_text: tokens left out, repeated and reordered; a % and quotes kept", {
  # the sprintf() the text becomes: a %s a token, the arguments in token order, a literal % doubled
  lab <- km_labels_code(legend_median_text = "{arm}, Median (95% CI): {median} ({lcl}, {ucl})")
  expect_true(grepl('sprintf("%s, Median (95%% CI): %s (%s, %s)", arm_of, fmt_med(med[, "median"]),', lab[1], fixed = TRUE))
  lab <- km_labels_code(legend_median_text = "{median} [{ucl}-{lcl}] {arm} {arm}")
  expect_true(grepl('sprintf("%s [%s-%s] %s %s", fmt_med(med[, "median"]), fmt_med(med[, grep("UCL$"', lab[1], fixed = TRUE))
  lab <- km_labels_code(legend_median_text = "\"{arm}\" \\ 5%")
  expect_true(grepl('sprintf("\\"%s\\" \\\\ 5%%", arm_of)', lab[1], fixed = TRUE))
  expect_error(parse(text = lab), NA)
  # no token: the text, for each arm
  expect_true(grepl('rep("Median", length(arm_of))', km_labels_code(legend_median_text = "Median"), fixed = TRUE))
  # without legend_median the text has no effect, whatever it is
  k <- tfl_template_expand("km_survival", list(by = "TRT01A", legend_median_text = "{nope}"))
  expect_false(any(grepl("legend_labels", unlist(lapply(k$data$steps, `[[`, "code")), fixed = TRUE)))
})

test_that("legend_median_text: a token that is not one of the four is an error naming the argument", {
  for (tpl in c("km_survival", "km_cumulative")) {
    expect_error(tfl_template_expand(tpl, list(by = "TRT01A", legend_median = TRUE,
                                               legend_median_text = "{arm}: {mean} ({lcl}, {ucl})")),
                 "`legend_median_text`: {mean} is not one of {arm}, {median}, {lcl}, {ucl}", fixed = TRUE)
  }
  expect_error(tfl_template_expand("km_survival", list(by = "TRT01A", legend_median = TRUE,
                                                       legend_median_text = "{arm} {")),
               "`legend_median_text`", fixed = TRUE)
})

test_that("legend_median_text: the figure builds from the generated code, in both types", {
  km_skip()
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  df <- adtte[adtte$PARAMCD == "PFS" & adtte$USUBJID %in% saf, ]
  arms <- sort(unique(as.character(df$TRT01A)))
  for (tpl in c("km_survival", "km_cumulative")) {
    p <- tfl_tpl(df, tpl, by = TRT01A, time_unit = "days", risk_table = FALSE, legend_median = TRUE,
                 legend_median_text = "{arm}, Median (95% CI): {median} ({lcl}, {ucl})")
    lab <- p$scales$get_scales("colour")$labels
    expect_identical(sort(names(lab)), arms)
    expect_true(all(grepl("^.+, Median \\(95% CI\\): (NE|[0-9.]+) [(](NE|[0-9.]+), (NE|[0-9.]+)[)]$", lab)))
    q <- tfl_tpl(df, tpl, by = TRT01A, time_unit = "days", risk_table = FALSE, legend_median = TRUE,
                 legend_median_text = "{lcl} | {arm} | {arm} | \"{median}\" \\ 5%")
    lab <- q$scales$get_scales("colour")$labels
    expect_true(all(grepl("^(NE|[0-9.]+) [|] .+ [|] .+ [|] \"(NE|[0-9.]+)\" \\\\ 5%$", lab)))
    expect_identical(unname(sub("^[^|]*[|] ([^|]*) [|] ([^|]*) [|].*$", "\\1", lab)), names(lab))
  }
  # the default text: the labels it always made
  d <- tfl_tpl(df, "km_survival", by = TRT01A, time_unit = "days", risk_table = FALSE, legend_median = TRUE,
               legend_median_text = "{arm}: {median} ({lcl}, {ucl})")
  expect_true(all(grepl("^.+: (NE|[0-9.]+) [(](NE|[0-9.]+), (NE|[0-9.]+)[)]$", d$scales$get_scales("colour")$labels)))
})

test_that("km_survival takes the report's plot.x_by / x_max for its axis, its arguments first", {
  km_skip()
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  km <- function(args, plot) {
    o <- tfl_output("F-9", "figure",
                    data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                             list(step = "param", value = "PFS"))),
                    template = list(name = "km_survival", args = c(list(by = "TRT01A"), args)))
    o$plot <- plot
    x <- tfl_template_to_spec(o, s)
    unlist(lapply(x$data$steps, function(st) if (identical(st$step, "code")) st$code))
  }
  expect_true("x_breaks <- seq(0, 30, by = 6)" %in% km(list(), list(x_by = 6, x_max = 30)))
  # (no x_max: to the first break at or after the last time)
  expect_true("x_breaks <- seq(0, ceiling(max(fit$time) / 6 - 1e-8) * 6, by = 6)" %in% km(list(), list(x_by = 6)))
  expect_true("x_breaks <- seq(0, ceiling(max(fit$time) / 3 - 1e-8) * 3, by = 3)" %in%
                km(list(x_step = 3), list(x_by = 6)))
  expect_true("x_breaks <- pretty(c(0, max(fit$time)))" %in% km(list(), list(x_label = "Months")))
})

test_that("a report's own layer_code with a template: statements on p after the call; the design from its base layer", {
  km_skip()
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  own <- tfl_item("layer_code", list(label = "a note", code = paste(
    "txt <- paste('n =', nrow(df))",
    "p <- p + ggplot2::annotate('text', x = 1, y = 0.1, label = txt)", sep = "\n")))
  o <- tfl_output("F-9", "figure",
                  data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                           list(step = "param", value = "PFS"))),
                  template = list(name = "km_survival", args = list(by = "TRT01A")),
                  content = list(own))
  ro <- tfl_resolve_output(o, s)
  # the program's call: into p, the statements, then content
  a <- tfl_content_code(ro)
  i <- which(startsWith(a, "p <- tflspec::tfl_tpl("))
  expect_length(i, 1L)
  expect_true(any(a == "# a note"))
  expect_identical(a[[length(a)]], "content <- p")
  expect_false(inherits(tryCatch(parse(text = a), error = function(e) e), "error"))
  # the design form (what tfl_ai_run draws): the curves first, whatever the order
  b <- tflspec:::.content_fig_code(ro)
  expect_true(any(startsWith(b, "p <- ggsurvfit(")))
  expect_false(any(b == "p <- ggplot() +"))
  expect_false(inherits(tryCatch(parse(text = b), error = function(e) e), "error"))
})
