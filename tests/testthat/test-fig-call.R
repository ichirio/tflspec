# The generic `call` piece / `plot.add` (issue #34): the writer rules, the
# `!r` round trip, validation, output order and the design doc's examples.

fig_call_run <- function(design, adam = tfl_example_adam()) {
  code <- tfl_fig_design_code(design, "t")
  e <- new.env()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  dir <- tempfile("fig")
  dir.create(dir)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressMessages(eval(parse(text = code), envir = e))
  list(code = code, fig = e$fig, png = file.exists(e$fig_path))
}

# ---- writer rules -----------------------------------------------------------

test_that("numbers, logicals, NULL and Inf write as R literals", {
  expect_equal(tflspec:::.fig_arg_code(3), "3")
  expect_equal(tflspec:::.fig_arg_code(0.5), "0.5")
  expect_equal(tflspec:::.fig_arg_code(-Inf), "-Inf")
  expect_equal(tflspec:::.fig_arg_code(Inf), "Inf")
  expect_equal(tflspec:::.fig_arg_code(TRUE), "TRUE")
  expect_equal(tflspec:::.fig_arg_code(FALSE), "FALSE")
  expect_equal(tflspec:::.fig_arg_code(NULL), "NULL")
})

test_that("a string is quoted (escaped) unless it is an expression", {
  expect_equal(tflspec:::.fig_arg_code("text"), '"text"')
  expect_equal(tflspec:::.fig_arg_code("AVAL"), '"AVAL"')
  expect_equal(tflspec:::.fig_arg_code("AVAL", expr = TRUE), "AVAL")
  expect_equal(tflspec:::.fig_arg_code("a \"quote\""), '"a \\"quote\\""')
})

test_that("!r is raw R, verbatim, and parses", {
  r <- tfl_fig_r("vars(PARAM)")
  expect_s3_class(r, "tfl_fig_r")
  expect_equal(tflspec:::.fig_arg_code(r), "vars(PARAM)")
  expect_equal(tflspec:::.fig_arg_code(tfl_fig_r("NA")), "NA")
})

test_that("a sequence writes as c(), even of length 1", {
  expect_equal(tflspec:::.fig_arg_code(list("a")), 'c("a")')
  expect_equal(tflspec:::.fig_arg_code(list("a", "b")), 'c("a", "b")')
  expect_equal(tflspec:::.fig_arg_code(list(0.25, 0.5, 1, 2, 4)), "c(0.25, 0.5, 1, 2, 4)")
})

test_that("a map without fn writes as a named vector, quoting non-syntactic names", {
  expect_equal(tflspec:::.fig_arg_code(list(a = 1, b = "x")), 'c(a = 1, b = "x")')
  expect_equal(tflspec:::.fig_arg_code(list(`Drug A` = "red", B = "blue")),
               'c("Drug A" = "red", B = "blue")')
})

test_that("a map with fn writes as a nested call, package-qualified when asked", {
  expect_equal(tflspec:::.fig_build_call(list(fn = "element_blank")), "element_blank()")
  expect_equal(tflspec:::.fig_build_call(list(fn = "element_text", args = list(face = "bold"))),
               'element_text(face = "bold")')
  code <- tflspec:::.fig_build_call(list(fn = "f", package = "somepkg", pos = list(1, 2)))
  expect_equal(code, "somepkg::f(1, 2)")
})

test_that("strings inside aes/vars are expressions", {
  code <- tflspec:::.fig_build_call(list(fn = "aes", args = list(x = "AVISIT", label = "n")))
  expect_equal(code, "aes(x = AVISIT, label = n)")
  code2 <- tflspec:::.fig_build_call(list(fn = "vars", pos = list("PARAM")))
  expect_equal(code2, "vars(PARAM)")
})

test_that("pos comes before args, in order; data and aes come first", {
  spec <- list(fn = "f", data = "sm", aes = list(x = "AVISIT"), pos = list(1, 2), args = list(z = 3))
  expect_equal(tflspec:::.fig_build_call(spec), "f(data = sm, aes(x = AVISIT), 1, 2, z = 3)")
})

test_that("a call outside ggplot2 is package-qualified and noted as needed", {
  res <- tflspec:::.fig_call_code(list(fn = "geom_point", package = "notapkg"))
  expect_equal(res$call, "notapkg::geom_point()")
  expect_equal(res$pkgs, "notapkg")
})

test_that("ggsurvfit/patchwork calls are bare and added to library()", {
  res <- tflspec:::.fig_call_code(list(fn = "add_quantile", package = "ggsurvfit",
                                       args = list(y_value = 0.5, linetype = "dotted")))
  expect_equal(res$call, 'add_quantile(y_value = 0.5, linetype = "dotted")')
  expect_equal(res$libs, "ggsurvfit")
})

test_that("a bad call does not parse and errors at generation time", {
  expect_error(tflspec:::.fig_call_code(list(fn = "f", args = list(bad = tfl_fig_r("f(")))))
})

# ---- YAML round trip ---------------------------------------------------------

test_that("!r round-trips through write/read, and generates the same code", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADLB")),
    plot = list(add = list(list(fn = "facet_grid", args = list(
      rows = tfl_fig_r("vars(PARAM)"), labeller = tfl_fig_r("label_wrap_gen(width = 25)"))))),
    layers = list(list(layer = "call", fn = "add_risktable", package = "ggsurvfit",
                       args = list(risktable_stats = list("n.risk", "n.event"),
                                  theme = tfl_fig_r("theme_risktable_default()")))))
  f <- tempfile(fileext = ".yml")
  tfl_write_fig_design(d, f)
  back <- tfl_read_fig_design(f)
  expect_s3_class(back$plot$add[[1]]$args$rows, "tfl_fig_r")
  expect_equal(as.character(back$plot$add[[1]]$args$rows), "vars(PARAM)")
  expect_s3_class(back$layers[[1]]$args$theme, "tfl_fig_r")
  # a plain (non-!r) sequence read back may be an atomic vector rather than a
  # list (a YAML round trip, not our writer): the *generated code* is what
  # must be identical.
  expect_equal(paste(tfl_fig_design_code(d), collapse = "\n"),
               paste(tfl_fig_design_code(back), collapse = "\n"))
})

# ---- validation ---------------------------------------------------------------

test_that("an unknown argument to a function without ... errors, with a suggestion", {
  p <- tflspec:::.fig_check_call(list(fn = "coord_cartesian", args = list(xlimm = list(0, 1))), "p")
  expect_true(any(grepl("not an argument", p$problem)))
  expect_true(any(grepl("xlim", p$problem)))
})

test_that("an unknown geom/stat parameter warns, not errors, and is still generated", {
  p <- tflspec:::.fig_check_call(list(fn = "geom_point", args = list(sizee = 2)), "p")
  expect_true(any(grepl("not a parameter of geom_point", p$problem)))
  # still generates: geoms take ... freely at the R level
  expect_silent(tflspec:::.fig_call_code(list(fn = "geom_point", args = list(sizee = 2))))
})

test_that("an unknown theme() element errors, checked against get_element_tree()", {
  p <- tflspec:::.fig_check_call(list(fn = "theme", args = list(not_a_real_element = 1)), "p")
  expect_true(any(grepl("not a theme\\(\\) element", p$problem)))
})

test_that("an uninstalled package warns and continues (pkg::fn code still generated)", {
  p <- tflspec:::.fig_check_call(list(fn = "foo", package = "notapkg999"), "p")
  expect_true(any(grepl("not installed", p$problem)))
  res <- tflspec:::.fig_call_code(list(fn = "foo", package = "notapkg999"))
  expect_equal(res$call, "notapkg999::foo()")
})

test_that("a plot-wide function in layers warns to use plot.add instead", {
  d <- tfl_fig_design(
    layers = list(list(layer = "call", fn = "theme", args = list(legend.position = "none"))))
  p <- tfl_check_fig_design(d)
  expect_true(any(grepl("use plot.add", p$problem)))
})

test_that("plot.add overriding facet_by/colour_by/x_log warns", {
  d <- tfl_fig_design(
    plot = list(facet_by = "TRT01A", colour_by = "TRT01A", x_log = TRUE,
               add = list(list(fn = "facet_wrap", args = list(facets = tfl_fig_r("vars(PARAM)"))),
                          list(fn = "scale_colour_manual", args = list(values = list("red"))),
                          list(fn = "scale_x_log10"))))
  p <- tfl_check_fig_design(d)
  expect_true(any(grepl("overrides plot\\$facet_by", p$problem)))
  expect_true(any(grepl("overrides plot\\$colour_by", p$problem)))
  expect_true(any(grepl("overrides plot\\$x_log", p$problem)))
})

test_that("a call's aes variable is checked against the data it names", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE")),
    layers = list(list(layer = "call", fn = "geom_text", aes = list(x = "NOPE"))))
  p <- tfl_check_fig_design(d, tfl_example_adam())
  expect_true(any(grepl("no variable NOPE", p$problem)))
})

# ---- output order -------------------------------------------------------------

test_that("a call layer's code sits at its position among the other layers", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL")),
    layers = list(list(layer = "hline", yintercept = 0),
                  list(layer = "call", fn = "geom_point", aes = list(x = "TRTDURD", y = "AGE")),
                  list(layer = "hline", yintercept = 1)))
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  i_geom <- regexpr("geom_point(", code, fixed = TRUE)
  i_h0 <- regexpr("yintercept = 0", code)
  i_h1 <- regexpr("yintercept = 1", code)
  expect_true(i_h0 < i_geom && i_geom < i_h1)
})

test_that("plot.add is written after the figure's settings and before panels", {
  d <- tfl_fig_template("km_risk_table", param = "OS", group = "TRT01P")
  d$plot$add <- list(list(fn = "labs", args = list(caption = "the add")))
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  # one chain: the settings, then the plot.add call, then the panel's chain
  i_settings <- regexpr("labs(x = ", code, fixed = TRUE)
  i_add <- regexpr("labs(caption = \"the add\")", code, fixed = TRUE)
  i_panel <- regexpr("p_risk <- ", code, fixed = TRUE)
  expect_true(i_settings < i_add && i_add < i_panel)
})

test_that("base = TRUE on the first layer replaces p <- ggplot()", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE")),
    stats = list(list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR")),
    layers = list(list(layer = "call", fn = "ggsurvfit", package = "ggsurvfit", base = TRUE,
                       pos = list(tfl_fig_r("fit")))))
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "fig <- ggsurvfit(fit) +", fixed = TRUE)
  expect_false(grepl("p <- ggplot()", code, fixed = TRUE))
})

# ---- design doc examples (non-patchwork) ---------------------------------------

test_that("design doc example 1 (KM + add_* as calls) renders", {
  skip_if_not_installed("ggsurvfit")
  adam <- tfl_example_adam()
  prm <- unique(adam$ADTTE$PARAMCD)[1]
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = prm),
               list(step = "flag", variable = "FASFL"),
               list(step = "time_unit", variable = "AVAL", unit = "months")),
    stats = list(list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "TRT01P")),
    plot = list(x_label = "Time (Months)", y_label = "Survival Probability", colour_by = "TRT01P",
               legend = "inside", x_min = 0, x_max = 24, x_by = 3, y_min = 0, y_max = 1,
               add = list(list(fn = "theme", args = list(
                 legend.position.inside = list(0.85, 0.85),
                 axis.title = list(fn = "element_text", args = list(face = "bold")))))),
    layers = list(list(layer = "km_curve"), list(layer = "censor_mark"),
                 list(layer = "call", fn = "add_quantile", package = "ggsurvfit",
                      args = list(y_value = 0.5, linetype = "dotted")),
                 list(layer = "call", fn = "add_risktable", package = "ggsurvfit",
                      args = list(risktable_stats = list("n.risk"), size = 3,
                                 theme = tfl_fig_r("theme_risktable_default(axis.text.y.size = 9)")))))
  expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L)
  r <- fig_call_run(d, adam)
  expect_true(r$png)
})

test_that("design doc example 2 (facet_grid + theme via plot.add, geom_text via call) renders", {
  adam <- tfl_example_adam()
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADLB"),
               list(step = "join", dataset = "ADSL", vars = "TRT01A, SAFFL"),
               list(step = "flag", variable = "SAFFL"),
               list(step = "levels", variable = "AVISIT", order_by = "AVISITN")),
    stats = list(list(step = "summary", name = "sm", value = "AVAL",
                      by = "PARAM, TRT01A, AVISITN, AVISIT", interval = "se")),
    plot = list(x_label = "Visit", y_label = "Mean (+/- SE)", colour_by = "TRT01A", legend = "bottom",
               add = list(
                 list(fn = "facet_grid", args = list(rows = tfl_fig_r("vars(PARAM)"), scales = "free_y")),
                 list(fn = "theme", args = list(
                   strip.background = list(fn = "element_rect", args = list(fill = "grey90")),
                   panel.grid.minor = list(fn = "element_blank"))))),
    layers = list(
      list(layer = "line", data = "sm", x = "AVISIT", y = "mean", colour = "TRT01A", dodge = TRUE),
      list(layer = "point", data = "sm", x = "AVISIT", y = "mean", colour = "TRT01A", dodge = TRUE),
      list(layer = "errorbar", data = "sm", x = "AVISIT", colour = "TRT01A", dodge = TRUE),
      list(layer = "call", fn = "geom_text", data = "sm",
           aes = list(x = "AVISIT", y = "hi", label = "n", colour = "TRT01A"),
           args = list(vjust = -0.6, size = 2.5, show.legend = FALSE))))
  expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L)
  r <- fig_call_run(d, adam)
  expect_true(r$png)
})

test_that("design doc example 3 (forest plot via calls: coord_flip, scale_y_log10) renders", {
  est <- data.frame(subgroup = c("Overall", "Age < 65", "Age >= 65"),
                    hr = c(0.7, 0.6, 0.9), lcl = c(0.5, 0.4, 0.6), ucl = c(0.98, 0.9, 1.35))
  d <- tfl_fig_design(
    data = list(list(step = "data_code", code = "")),
    stats = list(list(step = "stats_code", code = "")),
    plot = list(y_label = "Hazard ratio (95% CI)", x_label = "", legend = "none",
               add = list(
                 list(fn = "scale_y_log10", args = list(breaks = list(0.25, 0.5, 1, 2, 4))),
                 list(fn = "scale_x_discrete", args = list(limits = tfl_fig_r("rev"))),
                 list(fn = "coord_flip"),
                 list(fn = "labs", args = list(caption = "HR < 1 favours treatment")))),
    layers = list(list(layer = "hline", yintercept = 1, linetype = "dashed", colour = "grey50"),
                 list(layer = "pointrange", data = "est", x = "subgroup", y = "hr", ymin = "lcl", ymax = "ucl")))
  # est is normally made by stats_code; injected directly into the eval env here
  code <- tfl_fig_design_code(d, "t")
  e <- new.env()
  e$est <- est
  dir <- tempfile("fig"); dir.create(dir); owd <- setwd(dir); on.exit(setwd(owd), add = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  suppressMessages(eval(parse(text = code), envir = e))
  expect_true(file.exists(e$fig_path))
})
