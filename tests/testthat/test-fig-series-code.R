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

# the HR ARD of sfc_adtte(): a Cox model a condition row (ALL, SEX F, SEX M)
sfc_hr_ard <- function() {
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  skip_if_not_installed("broom.helpers")
  d <- sfc_adtte()
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
  hr
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
  expect_identical(code[-1L], c("content <- tflspec::forest_hr(", "  ard = ard,", "  params = list(",
                                "    comparison = \"Drug X\"", "  )", ")"))
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

# ---- the report's own cards after the template's call (canon 1.4, C1) --------------

sfc_km_lines <- c("id: F-1", "kind: figure", "label: Kaplan-Meier",
                  "template:", "  name: tflspec_v1/km_simple",
                  "  analysis:", "    by: TRT01A", "  data:", "    time_unit: months")

sfc_km_cards <- function(...) {
  tfl_template_resolve(sfc_report(c(sfc_km_lines, "content:", ...)), study = sfc_study)
}

test_that("a km_curves card on a km_simple report changes only the params it writes", {
  plain <- tfl_content_code(tfl_template_resolve(sfc_km(), study = sfc_study))
  code <- tfl_content_code(sfc_km_cards("  - km_curves:", "      params: {legend: inside}"))
  expect_true(any(code == "    legend = \"inside\""))
  expect_false(any(grepl("legend = \"bottom\"", code, fixed = TRUE)))
  # the template's other values, in their places: only the legend's line differs
  expect_identical(length(code), length(plain))
  expect_identical(code[code != plain], "    legend = \"inside\"")
  expect_no_error(parse(text = code, keep.source = FALSE))
  # !unset takes a param away: the function's own default
  u <- sfc_km_cards("  - km_curves:", "      params: {legend: !unset}")
  code <- tfl_content_code(u)
  expect_false(any(grepl("legend =", code, fixed = TRUE)))
  expect_true(any(code == "    y_label = \"Survival Probability\""))
  expect_no_error(parse(text = code, keep.source = FALSE))
})

test_that("a km_curves card's !unset after the study's layers (a pattern's card under it): the param left out", {
  d <- fcards_study()
  lines <- c("id: F-31", "kind: figure", "label: Kaplan-Meier",
             "template:", "  name: tflspec_v1/km_simple",
             "  analysis:", "    by: TRT01P", "  data:", "    time_unit: months",
             "content:", "  - km_curves:", "      params: {legend: !unset}")
  ro <- fcards_resolve(d, lines)
  # (the layers' merge was not the empty one: the pattern's card is there)
  expect_true("labs" %in% vapply(ro$content, function(it) .schema_fn_name(it$fn), ""))
  code <- tfl_content_code(ro)
  # the card kept its !unset for the call: legend left out, the other params stay
  expect_false(any(grepl("legend =", code, fixed = TRUE)))
  expect_false(any(grepl("tfl_unset", code, fixed = TRUE)))
  expect_true(any(code == "    y_label = \"Survival Probability\""))
  expect_true(any(code == "    time_unit = \"months\","))
  expect_true(any(grepl("labs(caption = \"Source: ADTTE\")", code, fixed = TRUE)))
  expect_no_error(parse(text = code, keep.source = FALSE))
})

test_that("two km_curves cards: the later one wins, by name", {
  code <- tfl_content_code(sfc_km_cards(
    "  - km_curves:", "      params: {legend: none, y_label: Overall Survival}",
    "  - km_curves:", "      params: {legend: inside}"))
  expect_true(any(code == "    legend = \"inside\""))
  expect_true(any(code == "    y_label = \"Overall Survival\","))
  expect_true(any(code == "    time_unit = \"months\","))
  expect_identical(sum(grepl("legend =", code, fixed = TRUE)), 1L)
})

test_that("a card the template has not (labs, annotate) is added after the call, in order", {
  o <- sfc_km_cards("  - labs: {y: Overall Survival}",
                    "  - annotate: {geom: text, x: 100, y: 0.9, label: HR 0.70}")
  code <- tfl_content_code(o)
  at <- function(pat) which(grepl(pat, code, fixed = TRUE))
  expect_identical(code[at("labs(y = \"Overall Survival\")") - 1L], ") +")
  expect_true(any(code == "  labs(y = \"Overall Survival\") +"))
  expect_true(any(code == "  annotate(geom = \"text\", x = 100, y = 0.9, label = \"HR 0.70\")"))
  expect_true(at("content <- tflspec::km_curves(") < at("labs(y ="))
  expect_true(at("labs(y =") < at("annotate("))
  expect_no_error(parse(text = code, keep.source = FALSE))
  # (the same writer for the code SPEC the template report converts to:
  # `figure: {name, args}`)
  b <- o
  b$figure <- .fig_content_key(o)
  expect_identical(as.character(tfl_content_code(b)), as.character(code))
})

test_that("two scale cards after the call (a scale km_curves does not draw) are one scale; layers stack", {
  o <- sfc_km_cards("  - scale_fill_manual: {values: [red, blue], name: Arm}",
                    "  - geom_hline: {yintercept: 0.5}",
                    "  - scale_fill_manual: {values: [grey40, black]}",
                    "  - geom_hline: {yintercept: 0.25}")
  code <- tfl_content_code(o)
  expect_identical(sum(grepl("scale_fill_manual(", code, fixed = TRUE)), 1L)
  expect_true(any(code == "  scale_fill_manual(values = c(\"grey40\", \"black\"), name = \"Arm\") +"))
  expect_identical(sum(grepl("geom_hline(", code, fixed = TRUE)), 2L)
  expect_no_error(parse(text = code, keep.source = FALSE))
})

test_that("a scale km_curves draws: its card is the call's scale_y, by argument (F3)", {
  o <- sfc_km_cards("  - scale_y_continuous: {breaks: [0, 0.5, 1]}",
                    "  - scale_y_continuous: {expand: [0, 0]}",
                    "  - scale_x_continuous:", "      breaks: !r seq(0, 24, by = 6)")
  code <- tfl_content_code(o)
  # no `+ scale_y_continuous()` after the call: the function's own scale, its
  # arguments over the function's (limits c(0, 1) kept)
  expect_false(any(grepl("scale_y_continuous(", code, fixed = TRUE)))
  expect_true(any(code == "    scale_y = list(breaks = c(0, 0.5, 1), expand = c(0, 0)),"))
  expect_true(any(code == "    scale_x = list(breaks = seq(0, 24, by = 6))"))
  expect_no_error(parse(text = code, keep.source = FALSE))
  # !unset takes one of the card's arguments away
  u <- tfl_content_code(sfc_km_cards("  - scale_y_continuous: {breaks: [0, 0.5, 1]}",
                                     "  - scale_y_continuous: {breaks: !unset, expand: [0, 0]}"))
  expect_true(any(u == "    scale_y = list(expand = c(0, 0))"))
  # (a scale of other aesthetics is not the function's: after the call)
  a <- tfl_content_code(sfc_km_cards("  - scale_y_continuous: {aesthetics: [y], breaks: [0, 1]}"))
  expect_true(any(grepl("^  scale_y_continuous[(]aesthetics = ", a)))
})

test_that("km_curves(): params$scale_y over its own scale, by argument -- the limits stay (F3)", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  d <- sfc_adtte()
  fit <- ggsurvfit::survfit2(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = d)
  p0 <- km_curves(fit)
  p <- km_curves(fit, params = list(scale_y = list(breaks = c(0, 0.5, 1))))
  y0 <- p0$scales$get_scales("y")
  y <- p$scales$get_scales("y")
  expect_equal(y$limits, c(0, 1))
  expect_equal(y$breaks, c(0, 0.5, 1))
  expect_equal(y$expand, y0$expand)
  expect_error(km_curves(fit, params = list(scale_y = c(0, 1))), "is a named list of the scale's arguments")
})

test_that("forest_hr(): params$scale_y over the rows' scale -- the rows' labels stay (F3)", {
  skip_if_not_installed("ggplot2", "3.5.2")
  hr <- sfc_hr_ard()
  p0 <- forest_hr(hr)
  p <- forest_hr(hr, list(scale_y = list(expand = ggplot2::expansion(add = 0.5))))
  y0 <- p0$scales$get_scales("y")
  y <- p$scales$get_scales("y")
  expect_identical(y$labels, y0$labels)
  expect_identical(y$breaks, y0$breaks)
  expect_equal(y$expand, ggplot2::expansion(add = 0.5))
  # breaks without their labels: the labels are the rows'
  expect_error(forest_hr(hr, list(scale_y = list(breaks = 1:3))), "without `labels`")
  # a forest_hr report's scale_y_continuous card: the call's scale_y
  rep <- sfc_forest()
  rep$content <- list(list(scale_y_continuous = list(expand = list(0, 0))))
  code <- tfl_content_code(tfl_template_resolve(rep, study = sfc_study))
  expect_false(any(grepl("scale_y_continuous(", code, fixed = TRUE)))
  expect_true(any(code == "    scale_y = list(expand = c(0, 0))"))
})

test_that("a forest_hr card's subgroup_labels merge by name into the template's; !unset takes one name away (F6)", {
  # (the code SPEC's key, `figure: {name, args}`, with the template's labels)
  o <- tfl_template_resolve(sfc_forest(), study = sfc_study)
  o$figure <- list(name = "forest_hr",
                   args = c(.fig_content_key(o)$args,
                            list(subgroup_labels = list(SEX = "Sex", AGEGR1 = "Age group"))))
  o$content <- list(tfl_item("forest_hr", list(params = list(subgroup_labels = list(AGEGR1 = "Age (years)")))))
  code <- tfl_content_code(o)
  expect_true(any(grepl("subgroup_labels = c(SEX = \"Sex\", AGEGR1 = \"Age (years)\")", code, fixed = TRUE)))
  o$content <- list(tfl_item("forest_hr", list(params = list(subgroup_labels = list(SEX = tfl_unset())))))
  code <- tfl_content_code(o)
  expect_true(any(grepl("subgroup_labels = c(AGEGR1 = \"Age group\")", code, fixed = TRUE)))
})

test_that(".fig_params_over(): a param's named list merges by name, one level in (F6)", {
  base <- list(comparison = "Drug X", subgroup_labels = list(SEX = "Sex"))
  x <- .fig_params_over(base, list(subgroup_labels = list(AGEGR1 = "Age group")))
  expect_identical(x$subgroup_labels, list(SEX = "Sex", AGEGR1 = "Age group"))
  expect_identical(x$comparison, "Drug X")
  y <- .fig_params_over(x, list(subgroup_labels = list(SEX = tfl_unset())))
  expect_identical(y$subgroup_labels, list(AGEGR1 = "Age group"))
  # a value that is not a named list is replaced whole
  expect_identical(.fig_params_over(base, list(comparison = "Drug Y"))$comparison, "Drug Y")
  # `params: !unset`: none of the template's
  expect_identical(.fig_params_over(base, tfl_unset()), list())
  expect_error(.fig_params_over(base, list(1, 2), "F-1", "forest_hr"), "params is a map")
})

test_that("the cards run: only what they write changes in the plot", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("cards")
  d <- sfc_adtte()
  bign <- cards::ard_tabulate(d, variables = "TRT01A")
  bign$analysis_id <- "BIGN"
  data <- list(adtte_sub = d, ard = bign)
  p0 <- tfl_content_run(tfl_template_resolve(sfc_km(), study = sfc_study), data)
  p <- tfl_content_run(sfc_km_cards("  - labs: {y: Overall Survival}",
                                    "  - km_curves:", "      params: {legend: inside}",
                                    "  - annotate: {geom: text, x: 100, y: 0.9, label: HR 0.70}"), data)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(ggplot2::get_labs(p)$y, "Overall Survival")
  expect_identical(ggplot2::get_labs(p)$x, ggplot2::get_labs(p0)$x)
  expect_identical(length(p$layers), length(p0$layers) + 1L)
  expect_true(inherits(p$layers[[length(p$layers)]]$geom, "GeomText"))
})

test_that("a forest_hr card merges into the call; the comparison the template gave stays", {
  rep <- sfc_forest()
  rep$content <- list(list(forest_hr = list(params = list(favours = list("Favours Drug X", "Favours Control")))),
                      list(labs = list(x = "Hazard ratio")))
  code <- tfl_content_code(tfl_template_resolve(rep, study = sfc_study))
  expect_identical(code[-1L], c("content <- tflspec::forest_hr(", "  ard = ard,", "  params = list(",
                                "    comparison = \"Drug X\",",
                                "    favours = c(\"Favours Drug X\", \"Favours Control\")", "  )",
                                ") +", "  labs(x = \"Hazard ratio\")"))
})

test_that("a card that does not fit the call stops, naming it", {
  # a name the function does not take (its params are in `params`)
  expect_error(tfl_content_code(sfc_km_cards("  - km_curves: {legend: inside}")),
               "km_curves() takes km_fit, ard, params", fixed = TRUE)
  # a params name it does not take
  expect_error(tfl_content_code(sfc_km_cards("  - km_curves:", "      params: {colour: red}")),
               "km_curves() has no params colour", fixed = TRUE)
  # another figure content function's card, a card that makes a plot: a plot of its own
  expect_error(tfl_content_code(sfc_km_cards("  - forest_hr: {params: {size_by_n: true}}")),
               "a forest_hr card makes a plot of its own", fixed = TRUE)
  expect_error(tfl_content_code(sfc_km_cards("  - ggplot: {data: adtte_sub}")),
               "a ggplot card makes a plot of its own", fixed = TRUE)
  expect_error(tfl_content_code(sfc_km_cards("  - ggsurvfit: {linewidth: 1}")),
               "a ggsurvfit card makes a plot of its own", fixed = TRUE)
  # the call's input, which the template wires: not a card's (F8)
  expect_error(tfl_content_code(sfc_km_cards("  - km_curves: {ard: !unset}")),
               "the km_curves card writes ard, the call's input", fixed = TRUE)
  expect_error(tfl_content_code(sfc_km_cards("  - km_curves: {km_fit: !r my_fit}")),
               "the km_curves card writes km_fit, the call's input", fixed = TRUE)
  rep <- sfc_forest()
  rep$content <- list(list(forest_hr = list(ard = "ard2")))
  expect_error(tfl_content_code(tfl_template_resolve(rep, study = sfc_study)),
               "the forest_hr card writes ard, the call's input", fixed = TRUE)
  # R code, a named plot: not cards after the call
  o <- tfl_template_resolve(sfc_km(), study = sfc_study)
  o$content <- tfl_r("content <- ggplot2::ggplot()")
  expect_error(tfl_content_code(o), "not `content: !r`", fixed = TRUE)
  o$content <- list(tfl_plot("km", list(tfl_item("ggplot", list()))))
  expect_error(tfl_content_code(o), "not a named plot (`km`)", fixed = TRUE)
})

test_that("a tfl_image card says the image, not a `+` term", {
  # (two cards merged by name, the later winning: the file and the width)
  code <- tfl_content_code(sfc_km_cards("  - tfl_image: {width_in: 6, file: a.png}",
                                        "  - tfl_image: {file: f.png}"))
  expect_false(any(grepl("tfl_image(", code, fixed = TRUE)))
  expect_true(any(startsWith(code, "ggsave(\"f.png\", content, width = 6,")))
  expect_false(any(grepl("a.png", code, fixed = TRUE)))
})
