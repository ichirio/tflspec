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
  # (the same after the study's layers, as the report's program has it:
  # the card keeps its !unset for the call, the other params stay)
  u$content <- .merge_content(list(), u$content)
  expect_identical(as.character(tfl_content_code(u)), as.character(code))
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

test_that("two scale cards after the call are one scale, by argument; layers stack", {
  o <- sfc_km_cards("  - scale_y_continuous: {breaks: [0, 0.5, 1], limits: [0, 1]}",
                    "  - geom_hline: {yintercept: 0.5}",
                    "  - scale_y_continuous: {breaks: [0, 0.25, 0.5, 0.75, 1]}",
                    "  - geom_hline: {yintercept: 0.25}")
  code <- tfl_content_code(o)
  expect_identical(sum(grepl("scale_y_continuous(", code, fixed = TRUE)), 1L)
  expect_true(any(code == "  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1)) +"))
  expect_identical(sum(grepl("geom_hline(", code, fixed = TRUE)), 2L)
  expect_no_error(parse(text = code, keep.source = FALSE))
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
  # another figure content function's card
  expect_error(tfl_content_code(sfc_km_cards("  - forest_hr: {params: {size_by_n: true}}")),
               "a forest_hr card does not change it", fixed = TRUE)
})

test_that("a tfl_image card says the image, not a `+` term", {
  # (two cards merged by name, the later winning: the file and the width)
  code <- tfl_content_code(sfc_km_cards("  - tfl_image: {width_in: 6, file: a.png}",
                                        "  - tfl_image: {file: f.png}"))
  expect_false(any(grepl("tfl_image(", code, fixed = TRUE)))
  expect_true(any(startsWith(code, "ggsave(\"f.png\", content, width = 6,")))
  expect_false(any(grepl("a.png", code, fixed = TRUE)))
})
