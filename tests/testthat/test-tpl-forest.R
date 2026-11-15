fr_skip <- function() {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("survival")
  skip_if_not_installed("cardx")
}

test_that("forest_hr: the report's ARD (its Cox analyses) and the design that reads it", {
  x <- tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF"))
  ad <- x$ard$analysis_data[[1L]]
  expect_identical(ad[c("id", "from", "population")],
                   list(id = "adtte_saf_pfs", from = "ADTTE", population = "SAF"))
  expect_identical(as.character(ad$where), "PARAMCD == \"PFS\"")
  expect_identical(ad$add, list("SEX", "AGEGR1"))
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("HR", "HR_SEX", "HR_AGEGR1"))
  expect_identical(vapply(x$data$steps, `[[`, "", "step"), c("ard_stats", "code"))
  expect_identical(x$call_steps, 2L)
  expect_identical(vapply(x$content, `[[`, "", "fn"),
                   c("vline", "errorbar_h", "point", "text_column"))
  expect_identical(tfl_template_params("forest_hr")$subgroups$type, "vars")
  y <- tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF", favours = c("A", "B"),
                                             x_breaks = c(0.5, 1, 2), size_by_n = TRUE))
  expect_identical(vapply(y$content, `[[`, "", "fn"), c("vline", "errorbar_h", "geom", "text_column"))
  expect_error(tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF", favours = "A")),
               "two labels")
  # bold headings: the axis's labels are y_labels (made with the rows); nothing changes without it
  bold <- tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF", bold_headings = TRUE))
  code <- function(x) paste(vapply(x$data$steps, function(s) as.character(s$code %||% ""), ""), collapse = "
")
  expect_match(code(bold), "y_labels <- as.expression(Map(", fixed = TRUE)
  expect_identical(as.character(bold$plot$add[[1L]]$args$labels), "y_labels")
  plain <- tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF"))
  expect_false(grepl("y_labels", code(plain), fixed = TRUE))
  expect_identical(as.character(plain$plot$add[[1L]]$args$labels), "est$label")
})

test_that("in a study: the ARD built; A (the call, the layout) = B (the design) = the template SPEC", {
  skip_on_cran()
  fr_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "FR-1")
  o <- tfl_output("F-9-1", "figure", common = list(population = "SAF"), label = "Forest",
                  extends = "defaults",
                  template = list(name = "forest_hr",
                                  args = list(param = "PFS", population = "SAF",
                                              subgroups = c("SEX", "AGEGR1"))),
                  content = list(tfl_item("geom_vline", list(xintercept = 0.5, linetype = "dotted")),
                                 tfl_item("labs", list(x = "Hazard ratio (95% CI)"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-9-1.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "F-9-1", dir = d, save = FALSE))
  expect_true(all(c("HR", "HR_SEX", "HR_AGEGR1") %in% ard$analysis_id))
  x <- tplfig_check(o, d, c(data, list(ard = ard)))
  txt <- paste(x$a, collapse = "\n")
  expect_match(txt, "fig <- tflspec::tfl_tpl(ard, \"forest_hr\", subgroups = c(SEX, AGEGR1)) +", fixed = TRUE)
  expect_match(txt, "content <- tpl_forest_layout(fig)", fixed = TRUE)
  # the forest alone, then its text column at its right
  p <- tpl_forest_hr(ard, group = TRT01A, subgroups = c(SEX, AGEGR1))
  expect_s3_class(p, "ggplot")
  expect_s3_class(tpl_forest_layout(p), "patchwork")
  expect_error(tpl_forest_layout(ggplot2::ggplot()), "a forest of tpl_forest_hr")
  # the shells' look (#579): favours labels, ticks, points by n, bold headings
  # (no warning from ggplot2: the bold is the axis's labels, not a vector in element_text())
  expect_no_warning(q <- tpl_forest_hr(ard, group = TRT01A, subgroups = c(SEX, AGEGR1),
                                       favours = c("Drug A", "Drug B"), x_breaks = c(0.5, 1, 2),
                                       size_by_n = TRUE, bold_headings = TRUE))
  expect_no_warning(b <- ggplot2::ggplot_build(q))
  pts <- ggplot2::layer_data(q, 3L)
  expect_true(length(unique(stats::na.omit(pts$size))) > 1L)
  expect_true(all(stats::na.omit(b$layout$panel_params[[1L]]$x$get_labels()) %in% c("0.5", "1", "2")))
  expect_match(q$labels$x %||% b$plot$labels$x, "<- Favours Drug A", fixed = TRUE)
  expect_null(q$theme$axis.text.y$face)
  # bold exactly at the heading rows, in the order the axis lists its labels:
  # a heading is bold("<text>"), a level's row its text as it is
  bold_text <- function(lab) vapply(lab, function(l) if (is.call(l)) as.character(l[[2L]]) else NA_character_, "")
  ylab <- b$layout$panel_params[[1L]]$y$get_labels()
  expect_identical(unname(bold_text(ylab)[!is.na(bold_text(ylab))]), c("Sex", "Age group"))
  expect_identical(unname(vapply(ylab, is.call, NA)), c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
  # the levels' rows (indented, plain text) and the subgroup levels' text as it is: <65, >=65
  expect_true(all(c("    F: Control", "    <65: Control", "    >=65: Control") %in% unlist(ylab[!vapply(ylab, is.call, NA)])))
  # the headings by their label (the common ADaM variables'), not the variable's name
  expect_false(any(c("SEX", "AGEGR1") %in% bold_text(ylab)))
  # a label given for the variable: its heading, bold on it (any text, quotes and symbols, as it is)
  g <- tpl_forest_hr(ard, group = TRT01A, subgroups = c(SEX, AGEGR1), bold_headings = TRUE,
                     subgroup_labels = c(SEX = "Gender", AGEGR1 = "Age <65 / >=65 \"x\""))
  gb <- ggplot2::ggplot_build(g)
  glab <- gb$layout$panel_params[[1L]]$y$get_labels()
  expect_identical(unname(bold_text(glab)[vapply(glab, is.call, NA)]), c("Gender", "Age <65 / >=65 \"x\""))
  expect_identical(unname(vapply(glab, is.call, NA)), c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_s3_class(tpl_forest_layout(q), "patchwork")
})

test_that("ref: the reference arm is put first by a relevel; comparison only shows one arm", {
  x <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT", group = "TRT01P",
                                             ref = "Drug X"))
  expect_match(unclass(x$ard$analysis_data[[1L]]$derive),
               "TRT01P = factor(TRT01P, levels = c(\"Drug X\", setdiff(levels(factor(TRT01P)), \"Drug X\")))",
               fixed = TRUE)
  # comparison does not reorder the arms (the data step is the blank arm made missing only)
  y <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT", group = "TRT01P",
                                             comparison = "Drug X"))
  expect_false(grepl("factor(", unclass(y$ard$analysis_data[[1L]]$derive), fixed = TRUE))
  expect_error(tfl_template_expand("forest_hr", list(param = "OS", population = "ITT",
                                                     ref = "Drug X", comparison = "Drug X")),
               "`comparison` is the reference arm")
  # (no ref: only the blank arm made missing)
  expect_identical(as.vector(unclass(tfl_template_expand("forest_hr", list(param = "OS", population = "ITT"))$ard$analysis_data[[1L]]$derive)),
                   "TRT01A = replace(TRT01A, !is.na(TRT01A) & as.character(TRT01A) == \"\", NA)")
  skip_on_cran()
  fr_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "FR-2")
  # (the bundled data's arms: Drug X first, TRT01PN 1)
  o <- tfl_output("F-9-2", "figure", common = list(population = "SAF"), label = "Forest", extends = "defaults",
                  template = list(name = "forest_hr", args = list(
                    param = "OS", population = "SAF", group = "TRT01P", subgroups = "SEX",
                    ref = "Control", comparison = "Drug X")))
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-9-2.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "F-9-2", dir = d, save = FALSE))
  hr <- ard[ard$analysis_id == "HR" & ard$stat_name == "conf.low", ]
  # the interval is Drug X's (against Control), not the reference's
  lv <- as.character(unlist(hr$variable_level))
  ci <- unlist(hr$stat)
  expect_false(is.na(ci[lv == "Drug X"][1L]))
  ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", "F-9-2.yml")), st)
  fig <- suppressMessages(suppressWarnings(tfl_content_run(ro, c(data, list(ard = ard)))))
  b <- suppressWarnings(ggplot2::ggplot_build(fig))
  expect_gt(sum(vapply(b$data, nrow, 1L)), 0L)
})

test_that("x_breaks as YAML writes it (decimals and whole numbers: a list) is several numbers", {
  a <- yaml::yaml.load("{param: OS, population: ITT, x_breaks: [0.25, 0.5, 1, 2, 4]}")
  expect_true(is.list(a$x_breaks))
  p <- tflspec:::.ai_check_template(list(name = "forest_hr", args = a), "")
  expect_identical(nrow(p[p$severity == "error", , drop = FALSE]), 0L)
  x <- tfl_template_expand("forest_hr", a)
  sc <- Filter(function(z) identical(z$fn, "scale_x_log10"), x$plot$add)
  expect_identical(as.numeric(sc[[1L]]$args$breaks), c(0.25, 0.5, 1, 2, 4))
  bad <- tflspec:::.ai_check_template(list(name = "forest_hr", args = list(param = "OS", population = "ITT",
                                                                           x_breaks = list(1, "a"))), "")
  expect_true(any(grepl("number", bad$problem)))
})

test_that("n_by_arm: each arm's events / subjects, a column each; the call's look kept (#579)", {
  x <- tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF",
                                             n_by_arm = c("Control", "Drug X")))
  st <- x$data$steps
  expect_match(st[[1L]]$stats, "n_event", fixed = TRUE)
  expect_match(st[[2L]]$code, "n_arm2 = sprintf(\"%s/%s\", first(n_event[TRT01A == \"Drug X\"])", fixed = TRUE)
  tc <- Filter(function(it) identical(it$fn, "text_column"), x$content)[[1L]]
  expect_identical(tc$args$columns, "Control = n_arm1 | Drug X = n_arm2 | Hazard ratio (95% CI) = txt")
  skip_on_cran()
  fr_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "FR-3")
  o <- tfl_output("F-9-3", "figure", common = list(population = "SAF"), label = "Forest", extends = "defaults",
                  template = list(name = "forest_hr", args = list(
                    param = "PFS", population = "SAF", subgroups = "SEX",
                    favours = c("Drug X", "Control"), n_by_arm = c("Control", "Drug X"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-9-3.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "F-9-3", dir = d, save = FALSE))
  # A (the call, the layout given the same arms) = B (the design) = the template SPEC
  r <- tplfig_check(o, d, c(data, list(ard = ard)))
  txt <- paste(r$a, collapse = "\n")
  expect_match(txt, "content <- tpl_forest_layout(fig, n_by_arm = c(\"Control\", \"Drug X\"))", fixed = TRUE)
  # the arms' events / subjects; none on a heading row
  p <- tpl_forest_hr(ard, group = TRT01A, subgroups = SEX, n_by_arm = c("Control", "Drug X"))
  est <- p$layers[[2L]]$data
  expect_true(all(grepl("^[0-9]+/[0-9]+$", est$n_arm1[!est$head])))
  expect_identical(unique(est$n_arm2[est$head]), "")
  expect_s3_class(tpl_forest_layout(p, n_by_arm = c("Control", "Drug X")), "patchwork")
  expect_error(tpl_forest_layout(tpl_forest_hr(ard, group = TRT01A, subgroups = SEX), n_by_arm = "Control"),
               "the arms tpl_forest_hr")
  # tfl_tpl() draws the look its arguments ask for (favours under the axis)
  q <- tfl_tpl(ard, "forest_hr", subgroups = SEX, favours = c("Drug X", "Control"))
  expect_match(q$labels$x %||% suppressWarnings(ggplot2::ggplot_build(q))$plot$labels$x,
               "<- Favours Drug X", fixed = TRUE)
})

test_that("subgroup_labels: the headings by label; the levels' rows and the order stay", {
  skip_if_not_installed("dplyr")
  rows <- function(code) {
    est <- data.frame(SEX = c("F", "M", NA, NA, NA), AGEGR1 = c(NA, NA, "<65", ">=65", NA),
                      STRATA1 = NA_character_, TRT01A = "Drug", estimate = 1, conf.low = 0.5, conf.high = 2, n_obs = 10L)
    est <- rbind(est, transform(est, SEX = NA, AGEGR1 = NA)[1L, ])
    env <- list2env(list(est = est), parent = asNamespace("dplyr"))
    eval(parse(text = code), env)
    env$est
  }
  heads <- function(e) e$label[e$head]
  # default: the common ADaM variables' label, the name for the others
  x <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT",
                                             subgroups = c("SEX", "AGEGR1", "STRATA1")))
  code <- x$data$steps[[2L]]$code
  expect_match(code, "sg_label <- c(SEX = \"Sex\", AGEGR1 = \"Age group\")", fixed = TRUE)
  e <- rows(code)
  expect_identical(heads(e), c("Sex", "Age group"))
  expect_true(all(c("    F: Drug", "    M: Drug", "    <65: Drug", "    >=65: Drug") %in% e$label))
  # given: over the default, for a variable of the template's subgroups only
  y <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT",
                                             subgroups = c("SEX", "AGEGR1"),
                                             subgroup_labels = list(SEX = "Gender", ECOGBL = "ECOG")))
  expect_match(y$data$steps[[2L]]$code, "sg_label <- c(SEX = \"Gender\", AGEGR1 = \"Age group\")",
               fixed = TRUE)
  expect_identical(heads(rows(y$data$steps[[2L]]$code)), c("Gender", "Age group"))
  # a variable's own name when nothing labels it; the old code when no subgroup has a label
  z <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT", subgroups = "STRATA1"))
  expect_false(grepl("sg_label", z$data$steps[[2L]]$code, fixed = TRUE))
  w <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT", subgroups = "STRATA1",
                                             subgroup_labels = c(STRATA1 = "Stratum")))
  expect_match(w$data$steps[[2L]]$code, "STRATA1 = \"Stratum\"", fixed = TRUE)
  expect_error(tfl_template_expand("forest_hr", list(param = "OS", population = "ITT",
                                                     subgroup_labels = c("Sex"))), "named")
  expect_identical(tfl_template_params("forest_hr")$subgroup_labels$type, "list")
})

# ---- the reference is the first level (#722): Placebo = 1, Drug B = 2, Drug A = 3 ----
fr_arms <- c("Placebo", "Drug B", "Drug A")
fr_data <- function() {
  set.seed(7220)
  n <- 120L
  d <- data.frame(USUBJID = sprintf("DMY-%03d", seq_len(n)),
                  TRT01A = rep(fr_arms, each = n / 3L), TRT01AN = rep(1:3, each = n / 3L),
                  SEX = rep(c("F", "M"), length.out = n))
  d$AVAL <- stats::rexp(n, rate = c(0.10, 0.07, 0.05)[d$TRT01AN])
  d$CNSR <- stats::rbinom(n, 1L, 0.3)
  # the data step's factor (order_levels()): the arm in the order of TRT01AN
  d$TRT01A <- factor(d$TRT01A, levels = fr_arms)
  d
}
# the template's data step (derive) and its overall Cox analysis, run on the data
fr_hr <- function(ref = NULL) {
  x <- tfl_template_expand("forest_hr", c(list(param = "OS", population = "ITT", group = "TRT01A",
                                               subgroups = "SEX"),
                                          if (!is.null(ref)) list(ref = ref)))
  d <- fr_data()
  for (e in strsplit(as.character(unclass(x$ard$analysis_data[[1L]]$derive)), " | ", fixed = TRUE)[[1L]]) {
    d <- eval(parse(text = sprintf("dplyr::mutate(d, %s)", e)))
  }
  ard <- suppressWarnings(eval(parse(text = x$ard$analyses[[1L]]$code), list(data = d)))
  # the rows the figure's ard_stats() step gives: an estimate for every arm
  # (the reference's is 1), an interval for the others
  a <- ard[ard$stat_name %in% c("estimate", "conf.low", "conf.high"), ]
  arm <- vapply(a$variable_level, function(v) as.character(v[[1L]]), "")
  num <- function(s) {
    z <- a$stat[a$stat_name == s]
    stats::setNames(vapply(z, function(v) as.numeric(v[[1L]]), 0), arm[a$stat_name == s])
  }
  arms <- arm[a$stat_name == "estimate"]
  list(x = x, est = data.frame(
    TRT01A = arms, estimate = unname(num("estimate")),
    conf.low = unname(num("conf.low")[arms]), conf.high = unname(num("conf.high")[arms]),
    n_obs = 40L))
}

test_that("ref: the default reference is Placebo (the first level); ref = Drug B makes it Drug B", {
  fr_skip()
  d <- fr_data()
  for (ref in list(NULL, "Placebo", "Drug B")) {
    used <- ref %||% "Placebo"
    r <- fr_hr(ref)
    d2 <- d
    d2$TRT01A <- stats::relevel(d2$TRT01A, ref = used)
    fit <- survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = d2)
    hr <- exp(stats::coef(fit))
    names(hr) <- sub("^TRT01A", "", names(hr))
    others <- setdiff(fr_arms, used)
    expect_setequal(r$est$TRT01A, fr_arms)
    expect_true(is.na(r$est$conf.low[r$est$TRT01A == used]), info = used)
    expect_equal(stats::setNames(r$est$estimate, r$est$TRT01A)[others], hr[others],
                 tolerance = 1e-6, info = used)
    ci <- exp(stats::confint.default(fit))
    rownames(ci) <- sub("^TRT01A", "", rownames(ci))
    expect_equal(r$est$conf.low[match(others, r$est$TRT01A)], unname(ci[others, 1L]),
                 tolerance = 1e-6, info = used)
    expect_equal(r$est$conf.high[match(others, r$est$TRT01A)], unname(ci[others, 2L]),
                 tolerance = 1e-6, info = used)
  }
})

test_that("comparison: shows only that arm, with the HR it has without the filter; the reference is refused", {
  fr_skip()
  r <- fr_hr()
  run <- function(code, est) eval(parse(text = code), list2env(list(est = est),
                                                              parent = asNamespace("dplyr")))
  all <- run(tflspec:::.forest_est_code("TRT01A", character()), r$est)
  one <- run(tflspec:::.forest_est_code("TRT01A", character(), "Drug A"), r$est)
  expect_identical(nrow(all), 2L)
  expect_setequal(all$TRT01A, c("Drug B", "Drug A"))
  expect_identical(nrow(one), 1L)
  expect_identical(one$txt, all$txt[all$TRT01A == "Drug A"])
  expect_identical(one$level, "All subjects")
  # comparison = the reference (Placebo, the first level: no row of its own): refused at run time
  expect_error(run(tflspec:::.forest_est_code("TRT01A", character(), "Placebo"), r$est),
               "`comparison` (Placebo) is the reference arm or not an arm", fixed = TRUE)
  # ... and with ref given, before any run
  expect_error(tfl_template_expand("forest_hr", list(param = "OS", population = "ITT", ref = "Drug B",
                                                    comparison = "Drug B")),
               "`comparison` is the reference arm")
  expect_error(tfl_fig_template("forest_hr", ref = "Drug B", comparison = "Drug B"),
               "`comparison` is the reference arm")
  # tfl_fig_template(ref =): the analysis data's derive relevels the arm
  an <- tfl_fig_forest_analyses("ADTTE", "OS", "FASFL", "TRT01A", "SEX", ref = "Drug B")
  expect_identical(an$analysis_data$derive,
                   "TRT01A = factor(TRT01A, levels = c(\"Drug B\", setdiff(levels(factor(TRT01A)), \"Drug B\")))")
  expect_false("derive" %in% names(tfl_fig_forest_analyses("ADTTE", "OS", "FASFL", "TRT01A", "SEX")$analysis_data))
})
