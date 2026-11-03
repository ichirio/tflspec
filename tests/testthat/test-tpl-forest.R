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
})

test_that("in a study: the ARD built; A (the call, the layout) = B (the design) = the template SPEC", {
  skip_on_cran()
  fr_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "FR-1")
  o <- tfl_output("F-9-1", "figure", population = "SAF", label = "Forest",
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
  q <- tpl_forest_hr(ard, group = TRT01A, subgroups = c(SEX, AGEGR1), favours = c("Drug A", "Drug B"),
                     x_breaks = c(0.5, 1, 2), size_by_n = TRUE, bold_headings = TRUE)
  b <- suppressWarnings(ggplot2::ggplot_build(q))
  pts <- ggplot2::layer_data(q, 3L)
  expect_true(length(unique(stats::na.omit(pts$size))) > 1L)
  expect_true(all(stats::na.omit(b$layout$panel_params[[1L]]$x$get_labels()) %in% c("0.5", "1", "2")))
  expect_match(q$labels$x %||% b$plot$labels$x, "<- Favours Drug A", fixed = TRUE)
  face <- q$theme$axis.text.y$face
  expect_true(all(c("bold", "plain") %in% face))
  # bold exactly at the heading rows, in the order the axis lists its labels
  ylab <- b$layout$panel_params[[1L]]$y$get_labels()
  expect_identical(face == "bold", ylab %in% c("SEX", "AGEGR1"))
  expect_s3_class(tpl_forest_layout(q), "patchwork")
})

test_that("comparison: the Cox model's reference is the other arm, whatever the arms' order", {
  x <- tfl_template_expand("forest_hr", list(param = "OS", population = "ITT", group = "TRT01P",
                                             comparison = "Drug X"))
  expect_match(unclass(x$ard$analysis_data[[1L]]$derive),
               "TRT01P = factor(TRT01P, levels = c(setdiff(levels(factor(TRT01P)), \"Drug X\"), \"Drug X\"))",
               fixed = TRUE)
  expect_null(tfl_template_expand("forest_hr", list(param = "OS", population = "ITT"))$ard$analysis_data[[1L]]$derive)
  skip_on_cran()
  fr_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "FR-2")
  # (the bundled data's arms: Drug X first, TRT01PN 1)
  o <- tfl_output("F-9-2", "figure", population = "SAF", label = "Forest", extends = "defaults",
                  template = list(name = "forest_hr", args = list(
                    param = "OS", population = "SAF", group = "TRT01P", subgroups = "SEX",
                    comparison = "Drug X")))
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
  o <- tfl_output("F-9-3", "figure", population = "SAF", label = "Forest", extends = "defaults",
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
