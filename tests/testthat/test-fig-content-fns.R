# The figure content functions a template names in `figure:` (km_curves,
# forest_hr) and their lookup tfl_figure_fn().  Fictional data only.

.fcf_adtte <- function() {
  set.seed(20261116)
  n <- 40L
  data.frame(
    USUBJID = sprintf("S-%03d", seq_len(n)),
    TRT01A = rep(c("Control", "Drug X"), each = n / 2L),
    SEX = rep(c("F", "M"), times = n / 2L),
    AVAL = round(c(stats::rexp(n / 2L, 1 / 200), stats::rexp(n / 2L, 1 / 320)) + 1),
    CNSR = stats::rbinom(n, 1L, 0.3),
    stringsAsFactors = FALSE
  )
}

.fcf_bign <- function(d) {
  b <- cards::ard_tabulate(d, variables = "TRT01A")
  b$analysis_id <- "BIGN"
  b
}

# The HR ARD as the template's analysis makes it: a Cox model per
# condition row, tagged with the subgroup and its level, ALL first
.fcf_hr <- function(d) {
  conds <- list(c("ALL", "ALL"), c("SEX", "F"), c("SEX", "M"))
  rows <- lapply(conds, function(cnd) {
    sub <- if (cnd[[1L]] == "ALL") d else d[d[[cnd[[1L]]]] == cnd[[2L]], , drop = FALSE]
    fit <- survival::coxph(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = sub)
    a <- cardx::ard_regression(fit, exponentiate = TRUE)
    a$group1 <- "subgroup"
    a$group1_level <- list(cnd[[1L]])
    a$group2 <- "subgroup_lvl"
    a$group2_level <- list(cnd[[2L]])
    a
  })
  hr <- do.call(rbind, lapply(rows, as.data.frame))
  hr$analysis_id <- "HR"
  hr
}

test_that("tfl_figure_fn() finds the content by the template's figure: name", {
  expect_identical(tfl_figure_fn("km_curves"), km_curves)
  expect_identical(tfl_figure_fn("forest_hr"), forest_hr)
  expect_error(tfl_figure_fn("waterfall"), "km_curves, forest_hr")
  expect_error(tfl_figure_fn(NA_character_), "one of")
  expect_error(tfl_figure_fn(c("km_curves", "forest_hr")), "one of")
})

test_that("km_curves() draws the KM curves with N in the legend", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("cards")
  d <- .fcf_adtte()
  fit <- ggsurvfit::survfit2(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = d)
  p <- km_curves(fit, .fcf_bign(d), list(time_unit = "days"))
  expect_s3_class(p, "ggplot")
  expect_no_error(b <- ggplot2::ggplot_build(p))
  sc <- b$plot$scales$get_scales("colour")
  expect_equal(unname(sc$get_labels()), c("Control (N=20)", "Drug X (N=20)"), ignore_attr = TRUE)
  expect_identical(p$labels$x %||% ggplot2::get_labs(p)$x, "Time (Days)")
  expect_identical(p$labels$y %||% ggplot2::get_labs(p)$y, "Survival Probability")
})

test_that("km_curves() takes its params; an unknown one stops", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  d <- .fcf_adtte()
  fit <- ggsurvfit::survfit2(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRT01A, data = d)
  for (lg in c("inside", "bottom", "none")) {
    p <- km_curves(fit, NULL, list(legend = lg, x_label = "Months ({time_unit})",
                                   time_unit = "months", y_label = "S(t)"))
    expect_no_error(ggplot2::ggplot_build(p))
  }
  expect_identical(p$theme$legend.position, "none")
  # no BIGN: the levels alone
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  expect_equal(unname(sc$get_labels()), c("Control", "Drug X"), ignore_attr = TRUE)
  expect_error(km_curves(fit, NULL, list(colour = "red")), "unknown params colour")
  expect_error(km_curves(fit, NULL, list(legend = "left")), "should be one of")
  expect_error(km_curves(list(), NULL), "survfit")
})

test_that("forest_hr() draws a row per condition row, ALL first", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  d <- .fcf_adtte()
  hr <- .fcf_hr(d)
  p <- forest_hr(hr)
  expect_s3_class(p, "ggplot")
  expect_no_error(b <- ggplot2::ggplot_build(p))
  # three conditions (ALL, SEX F, SEX M): three points; a heading for SEX
  expect_identical(nrow(p$data), 3L)
  expect_identical(p$data$label, c("All subjects", "    F", "    M"))
  ys <- b$plot$scales$get_scales("y")
  # (the last label: the text columns' heading row, empty at the left)
  expect_identical(ys$get_labels(), c("All subjects", "Sex", "    F", "    M", ""))
  # the reference line at 1
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), logical(1))))
  # the estimates are Drug X's (the arm with an interval)
  ref <- .fig_fn_num(hr$stat)[.fig_fn_chr(hr$stat_name) == "estimate" &
                                .fig_fn_chr(hr$variable_level) == "Drug X"]
  expect_equal(p$data$estimate, unname(ref))
})

test_that("forest_hr() takes its params", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  d <- .fcf_adtte()
  hr <- .fcf_hr(d)
  # the BIGN rows in the same ARD are not read
  ard <- rbind(hr[, c("analysis_id", "variable", "variable_level", "stat_name", "stat")],
                                  as.data.frame(.fcf_bign(d))[, c("analysis_id", "variable", "variable_level",
                                                                  "stat_name", "stat")])
  p <- forest_hr(hr, list(comparison = "Drug X", subgroup_labels = list(SEX = "Gender"),
                          favours = c("Favours Drug X", "Favours Control"),
                          x_breaks = c(0.25, 0.5, 1, 2, 4), size_by_n = TRUE,
                          bold_headings = TRUE, n_by_arm = c("Drug X", "Control")))
  expect_no_error(b <- ggplot2::ggplot_build(p))
  labs <- b$plot$scales$get_scales("y")$labels
  expect_true(is.expression(labs))
  expect_true(any(grepl("Gender", vapply(labs, deparse, ""))))
  expect_true(all(grepl("^\\S+\\s+\\d+/\\d+\\s+\\d+/\\d+", p$data$txt) | grepl("NE$", p$data$txt)))
  # no subgroup rows: one row
  all_only <- hr[.fig_fn_chr(hr$group1_level) == "ALL", ]
  p1 <- forest_hr(all_only)
  expect_identical(nrow(p1$data), 1L)
  expect_no_error(ggplot2::ggplot_build(p1))
  expect_no_error(ggplot2::ggplot_build(forest_hr(ard)))
  expect_error(forest_hr(hr, list(comparison = "Control")), "reference arm")
  expect_error(forest_hr(hr, list(favours = "x")), "two labels")
  expect_error(forest_hr(hr, list(size = 3)), "unknown params size")
  expect_error(forest_hr(as.data.frame(.fcf_bign(d))), "no HR rows")
})
