# The forest plot from the figure's own ARD (tflplanner #293 phase 6): the
# analyses the design brings, the ARD they make, the pieces that draw it.

test_that("tfl_fig_forest_analyses() gives the rows of the figure's own ARD definition", {
  an <- tfl_fig_forest_analyses("ADTTE", "OS", "FASFL", "TRT01P", c("SEX", "AGEGR1"))
  expect_named(an, c("analysis_data", "analyses"))
  expect_identical(an$analysis_data$data_id, "adtte_os")
  expect_identical(an$analysis_data$add, "SEX | AGEGR1")
  expect_identical(an$analysis_data$where, "PARAMCD == \"OS\"")
  expect_identical(an$analyses$analysis_id, c("HR", "HR_SEX", "HR_AGEGR1"))
  expect_true(all(an$analyses$method == "custom"))
  expect_match(an$analyses$code[1], "cardx::ard_regression(survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRT01P, data = data), exponentiate = TRUE)", fixed = TRUE)
  expect_match(an$analyses$code[2], "cards::ard_strata(data, .strata = SEX, .f = ~ cardx::ard_regression(", fixed = TRUE)
  # one string of subgroups reads the same; none: the overall only
  expect_identical(tfl_fig_forest_analyses(subgroups = "SEX, AGEGR1")$analyses$analysis_id,
                   an$analyses$analysis_id)
  expect_identical(tfl_fig_forest_analyses(subgroups = character())$analyses$analysis_id, "HR")
  expect_true(is.na(tfl_fig_forest_analyses(subgroups = character())$analysis_data$add))
})

test_that("the analyses make an ARD whose hazard ratios are coxph()'s, overall and by subgroup", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  adam <- exact_data()
  dir <- exact_dir(adam)
  an <- tfl_fig_forest_analyses("ADTTE", unique(adam$ADTTE$PARAMCD)[1], "SAF", "TRTA", "SEX")
  S <- .ard_spec_sheets
  sh <- function(rows, cols) exact_sheet(lapply(seq_len(nrow(rows)), function(i)
    c(list(output_id = "F1"), as.list(rows[i, ]))), cols)
  sp <- tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                                list(dataset = "ADTTE", path = "adam/ADTTE.rds")), S$datasets),
    populations = exact_sheet(list(list(population_id = "SAF", dataset = "ADSL",
                                        where = "SAFFL == \"Y\"")), S$populations),
    analysis_data = sh(an$analysis_data, S$analysis_data),
    analyses = sh(an$analyses, S$analyses)))
  ard <- suppressMessages(suppressWarnings(tfl_build_ard(sp, dir = dir, save = FALSE)))
  expect_setequal(unique(ard$analysis_id), c("HR", "HR_SEX"))
  # the numbers are the model's
  adsl <- adam$ADSL
  d <- adam$ADTTE[adam$ADTTE$PARAMCD == unique(adam$ADTTE$PARAMCD)[1], ]
  d <- d[d$USUBJID %in% adsl$USUBJID[adsl$SAFFL == "Y"], ]
  d$SEX <- adsl$SEX[match(d$USUBJID, adsl$USUBJID)]
  fit <- survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = d)
  hr <- ard[ard$analysis_id == "HR" & ard$stat_name == "estimate", ]
  expect_equal(sort(unname(unlist(hr$stat)))[-1], sort(unname(exp(coef(fit)))), tolerance = 1e-6)
  f <- d[d$SEX == "F", ]
  fit_f <- survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = f)
  hr_f <- ard[ard$analysis_id == "HR_SEX" & ard$stat_name == "estimate" &
                vapply(ard$group1_level, function(v) identical(as.character(v), "F"), NA), ]
  expect_equal(sort(unname(unlist(hr_f$stat)))[-1], sort(unname(exp(coef(fit_f)))), tolerance = 1e-6)
})

test_that("the forest template is in parts, with its analyses, and its design checks clean", {
  tp <- tfl_fig_templates()
  expect_true(tp$parts[tp$template == "forest_hr"])
  d <- tfl_fig_template("forest_hr", data = "ADTTE", param = "TTDE", pop = "SAFFL", group = "TRT01A",
                        subgroups = "SEX, AGEGR1", comparison = "Xanomeline High Dose")
  an <- attr(d, "analyses")
  expect_identical(an$analyses$analysis_id, c("HR", "HR_SEX", "HR_AGEGR1"))
  # (the statistics steps are data steps of the design: ard_stats, then the code)
  expect_identical(vapply(d$data, `[[`, "", "step"), c("read", "param", "flag", "ard_stats", "code"))
  expect_identical(vapply(d$layers, `[[`, "", "layer"), c("vline", "errorbar_h", "point", "text_column"))
  pr <- tfl_check_fig_design(d)
  expect_equal(nrow(pr), 0L)
  code <- paste(tfl_fig_design_code(d, "F1", setup = TRUE, save = FALSE, name = "plot"), collapse = "\n")
  expect_match(code, 'filter(TRT01A == "Xanomeline High Dose")', fixed = TRUE)
  expect_match(code, "plot <- plot + p_txt4 + plot_layout(widths = c(0.6, 0.4))", fixed = TRUE)
  expect_match(code, "geom_errorbar(data = est, aes(y = y, xmin = conf.low, xmax = conf.high), width = 0.25, na.rm = TRUE)", fixed = TRUE)
  # no comparison: every arm but the reference; no subgroups: all subjects
  d2 <- tfl_fig_template("forest_hr", subgroups = "")
  expect_false(grepl("filter(TRT01P ==", paste(tfl_fig_design_code(d2, "F1", save = FALSE), collapse = "\n"), fixed = TRUE))
  expect_identical(attr(d2, "analyses")$analyses$analysis_id, "HR")
  expect_match(.forest_est_code("TRT01P", character()), 'label = "All subjects"', fixed = TRUE)
})

test_that("text_column: its columns, a panel at the right; not with one below", {
  expect_identical(.text_columns("N = n_obs | Hazard ratio (95% CI) = txt"),
                   c(N = "n_obs", "Hazard ratio (95% CI)" = "txt"))
  expect_identical(.text_columns("txt"), c(txt = "txt"))
  expect_error(.text_columns(""), "columns")
  pieces <- tfl_fig_parts()
  expect_true("text_column" %in% pieces$piece[pieces$section == "layers"])
  expect_true("errorbar_h" %in% pieces$piece[pieces$section == "layers"])
  d <- tfl_fig_template("forest_hr")
  d$layers <- c(d$layers, list(list(layer = "n_table", data = "est", x = "y", group = "TRT01P")))
  expect_error(tfl_fig_design_code(d, "F1", save = FALSE), "below it or at its right, not both")
})
