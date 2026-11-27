# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

test_that("the built-in style carries the sample programs' values", {
  s <- tfl_fig_style()
  expect_named(s, c("settings", "colors", "markers"))
  expect_equal(tflspec:::.fs_get("censor_size", "km"), "3")
  expect_equal(tflspec:::.fs_get("bar_width", "waterfall"), "0.8")
  expect_equal(tflspec:::.fs_get("theme", "swimmer"), "L_axis")
  expect_equal(tflspec:::.fs_get("theme", "km"), "boxed")          # common row
  expect_equal(tfl_fig_palettes()$response[["PR"]], "#0000FF")
  expect_equal(tfl_fig_palettes()$response_assessment[["PR"]], "#0000CD")
})

test_that("the generators take their defaults from the style", {
  code <- paste(tfl_fig_km(style = "single_arm"), collapse = "\n")
  expect_match(code, 'linetype = "twodash"', fixed = TRUE)
  expect_match(code, "add_censor_mark(shape = 3, size = 3, stroke = 0.6", fixed = TRUE)
  wf <- paste(tfl_fig_waterfall(), collapse = "\n")
  expect_match(wf, "geom_col(width = 0.8)", fixed = TRUE)
  expect_match(wf, "hjust = -0.3, size = 3.7", fixed = TRUE)

  # a company's style wins
  s <- tfl_fig_style()
  s$settings$value[s$settings$key == "censor_size"] <- "2.5"
  s$colors$colour[s$colors$palette == "response" & s$colors$value == "CR"] <- "#00AA00"
  old <- options(tflspec.fig_style = s)
  on.exit(options(old), add = TRUE)
  expect_match(paste(tfl_fig_km(style = "single_arm"), collapse = "\n"),
               "size = 2.5", fixed = TRUE)
  expect_equal(tfl_fig_palettes()$response[["CR"]], "#00AA00")
})

test_that("a style workbook reads back", {
  f <- tempfile(fileext = ".xlsx")
  tfl_fig_style_template(f)
  s <- tfl_read_fig_style(f)
  expect_equal(s$settings$value, tfl_fig_style()$settings$value)
  expect_equal(s$markers$shape, tfl_fig_style()$markers$shape)
})

test_that("the helper script gives the standard's look and checks figures", {
  skip_if_not_installed("ggplot2")
  code <- tfl_fig_setup_code()
  expect_silent(parse(text = code))
  e <- new.env()
  eval(parse(text = code), envir = e)
  d <- data.frame(id = 1:5, v = c(-50, -35, 10, 25, NA),
                  r = c("CR", "PR", "SD", "PD", "SD"))
  good <- ggplot2::ggplot(d[!is.na(d$v), ], ggplot2::aes(id, v, fill = r)) +
    ggplot2::geom_col() + e$scale_fill_tfl("response") + e$theme_tfl("waterfall")
  r <- suppressMessages(e$tfl_check(good))
  expect_length(r$problems, 0L)
  expect_match(r$fingerprint, "^[0-9a-f]{32}$")

  bad <- ggplot2::ggplot(d, ggplot2::aes(id, v, fill = r)) + ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c(CR = "red", PR = "#0000FF",
                                          SD = "#FFA500", PD = "#800080"))
  w <- character()
  r <- withCallingHandlers(
    suppressMessages(tfl_check_fig(bad, levels = c("CR", "PR", "SD", "PD"))),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("Removed 1 row", r$problems)))
  expect_true(any(grepl("'CR' is drawn red", r$problems)))
  expect_true(any(grepl("legend shows", r$problems)))
  expect_true(all(startsWith(w, "Figure check:")))
})

test_that("the KM number at risk can come from the table's ARD", {
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  adtte <- adam$ADTTE
  adtte <- adtte[adtte$PARAMCD == "OS" & adtte$FASFL == "Y", ]
  times <- seq(0, 24, by = 6) * 30.4375
  km_table_ard <- cardx::ard_survival_survfit(
    survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = adtte),
    times = times)
  code <- paste(tfl_fig_km(adam, param = "OS", ard = "km_table_ard", x_max = 24,
                      x_by = 6), collapse = "\n")
  expect_match(code, "km_ard  <- km_table_ard", fixed = TRUE)
  # (the ARD's times in AVAL's own unit, converted as the curve's)
  expect_match(code, 'tte_time(data.frame(AVAL = risk_df$time, AVALU = km_unit), "months")', fixed = TRUE)
  run <- function(ard) {
    e <- new.env()
    e$adtte <- adam$ADTTE
    e$ADTTE <- adam$ADTTE
    e$km_table_ard <- ard
    e$output_path <- tempdir()
    w <- character()
    withCallingHandlers(
      suppressMessages(eval(parse(text = sub('file.path("output", "km.png")',
                                             'file.path(tempdir(), "km.png")',
                                             code, fixed = TRUE)), envir = e)),
      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
    w
  }
  expect_false(any(grepl("Figure check", run(km_table_ard))))
  bad <- km_table_ard
  i <- which(bad$stat_name == "n.risk")[3]
  bad$stat[[i]] <- bad$stat[[i]] + 1
  expect_true(any(grepl("Figure check: the number at risk", run(bad))))
})

test_that("tfl_km_risk() reads the table's ARD and checks it", {
  skip_if_not_installed("cardx")
  e <- new.env()
  eval(parse(text = tfl_fig_setup_code()), envir = e)
  d <- tfl_example_adam()$ADTTE
  d <- d[d$PARAMCD == "OS", ]
  fit <- survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = d)
  ard <- cardx::ard_survival_survfit(fit, times = c(0, 100, 200))
  r <- e$tfl_km_risk(ard, fit)
  expect_named(r, c("time", "strata", "n_risk"))
  expect_equal(nrow(r), 3L * length(unique(d$TRT01P)))
  ard$stat[[which(ard$stat_name == "n.risk")[2]]] <- -1
  expect_warning(e$tfl_km_risk(ard, fit), "Figure check: the number at risk")
})

# ---- axis_title_width (tflspec#603-4, canon 11-28) ---------------------------------

test_that("the built-in style's axis_title_width is 50; a study's figure style is read over it", {
  expect_equal(tflspec:::.fs_get("axis_title_width"), "50")
  expect_equal(tflspec:::.fs_get("axis_title_width", "km"), "50")
  expect_identical(tflspec:::.fs_title_width(), 50)
  # the study reference reads it as a number
  expect_identical(tflspec:::.tpl_fig_style_read("axis_title_width", NULL), 50)
  st <- list(figure_style = list(settings = list(
    list(key = "axis_title_width", value = "40"),
    list(type = "km", key = "axis_title_width", value = "35"))))
  expect_identical(tflspec:::.tpl_fig_style_read("axis_title_width", st), 40)
  expect_identical(tflspec:::.tpl_fig_style_read(c("km", "axis_title_width"), st), 35)
  # a study's settings table without the key: the built-in one's
  st2 <- list(figure_style = list(settings = list(list(key = "base_size", value = "11"))))
  expect_identical(tflspec:::.tpl_fig_style_read("axis_title_width", st2), 50)
  # a text value stays text
  expect_identical(tflspec:::.tpl_fig_style_read("theme", NULL), "boxed")
  # a company's style (options) without the key: the built-in width
  s <- tfl_fig_style()
  s$settings <- s$settings[s$settings$key != "axis_title_width", , drop = FALSE]
  old <- options(tflspec.fig_style = s)
  on.exit(options(old), add = TRUE)
  expect_identical(tflspec:::.fs_title_width(), 50)
  # (a row of the table's own columns, whatever they are)
  row <- s$settings[1L, , drop = FALSE]
  row[1L, ] <- NA
  row$key <- "axis_title_width"
  row$value <- "30"
  s$settings <- rbind(s$settings, row)
  options(tflspec.fig_style = s)
  expect_identical(tflspec:::.fs_title_width(), 30)
})

test_that(".fig_wrap_title() wraps an axis title at the spaces between words", {
  wrap <- tflspec:::.fig_wrap_title
  t90 <- "Mean Change from Baseline in Diastolic Blood Pressure (mmHg) with 95% Confidence Intervals"
  expect_identical(nchar(t90), 90L)
  expect_identical(wrap(t90, 50),
                   "Mean Change from Baseline in Diastolic Blood\nPressure (mmHg) with 95% Confidence Intervals")
  expect_identical(wrap(t90, 30), paste("Mean Change from Baseline in", "Diastolic Blood Pressure",
                                        "(mmHg) with 95% Confidence", "Intervals", sep = "\n"))
  # a title no longer than the width: as written
  t30 <- "Mean Change from Baseline (mm)"
  expect_identical(nchar(t30), 30L)
  expect_identical(wrap(t30, 50), t30)
  expect_identical(wrap(t30, 30), t30)
  # no width (NULL, !unset): no wrap; NULL / NA / empty / R text: unchanged
  expect_identical(wrap(t90, NULL), t90)
  expect_identical(wrap(t90, tfl_unset()), t90)
  expect_null(wrap(NULL, 50))
  expect_identical(wrap(NA_character_, 50), NA_character_)
  expect_identical(wrap("", 50), "")
  r <- tfl_r(paste0("paste(\"", t90, "\")"))
  expect_identical(wrap(r, 50), r)
  # a "\n" the title has is kept; only a line longer than the width is wrapped
  t2 <- "Mean Change from Baseline\nin Diastolic Blood Pressure (mmHg) with 95% Confidence Intervals"
  expect_identical(wrap(t2, 50),
                   "Mean Change from Baseline\nin Diastolic Blood Pressure (mmHg) with 95%\nConfidence Intervals")
  expect_identical(wrap("Short\nTitle", 50), "Short\nTitle")
  # wrapping what is wrapped changes nothing; the spaces inside a line are kept
  expect_identical(wrap(wrap(t90, 50), 50), wrap(t90, 50))
  fav <- "Hazard Ratio\n<- Favours Drug X            Favours Control ->"
  expect_identical(wrap(fav, 50), fav)
  expect_identical(wrap(fav, 30), "Hazard Ratio\n<- Favours Drug X\nFavours Control ->")
  # a word longer than the width stays whole; a text without spaces is one line
  expect_identical(wrap("Supercalifragilistic word", 10), "Supercalifragilistic\nword")
  expect_identical(wrap(strrep("x", 60), 50), strrep("x", 60))
  # the width: a number (or its text), positive and whole
  expect_identical(wrap(t90, "50"), wrap(t90, 50))
  expect_error(wrap(t90, 0), "positive whole number")
  expect_error(wrap(t90, 2.5), "positive whole number")
  expect_error(wrap(t90, "wide"), "axis_title_width")
  expect_error(wrap(t90, c(50, 60)), "axis_title_width")
})
