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
    survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01P, data = adtte),
    times = times)
  code <- paste(tfl_fig_km(adam, param = "OS", ard = "km_table_ard", x_max = 24,
                      x_by = 6), collapse = "\n")
  expect_match(code, "km_ard  <- km_table_ard", fixed = TRUE)
  expect_match(code, "/ 30.4375", fixed = TRUE)
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
  fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01P, data = d)
  ard <- cardx::ard_survival_survfit(fit, times = c(0, 100, 200))
  r <- e$tfl_km_risk(ard, fit)
  expect_named(r, c("time", "strata", "n_risk"))
  expect_equal(nrow(r), 3L * length(unique(d$TRT01P)))
  ard$stat[[which(ard$stat_name == "n.risk")[2]]] <- -1
  expect_warning(e$tfl_km_risk(ard, fit), "Figure check: the number at risk")
})
