# The figure templates met on real-looking data (an end-to-end check): a
# dataset of several parameters, subjects with no time or no group, blank
# visits, several records a subject, a median that rounds at a half, an axis
# that stops before the last time.  Each is stopped or said, not drawn wrong.

e2e_pfs <- function() {
  x <- tflspec::adtte[tflspec::adtte$PARAMCD == "PFS", ]
  # (the package's data has a subject with no arm: left out here, as the
  # templates leave it out, so that these tests count their own)
  x <- x[!is.na(x$TRT01A) & x$TRT01A != "", ]
  rownames(x) <- NULL
  x
}

e2e_mean_df <- function() {
  df <- tflspec::advs[tflspec::advs$PARAMCD == "SYSBP", ]
  df$AVISIT <- as.character(df$AVISIT)
  df
}

e2e_visits <- function(df) {
  df$AVISIT <- stats::reorder(factor(df$AVISIT), df$AVISITN)
  df
}

# a study of the data frames given (XPT files), its report written as a template
e2e_study <- function(d, data, id, template, args) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (n in names(data)) {
    haven::write_xpt(tflspec:::.ai_chr(data[[n]]), file.path(d, "adam", paste0(n, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "E2E-1")
  o <- tfl_output(id, "figure", population = "SAF", extends = "defaults",
                  template = list(name = template, args = args))
  tfl_write_output(o, file.path(d, "spec", "outputs", paste0(id, ".yml")))
  invisible(d)
}

test_that("km: several parameters in the data and no `param` is stopped, naming them", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("dplyr")
  all_p <- tflspec::adtte
  expect_gt(length(unique(all_p$PARAMCD)), 1L)
  for (tpl in c("km", "km_survival", "km_cumulative")) {
    e <- tryCatch(tfl_tpl(all_p, tpl, by = "TRT01A"), error = function(e) conditionMessage(e))
    expect_match(e, "more than one PARAMCD", info = tpl)
    for (pc in unique(all_p$PARAMCD)) expect_match(e, pc, fixed = TRUE, info = tpl)
  }
  expect_match(tryCatch(tfl_tpl(all_p, "km", by = "TRT01A"), error = conditionMessage), "give `param`",
               fixed = TRUE)
  expect_match(tryCatch(tfl_tpl(all_p, "km_survival", by = "TRT01A"), error = conditionMessage),
               "a param step", fixed = TRUE)
  # one parameter (or a dataset with no PARAMCD): drawn
  expect_s3_class(tfl_tpl(e2e_pfs(), "km_survival", by = "TRT01A"), "ggsurvfit")
  one <- e2e_pfs()
  one$PARAMCD <- NULL
  expect_s3_class(tfl_tpl(one, "km", by = "TRT01A"), "ggsurvfit")
})

test_that("km in a study: no `param` stops the build; with it the counts are one parameter's", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  e2e_study(d, list(adsl = tflspec::adsl, adtte = tflspec::adtte), "F-1", "km", list(by = "TRT01A"))
  expect_error(suppressMessages(tfl_run_output("F-1", dir = d)), "more than one PARAMCD")
})

test_that("tfl_run_output() lets a figure's own warnings through (a table's do)", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  tte <- tflspec::adtte
  tte$TRT01A[which(tte$PARAMCD == "PFS")[1L]] <- ""
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  e2e_study(d, list(adsl = tflspec::adsl, adtte = tte), "F-1", "km",
            list(by = "TRT01A", param = "PFS", population = "SAF"))
  w <- character()
  withCallingHandlers(suppressMessages(tfl_run_output("F-1", dir = d)),
                      warning = function(x) {
                        w <<- c(w, conditionMessage(x))
                        invokeRestart("muffleWarning")
                      })
  expect_true(any(grepl("1 subjects with an empty TRT01A were left out", w, fixed = TRUE)))
})

test_that("a report with several objects lets a figure's warnings through too", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("haven")
  tte <- tflspec::adtte
  tte$TRT01A[which(tte$PARAMCD == "PFS")[1L]] <- ""
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  e2e_study(d, list(adsl = tflspec::adsl, adtte = tte), "F-1", "km",
            list(by = "TRT01A", param = "PFS", population = "SAF"))
  st <- tfl_read_study_spec(d)
  o <- tfl_read_output(file.path(d, "spec", "outputs", "F-1.yml"))
  one <- tfl_resolve_output(o, st)
  expect_true(inherits(one, "tfl_output"))
  # the figure object, as the report's `contents:` runs it
  data <- tflspec:::.contents_fig_data(one, st, d)
  w <- character()
  withCallingHandlers(tfl_content_run(one, data),
                      warning = function(x) {
                        w <<- c(w, conditionMessage(x))
                        invokeRestart("muffleWarning")
                      })
  expect_true(any(grepl("empty TRT01A", w)))
})

test_that("km family: subjects with no AVAL or no CNSR are left out, with a warning and their count", {
  skip_if_not_installed("ggsurvfit")
  df <- e2e_pfs()
  df$CNSR[2L] <- NA
  df$AVAL[3L] <- NA
  df$AVAL[4L] <- NA
  df$CNSR[4L] <- NA
  for (tpl in c("km", "km_survival", "km_cumulative")) {
    expect_warning(p <- tfl_tpl(df, tpl, by = "TRT01A"),
                   sprintf("%s: 3 subjects with a missing AVAL or CNSR were left out", tpl), fixed = TRUE)
    expect_s3_class(p, "ggsurvfit")
  }
  # the same curves as the data without them
  ok <- df[!is.na(df$AVAL) & !is.na(df$CNSR), ]
  p1 <- suppressWarnings(tfl_tpl(df, "km_survival", by = "TRT01A"))
  p2 <- tfl_tpl(ok, "km_survival", by = "TRT01A")
  expect_identical(tplfig_png(p1), tplfig_png(p2))
  # clean data: no word
  expect_no_warning(tfl_tpl(ok, "km_survival", by = "TRT01A"))
  # the time and censor variables the report names
  df2 <- ok
  df2$MYT <- df2$AVAL
  df2$MYT[1L] <- NA
  expect_warning(tfl_tpl(df2, "km", by = "TRT01A", time = "MYT"), "missing MYT or CNSR", fixed = TRUE)
})

test_that("km: the median in the legend is rounded as the tables are (half away from zero in a SAS study)", {
  skip_if_not_installed("ggsurvfit")
  # one arm of three subjects, the median at 7.85
  df <- data.frame(USUBJID = c("A", "B", "C"), TRT01A = "Arm", AVAL = c(5, 7.85, 9), CNSR = 0L,
                   AVALU = "MONTHS", PARAMCD = "X", stringsAsFactors = FALSE)
  label <- function(rounding) {
    old <- options(rtfreporter.rounding = rounding)
    on.exit(options(old))
    p <- tfl_tpl(df, "km", by = "TRT01A", legend_median = TRUE)
    unname(p$scales$get_scales("colour")$labels)
  }
  expect_match(label("sas"), "^All: 7\\.9 ")
  expect_match(label("r"), "^All: 7\\.8 ")
  # the digits
  old <- options(rtfreporter.rounding = "sas")
  on.exit(options(old))
  p <- tfl_tpl(df, "km", by = "TRT01A", legend_median = TRUE, legend_median_digits = 2)
  expect_match(unname(p$scales$get_scales("colour")$labels), "^All: 7\\.85 ")
  # a median that is not reached
  df$CNSR <- 1L
  p <- tfl_tpl(df, "km", by = "TRT01A", legend_median = TRUE)
  expect_match(unname(p$scales$get_scales("colour")$labels), "^All: NE ")
})

test_that("km family: the x axis reaches the last time (the next break), whatever the step", {
  skip_if_not_installed("ggsurvfit")
  df <- e2e_pfs()
  # (the last time in months; a time in days is converted at 365.25 / 12)
  last <- max(df$AVAL / ifelse(df$AVALU == "DAYS", 365.25 / 12, 1))
  for (tpl in c("km", "km_survival", "km_cumulative")) {
    for (step in c(1, 3, 7)) {
      p <- tfl_tpl(df, tpl, by = "TRT01A", x_step = step)
      lim <- p$scales$get_scales("x")$limits
      expect_gte(max(lim), last - 1e-6, label = sprintf("%s, step %s", tpl, step))
      expect_lt(max(lim), last + step + 1e-6, label = sprintf("%s, step %s", tpl, step))
      expect_identical(max(lim) %% step, 0, label = sprintf("%s, step %s", tpl, step))
    }
  }
  # an x_max the report names stays
  p <- tfl_tpl(df, "km_survival", by = "TRT01A", x_step = 3, x_max = 12)
  expect_identical(max(p$scales$get_scales("x")$limits), 12)
  # the code
  km <- function(...) {
    k <- tfl_template_expand("km_survival", list(by = "TRT01A", ...))
    unlist(lapply(k$data$steps, function(s) if (identical(s$step, "code")) s$code))
  }
  expect_true("x_breaks <- seq(0, ceiling(max(fit$time) / 3 - 1e-8) * 3, by = 3)" %in% km(x_step = 3))
})

test_that("mean_se / mean_ci: a blank or missing group or visit is left out, with a warning", {
  skip_if_not_installed("dplyr")
  df <- e2e_mean_df()
  arms <- sort(unique(df$TRT01A))
  visits <- unique(df$AVISIT[order(df$AVISITN)])
  df$AVISIT[5L] <- ""
  df$TRT01A[6L] <- ""
  df$TRT01A[7L] <- NA
  for (nm in c("mean_se", "mean_ci")) {
    f <- get(paste0("tpl_", nm))
    expect_warning(p <- f(e2e_visits(df), group = TRT01A, param = "SYSBP"),
                   sprintf("%s: 3 records with an empty TRT01A or AVISIT were left out", nm), fixed = TRUE)
    b <- ggplot2::ggplot_build(p)
    expect_identical(b$layout$panel_params[[1L]]$x$get_labels(), visits, info = nm)
    expect_identical(length(unique(ggplot2::layer_data(p, 2L)$colour)), length(arms), info = nm)
    # the means of the records that are left: the data without the three
    rest <- df[-(5:7), ]
    clean <- f(e2e_visits(rest), group = TRT01A, param = "SYSBP")
    expect_identical(ggplot2::layer_data(p, 2L), ggplot2::layer_data(clean, 2L), info = nm)
    expect_no_warning(f(e2e_visits(rest), group = TRT01A, param = "SYSBP"))
  }
})

test_that("mean_ci: a group with one record at a visit has no bar and no warning", {
  skip_if_not_installed("dplyr")
  # the first visits only (the later ones have a record or two a group)
  df <- e2e_mean_df()
  df <- df[df$AVISITN <= 6, ]
  arm <- sort(unique(df$TRT01A))[1L]
  at <- df$TRT01A == arm & df$AVISITN == max(df$AVISITN)
  df <- df[!at | cumsum(at) == 1L, ]
  expect_equal(sum(df$TRT01A == arm & df$AVISITN == max(df$AVISITN)), 1L)
  expect_no_warning(p <- tpl_mean_ci(e2e_visits(df), group = TRT01A, param = "SYSBP"))
  eb <- ggplot2::layer_data(p, 3L)
  expect_identical(sum(is.na(eb$ymin)), 1L)
  expect_identical(sum(is.na(eb$ymax)), 1L)
  expect_false(anyNA(ggplot2::layer_data(p, 2L)$y))
  # the others are mean -/+ t * se
  d <- df[df$TRT01A != arm & df$AVISITN == max(df$AVISITN), ]
  g <- d[d$TRT01A == d$TRT01A[1L], "AVAL"]
  expect_equal(max(eb$ymax, na.rm = TRUE) >= mean(g), TRUE)
  # the code
  k <- tfl_template_expand("mean_ci", list(param = "SYSBP", data = "ADVS"))
  code <- paste(tflspec:::.fig_stats_code(k$data$steps[length(k$data$steps)], "df")$code, collapse = "\n")
  expect_match(code, "ifelse(n > 1, mean - qt(0.975, pmax(n - 1, 1)) * se, NA_real_)", fixed = TRUE)
})

test_that("mean_se / mean_ci: a record with no value is left out, with a warning and its count", {
  skip_if_not_installed("dplyr")
  df <- e2e_mean_df()
  expect_false(anyNA(df$AVAL))
  df$TRT01A <- as.character(df$TRT01A)
  df$AVAL[c(2L, 3L)] <- NA
  # (a blank group and no value at once: counted once, as the group's)
  df$TRT01A[4L] <- ""
  df$AVAL[4L] <- NA
  for (nm in c("mean_se", "mean_ci")) {
    f <- get(paste0("tpl_", nm))
    w <- character()
    p <- withCallingHandlers(f(e2e_visits(df), group = TRT01A, param = "SYSBP"),
                             warning = function(x) {
                               w <<- c(w, conditionMessage(x))
                               invokeRestart("muffleWarning")
                             })
    expect_identical(w, c(sprintf("%s: 1 records with an empty TRT01A or AVISIT were left out", nm),
                          sprintf("%s: 2 records with a missing AVAL were left out", nm)), info = nm)
    # the means of the records that are left
    clean <- f(e2e_visits(df[-(2:4), ]), group = TRT01A, param = "SYSBP")
    expect_identical(ggplot2::layer_data(p, 2L), ggplot2::layer_data(clean, 2L), info = nm)
    expect_no_warning(f(e2e_visits(df[-(2:4), ]), group = TRT01A, param = "SYSBP"))
  }
  # the variable averaged is the one named
  chg <- e2e_mean_df()
  chg$CHG[5L] <- NA
  expect_warning(tpl_mean_se(e2e_visits(chg), group = TRT01A, value = CHG, param = "SYSBP"),
                 sprintf("mean_se: %d records with a missing CHG were left out", sum(is.na(chg$CHG))),
                 fixed = TRUE)
})

test_that("mean_se in a study: records with no value reach the summary, which says them", {
  skip_on_cran()
  skip_if_not_installed("haven")
  vs <- tflspec::advs
  k <- which(vs$PARAMCD == "SYSBP")[c(3L, 9L, 20L)]
  vs$AVAL[k] <- NA
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  e2e_study(d, list(adsl = tflspec::adsl, advs = vs), "F-1", "mean_se",
            list(param = "SYSBP", population = "SAF", data = "ADVS", group = "TRT01A"))
  # the data step no longer drops them unseen
  x <- tfl_template_expand("mean_se", list(param = "SYSBP", data = "ADVS"))
  exprs <- unlist(lapply(x$data$steps, function(s) if (identical(s$step, "filter")) s$expr))
  expect_true("!is.na(AVISITN)" %in% exprs)
  expect_false(any(grepl("!is.na(AVAL) & !is.na(AVISITN)", exprs, fixed = TRUE)))
  w <- character()
  withCallingHandlers(suppressMessages(tfl_run_output("F-1", dir = d)),
                      warning = function(x) {
                        w <<- c(w, conditionMessage(x))
                        invokeRestart("muffleWarning")
                      })
  saf <- tflspec::adsl$USUBJID[tflspec::adsl$SAFFL == "Y"]
  n <- sum(is.na(vs$AVAL[k]) & vs$USUBJID[k] %in% saf)
  expect_gt(n, 0L)
  expect_true(any(grepl(sprintf("mean_se: %d records with a missing AVAL were left out", n), w, fixed = TRUE)))
})

test_that("waterfall: one bar a subject by default, each subject's smallest value, said", {
  skip_if_not_installed("dplyr")
  d <- data.frame(USUBJID = c("A", "A", "B", "B", "B", "C"),
                  AVISIT = c("W6", "W12", "W6", "W12", "W18", "W6"),
                  PCHG = c(-10, -10, 5, -20, -3, 30), stringsAsFactors = FALSE)
  expect_warning(p <- tpl_waterfall(d, value = PCHG),
                 "waterfall: one bar a subject, the smallest PCHG of each (the first of a tie): 3 of 6 records left out",
                 fixed = TRUE)
  b <- ggplot2::layer_data(p, 1L)
  expect_identical(nrow(b), 3L)
  # the largest first: C 30, A -10, B -20
  expect_identical(b$y, c(30, -10, -20))
  # a tie: the first record in the data
  kept <- p$layers[[1L]]$data
  expect_identical(kept$AVISIT[kept$USUBJID == "A"], "W6")
  # one record a subject: nothing said, nothing changed
  one <- d[c(1L, 4L, 6L), ]
  expect_no_warning(q <- tpl_waterfall(one, value = PCHG))
  expect_identical(ggplot2::layer_data(q, 1L)$y, c(30, -10, -20))
  # a subject with no value is not a bar (its record is no record)
  expect_identical(nrow(ggplot2::layer_data(suppressWarnings(tpl_waterfall(rbind(d, d[1L, ]), value = PCHG)), 1L)), 3L)
})

test_that("waterfall in a study: `where` is the report's own reduction; several records a subject are said", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dat <- list(adsl = tflspec::adsl, adtr = tflspec::adtr)
  e2e_study(d, dat, "F-1", "waterfall", list(param = "SUMDIAM", population = "SAF"))
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", "F-1.yml")), st)
  w <- character()
  p <- withCallingHandlers(tfl_content_run(ro, tflspec:::.contents_fig_data(ro, st, d)),
                           warning = function(x) {
                             w <<- c(w, conditionMessage(x))
                             invokeRestart("muffleWarning")
                           })
  saf <- tflspec::adsl$USUBJID[tflspec::adsl$SAFFL == "Y"]
  r <- tflspec::adtr[tflspec::adtr$PARAMCD == "SUMDIAM" & tflspec::adtr$USUBJID %in% saf & !is.na(tflspec::adtr$PCHG), ]
  expect_identical(nrow(ggplot2::layer_data(p, 1L)), length(unique(r$USUBJID)))
  expect_true(any(grepl("one bar a subject", w, fixed = TRUE)))
  # `where` that leaves several records a subject: said, one bar a record
  x <- tfl_template_expand("waterfall", list(param = "SUMDIAM", where = "PCHG < 1000"))
  codes <- unlist(lapply(x$data$steps, function(s) if (identical(s$step, "code")) s$code))
  expect_true(any(grepl("have more than one record, a bar for each", codes, fixed = TRUE)))
  expect_false(any(grepl("one bar a subject, the smallest", codes, fixed = TRUE)))
})

test_that("waterfall by response: a subject with no response is a grey bar with a key, Missing", {
  skip_if_not_installed("dplyr")
  df <- tflspec::adtr[tflspec::adtr$AVISIT == "WEEK 12" & !is.na(tflspec::adtr$PCHG), ]
  df <- df[!duplicated(df$USUBJID), ]
  bor <- tflspec::adrs[tflspec::adrs$PARAMCD == "BOR", c("USUBJID", "AVALC")]
  names(bor)[2L] <- "BOR"
  m <- merge(df, bor, by = "USUBJID", all.x = TRUE)
  expect_false(anyNA(m$BOR))
  fill_scale <- function(p) ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  p0 <- tpl_waterfall(m, by_response = TRUE)
  expect_false("Missing" %in% fill_scale(p0)$get_limits())
  m$BOR[2L] <- NA
  m$BOR[3L] <- ""
  p <- tpl_waterfall(m, by_response = TRUE)
  expect_true("Missing" %in% fill_scale(p)$get_limits())
  expect_identical(sum(ggplot2::layer_data(p, 1L)$fill == "grey80"), 2L)
  # the other bars: the response colours, as before
  expect_identical(sort(setdiff(unique(ggplot2::layer_data(p, 1L)$fill), "grey80")),
                   sort(setdiff(unique(ggplot2::layer_data(p0, 1L)$fill), "grey80")))
  # the report's own colours over it
  q <- tpl_waterfall(m, by_response = TRUE, plot = list(colours = list(Missing = "black")))
  expect_identical(sum(ggplot2::layer_data(q, 1L)$fill == "black"), 2L)
})

test_that("waterfall: a bar beyond the axis is cut off with a warning and its count", {
  skip_if_not_installed("dplyr")
  df <- tflspec::adtr[tflspec::adtr$AVISIT == "WEEK 12" & !is.na(tflspec::adtr$PCHG), ]
  df <- df[!duplicated(df$USUBJID), ]
  # (the package's data has a bar beyond the axis: cut here)
  df$PCHG <- pmin(df$PCHG, 100)
  expect_no_warning(tpl_waterfall(df))
  df$PCHG[1:2] <- c(140, 250)
  expect_warning(tpl_waterfall(df), "waterfall: 2 bars beyond the axis (-100 to 100) are cut off", fixed = TRUE)
})

test_that("forest_hr: subjects with no arm, no time or no censoring flag are said, with their count", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  skip_if_not_installed("dplyr")
  adtte <- e2e_pfs()
  ids <- tflspec::adsl$USUBJID[tflspec::adsl$SAFFL == "Y"]
  adtte <- adtte[adtte$USUBJID %in% ids, ]
  run <- function(tte) {
    d <- tempfile()
    on.exit(unlink(d, recursive = TRUE))
    e2e_study(d, list(adsl = tflspec::adsl, adtte = tte), "F-1", "forest_hr",
              list(param = "PFS", population = "SAF", group = "TRT01A", subgroups = c("SEX", "AGEGR1")))
    w <- character()
    withCallingHandlers(suppressMessages(tfl_run_output("F-1", dir = d)),
                        warning = function(x) {
                          w <<- c(w, conditionMessage(x))
                          invokeRestart("muffleWarning")
                        })
    w[grepl("^forest_hr", w)]
  }
  # clean: no word
  expect_identical(run(adtte), character())
  # a blank arm, a missing arm; a missing time, a missing flag, both
  bad <- adtte
  bad$TRT01A <- as.character(bad$TRT01A)
  bad$TRT01A[1L] <- ""
  bad$TRT01A[2L] <- NA
  bad$AVAL[3L] <- NA
  bad$CNSR[4L] <- NA
  bad$AVAL[5L] <- NA
  bad$CNSR[5L] <- NA
  # (a subject with no arm and no time: the arm's)
  bad$AVAL[1L] <- NA
  expect_identical(run(bad), c(
    "forest_hr: 2 subjects with an empty or missing TRT01A were left out of the Cox model",
    "forest_hr: 3 subjects with a missing AVAL or CNSR were left out of the Cox model"))
  # the code: the model is as it was, the words before it
  an <- tfl_fig_forest_analyses("ADTTE", "PFS", "SAFFL", "TRT01A", c("SEX", "AGEGR1"), time = "TIME", censor = "FLAG")$analyses
  expect_match(an$code[1L], "is.na(data$TIME) | is.na(data$FLAG)", fixed = TRUE)
  expect_match(an$code[1L], "cox <- survival::coxph(survival::Surv(TIME, FLAG == 0) ~ TRT01A, data = data)", fixed = TRUE)
  expect_no_match(an$code[2L], "warning(", fixed = TRUE)
})

test_that("forest_hr: a blank text arm is left out of every model, as the warning says", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  skip_if_not_installed("dplyr")
  adtte <- e2e_pfs()
  ids <- tflspec::adsl$USUBJID[tflspec::adsl$SAFFL == "Y"]
  adtte <- adtte[adtte$USUBJID %in% ids, ]
  adtte$TRT01A <- as.character(adtte$TRT01A)
  adtte$TRT01A[c(1L, 2L)] <- ""
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  e2e_study(d, list(adsl = tflspec::adsl, adtte = adtte), "F-1", "forest_hr",
            list(param = "PFS", population = "SAF", group = "TRT01A", subgroups = c("SEX", "AGEGR1")))
  st <- tfl_read_study_spec(d)
  w <- character()
  ard <- withCallingHandlers(suppressMessages(tfl_build_ard(st, d, output_id = "F-1", save = FALSE)),
                             warning = function(x) {
                               w <<- c(w, conditionMessage(x))
                               invokeRestart("muffleWarning")
                             })
  expect_true("forest_hr: 2 subjects with an empty or missing TRT01A were left out of the Cox model" %in% w)
  lv <- unlist(ard$variable_level[ard$variable == "TRT01A"])
  expect_false("" %in% lv)
  # the overall model and each subgroup's: the subjects with an arm
  n <- function(id) sum(unlist(ard$stat[ard$analysis_id == id & ard$stat_name == "n_obs"]))
  expect_equal(n("HR"), nrow(adtte) - 2L)
  expect_lte(n("HR_AGEGR1"), nrow(adtte) - 2L)
  expect_gte(n("HR_AGEGR1"), nrow(adtte) - 2L - sum(is.na(adtte$AGEGR1)))
})
