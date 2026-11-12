# The study helpers a figure reads an ARD with (#293): ard_value(),
# ard_stats(), ard_fingerprint() -- base R, as the generated programs call
# them, on a cards ARD as a table's ARD program saves it.

helper_env <- function() {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  e
}

km_ard <- function() {
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  d <- data.frame(time = c(5, 8, 12, 20, 25, 3, 9, 15, 30, 40),
                  status = c(1, 1, 0, 1, 0, 1, 0, 1, 1, 0),
                  ARM = rep(c("A", "B"), each = 5))
  fit <- survival::survfit(survival::Surv(time, status) ~ ARM, data = d)
  a <- cards::bind_ard(cardx::ard_survival_survfit(fit, probs = 0.5),
                       cardx::ard_survival_survfit(fit, times = c(0, 10, 20)),
                       .quiet = TRUE)
  a <- cards::apply_fmt_fun(a)
  dplyr::mutate(a, output_id = "T-1", analysis_id = "KM", .before = 1L)
}

test_that("ard_value() gives one statistic: its text, its value, or stops", {
  e <- helper_env()
  a <- km_ard()
  est <- unlist(a$stat[a$variable == "prob" & a$stat_name == "estimate" &
                         vapply(a$group1_level, as.character, "") == "A"])
  expect_identical(e$ard_value(a, "KM", "prob", "estimate", ARM = "A", level = 0.5),
                   unlist(a$stat_fmt[a$variable == "prob" & a$stat_name == "estimate" &
                                       vapply(a$group1_level, as.character, "") == "A"]))
  expect_identical(e$ard_value(a, "KM", "prob", "estimate", ARM = "A", level = 0.5, digits = 1),
                   formatC(est, format = "f", digits = 1))
  # no group: a row each, so not one
  expect_error(e$ard_value(a, "KM", "prob", "estimate", level = 0.5), "2 rows .* not one")
  expect_error(e$ard_value(a, "KM", "prob", "estimate", ARM = "C", level = 0.5), "0 rows")
  # a missing value: `na`
  a$stat[a$variable == "prob" & a$stat_name == "conf.high"] <- list(NA_real_)
  expect_identical(e$ard_value(a, "KM", "prob", "conf.high", ARM = "B", level = 0.5), "NE")
  expect_identical(e$ard_value(a, "KM", "prob", "conf.high", ARM = "B", level = 0.5, na = "-"), "-")
})

test_that("ard_stats() gives statistics as a data frame, a row per group and level", {
  e <- helper_env()
  a <- km_ard()
  nr <- e$ard_stats(a, "KM", "time", "n.risk", by = "ARM")
  expect_identical(names(nr), c("ARM", "time", "n.risk"))
  expect_identical(nr$ARM, rep(c("A", "B"), each = 3))
  expect_identical(nr$time, rep(c(0, 10, 20), 2))
  expect_identical(nr$n.risk[nr$time == 0], c(5, 5))
  two <- e$ard_stats(a, "KM", "time", c("estimate", "conf.low"))
  expect_identical(names(two), c("ARM", "time", "estimate", "conf.low"))
  expect_error(e$ard_stats(a, "KM", stats = "nope"), "no rows")
})

test_that("ard_fingerprint() reads ard_status.csv", {
  e <- helper_env()
  withr::local_dir(withr::local_tempdir())
  expect_identical(e$ard_fingerprint("T-1"), "")
  dir.create("output/ard", recursive = TRUE)
  utils::write.csv(data.frame(output_id = c("T-1", "T-2"), definition = c("abc", "")),
                   "output/ard/ard_status.csv", row.names = FALSE)
  expect_identical(e$ard_fingerprint("T-1"), "abc")
  expect_identical(e$ard_fingerprint("T-2"), "")
  expect_identical(e$ard_fingerprint("T-3"), "")
})

test_that("a figure's ARD pieces: the code reads `ard`, the checks look in it", {
  a <- km_ard()
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"),
                list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "ARM"),
                list(step = "ard_stats", name = "nr", analysis_id = "KM", variable = "time",
                     stats = "n.risk", by = "ARM")),
    plot = list(colour_by = "ARM"),
    layers = list(list(layer = "km_curve"),
                  list(layer = "ard_number", analysis_id = "KM", variable = "prob", level = 0.5,
                       stat = "estimate", group = "ARM = A", label = "Median (A): {value}")))
  code <- tfl_fig_design_code(d, "F-1", setup = TRUE, save = FALSE, name = "plot")
  expect_true(attr(code, "ard"))
  expect_false(attr(tfl_fig_design_code(tfl_fig_template("km_simple"), setup = TRUE), "ard"))
  expect_true("nr <- ard_stats(ard, \"KM\", variable = \"time\", stats = \"n.risk\", by = \"ARM\")" %in% code)
  # the number's text in the data part, under a name the plot uses
  i <- which(code == "ard_label_1 <- paste0(")
  expect_length(i, 1L)
  expect_identical(code[i + 1:3], c("  \"Median (A): \",",
    "  ard_value(ard, \"KM\", \"prob\", \"estimate\", ARM = \"A\", level = 0.5)", ")"))
  expect_true(any(grepl("label = ard_label_1", code, fixed = TRUE)))
  # a script of its own says it needs the ARD
  expect_true(any(startsWith(tfl_fig_design_code(d, "F-1"), "# Input ARD: ard")))
  expect_identical(nrow(tfl_check_fig_design(d, ard = a)), 0L)
  # the group as a YAML map is the same
  d2 <- d
  d2$layers[[2]]$group <- list(ARM = "A")
  expect_identical(tfl_fig_design_code(d2, "F-1"), tfl_fig_design_code(d, "F-1"))
  bad <- d
  bad$layers[[2]]$group <- "ARM = Z"
  bad$data[[3]]$analysis_id <- "HR"
  p <- tfl_check_fig_design(bad, ard = a)
  expect_identical(p$field, c("analysis_id", "group"))
  expect_match(p$problem[1], "no analysis HR in the ARD")
  expect_match(p$problem[2], "no ARM = Z")
  # no ARD given: not checked
  expect_identical(nrow(tfl_check_fig_design(bad)), 0L)
})

test_that("the ARD pieces run: the figure prints the number the ARD has", {
  skip_if_not_installed("ggsurvfit")
  a <- km_ard()
  e <- helper_env()
  e$ard <- a
  e$adtte <- data.frame(AVAL = c(5, 8, 12, 20, 25, 3, 9, 15, 30, 40),
                        CNSR = 1 - c(1, 1, 0, 1, 0, 1, 0, 1, 1, 0),
                        ARM = rep(c("A", "B"), each = 5))
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"),
                list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "ARM")),
    plot = list(colour_by = "ARM"),
    layers = list(list(layer = "km_curve"),
                  list(layer = "ard_number", analysis_id = "KM", variable = "prob", level = 0.5,
                       stat = "estimate", group = "ARM = B", label = "Median (B): {value}")))
  code <- tfl_fig_design_code(d, "F-1", save = FALSE)
  suppressMessages(eval(parse(text = code), envir = e))
  b <- ggplot2::ggplot_build(e$fig)
  labels <- unlist(lapply(b$data, function(x) x$label))
  expect_true(paste0("Median (B): ", e$ard_value(a, "KM", "prob", "estimate", ARM = "B", level = 0.5)) %in% labels)
})

test_that("an ard_number's label names other statistics of its address", {
  a <- km_ard()
  l <- list(layer = "ard_number", analysis_id = "KM", variable = "prob", level = 0.5,
            stat = "estimate", group = "ARM = A",
            label = "Median {value} (95% CI {conf.low}, {conf.high})", digits = 1)
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"),
                list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "ARM")),
    plot = list(colour_by = "ARM"),
    layers = list(list(layer = "km_curve"), l))
  code <- tfl_fig_design_code(d, "F-1", setup = TRUE, save = FALSE, name = "plot")
  # a text and a number a line, in the data part
  i <- which(code == "ard_label_1 <- paste0(")
  expect_length(i, 1L)
  expect_identical(code[i + 1L], "  \"Median \",")
  expect_match(code[i + 2L], "^  ard_value[(]ard, \"KM\", \"prob\", \"estimate\"")
  expect_identical(code[i + 3L], "  \" (95% CI \",")
  expect_match(code[i + 4L], "^  ard_value[(]ard, \"KM\", \"prob\", \"conf.low\"")
  expect_match(code[i + 6L], "^  ard_value[(]ard, \"KM\", \"prob\", \"conf.high\"")
  expect_identical(code[i + 7:8], c("  \")\"", ")"))
  expect_identical(nrow(tfl_check_fig_design(d, ard = a)), 0L)
  expect_identical(.ard_label_stats(l), c("estimate", "conf.low", "conf.high"))
  # a statistic the address does not have, named in the label
  d$layers[[2]]$label <- "{value} ({conf.lo})"
  p <- tfl_check_fig_design(d, ard = a)
  expect_match(p$problem, "no statistic conf.lo")
  # no placeholder: the number alone; one: as before
  d$layers[[2]]$label <- "the median"
  expect_true(any(grepl("ard_label_1 <- ard_value(ard, \"KM\", \"prob\", \"estimate\"",
                        tfl_fig_design_code(d, "F-1"), fixed = TRUE)))
  # the number drawn is the ARD's
  skip_if_not_installed("ggsurvfit")
  e <- helper_env()
  e$ard <- a
  e$adtte <- data.frame(AVAL = c(5, 8, 12, 20, 25, 3, 9, 15, 30, 40),
                        CNSR = 1 - c(1, 1, 0, 1, 0, 1, 0, 1, 1, 0),
                        ARM = rep(c("A", "B"), each = 5))
  d$layers[[2]]$label <- "Median {value} (95% CI {conf.low}, {conf.high})"
  suppressMessages(eval(parse(text = tfl_fig_design_code(d, "F-1", save = FALSE)), envir = e))
  labels <- unlist(lapply(ggplot2::ggplot_build(e$fig)$data, function(x) x$label))
  want <- paste0("Median ", e$ard_value(a, "KM", "prob", "estimate", ARM = "A", level = 0.5, digits = 1),
                 " (95% CI ", e$ard_value(a, "KM", "prob", "conf.low", ARM = "A", level = 0.5, digits = 1),
                 ", ", e$ard_value(a, "KM", "prob", "conf.high", ARM = "A", level = 0.5, digits = 1), ")")
  expect_true(want %in% labels)
})

test_that("drop_empty_groups(): a group with N = 0 and nothing else loses its rows", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  ard <- data.frame(group1 = "AVISIT", group1_level = I(list("BASELINE", "BASELINE", "WEEK 2", "WEEK 2")),
                    variable = "CHG", stat_name = c("N", "mean", "N", "mean"),
                    stat = I(list(0, NA_real_, 12, 1.5)), stringsAsFactors = FALSE)
  r <- e$drop_empty_groups(ard)
  expect_identical(unlist(r$group1_level), c("WEEK 2", "WEEK 2"))
  expect_identical(nrow(e$drop_empty_groups(ard[0L, ])), 0L)
  no_group <- data.frame(variable = "AGE", stat_name = "N", stat = I(list(0)))
  expect_identical(nrow(e$drop_empty_groups(no_group)), 1L)
})

test_that("keep_min_pct(): the terms of a percentage of subjects or more, in any / all / the Total column", {
  skip_if_not_installed("cards")
  e <- helper_env()
  saf <- adsl[adsl$SAFFL == "Y", ]
  ae <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y", ]
  stack <- function(...) cards::ard_stack_hierarchical(ae, variables = c(AEBODSYS, AEDECOD), ...,
                                                      denominator = saf, id = USUBJID, over_variables = TRUE)
  # (the arms' ARD with the Total column bound under it, as a template's program makes it)
  ard <- dplyr::bind_rows(stack(by = TRT01A), stack())
  lv <- function(x) vapply(x, function(v) if (length(v)) as.character(v[[1L]]) else NA_character_, "")
  # a term's percentage in each column: the Total's rows have no `by` group
  # (the term's SOC is group1), an arm's have TRT01A as group1 (its SOC group2)
  pct <- function(by) {
    r <- ard[ard$variable %in% "AEDECOD" & ard$stat_name == "p" &
               (ard$group1 %in% "TRT01A") == by, ]
    split(unlist(r$stat), lv(r$variable_level))
  }
  arm <- pct(TRUE)
  tot <- pct(FALSE)
  pts <- function(x) sort(unique(lv(x$variable_level[x$variable %in% "AEDECOD"])))
  soc <- function(x) sort(unique(lv(x$variable_level[x$variable %in% "AEBODSYS"])))
  expect_gt(length(arm), 10L)
  for (pct_ in c(5, 20, 40)) {
    p <- pct_ / 100
    k_any <- e$keep_min_pct(ard, pct_)
    expect_identical(pts(k_any), sort(names(arm)[vapply(arm, function(v) any(v >= p), NA)]))
    expect_identical(pts(e$keep_min_pct(ard, pct_, "all")),
                     sort(names(arm)[vapply(arm, function(v) all(v >= p), NA)]))
    expect_identical(pts(e$keep_min_pct(ard, pct_, "total")),
                     sort(names(tot)[vapply(tot, function(v) v >= p, NA)]))
  }
  # a term keeps its rows in every column, the Total's too; what is not a term stays
  k <- e$keep_min_pct(ard, 20)
  kept <- pts(k)
  expect_gt(nrow(k[k$variable %in% "AEDECOD" & !k$group1 %in% "TRT01A", ]), 0L)
  expect_identical(sum(k$variable %in% "AEDECOD"), sum(ard$variable %in% "AEDECOD" &
    lv(ard$variable_level) %in% kept))
  expect_identical(sum(!k$context %in% "hierarchical" | startsWith(k$variable, "..")),
                   sum(!ard$context %in% "hierarchical" | startsWith(ard$variable, "..")))
  # a SOC stays when a term below it passes (not on its own percentage)
  under <- k$variable %in% "AEDECOD" & k$group1 %in% "TRT01A"
  expect_identical(soc(k), sort(unique(lv(k$group2_level[under]))))
  r <- ard[ard$variable %in% "AEBODSYS" & ard$stat_name == "p" & ard$group1 %in% "TRT01A", ]
  soc_arm <- split(unlist(r$stat), lv(r$variable_level))
  own <- names(soc_arm)[vapply(soc_arm, function(v) any(v >= 0.2), NA)]
  expect_gt(length(setdiff(own, soc(k))), 0L)
  # "total" on an ARD with no Total column stops, naming the way out;
  # no records, nothing to drop
  expect_error(e$keep_min_pct(stack(by = TRT01A), 5, "total"), "Total column.*\"any\"")
  expect_identical(e$keep_min_pct(ard[0L, ], 5), ard[0L, ])
})

test_that("set_levels() on a column that holds the list's labels already keeps them (a data made from another)", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  cl <- c(LOW = "Low", NORMAL = "Normal", HIGH = "High")
  d <- data.frame(WORST = c("HIGH", "LOW", NA), stringsAsFactors = FALSE)
  once <- e$set_levels(d, WORST = cl)
  expect_identical(levels(once$WORST), c("Low", "Normal", "High"))
  twice <- e$set_levels(once, WORST = cl)
  expect_identical(twice$WORST, once$WORST)
  expect_error(e$set_levels(data.frame(WORST = "Mid"), WORST = cl), "WORST has values its code list does not")
})

test_that("set_levels(): a blank text value is missing, not a value the list lacks; an unknown one still stops", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  cl <- c(LOW = "Low", NORMAL = "Normal", HIGH = "High")
  d <- data.frame(WORST = c("HIGH", "", NA, "  ", "LOW"), stringsAsFactors = FALSE)
  r <- e$set_levels(d, WORST = cl)
  expect_identical(levels(r$WORST), c("Low", "Normal", "High"))
  expect_identical(is.na(r$WORST), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(as.character(r$WORST[c(1L, 5L)]), c("High", "Low"))
  expect_error(e$set_levels(data.frame(WORST = c("", "Mid")), WORST = cl),
               "WORST has values its code list does not: \"Mid\"", fixed = TRUE)
  # a factor with the labels already still works, its blank staying missing
  f <- e$set_levels(r, WORST = cl)
  expect_identical(f$WORST, r$WORST)
})

test_that("tag_ard() refuses what is not a cards ARD (a custom analysis's plain data frame)", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  expect_error(e$tag_ard(data.frame(AEBODSYS = "x", p.value = 0.5), "T", "FISHER"),
               "analysis FISHER: its code must return a cards ARD.*columns AEBODSYS, p.value")
  expect_error(e$tag_ard(0.5, "T", "FISHER"), "an object of class numeric")
  ok <- e$tag_ard(data.frame(variable = "x", stat_name = "n", stat = 1), "T", "A")
  expect_identical(ok$output_id, "T")
})
