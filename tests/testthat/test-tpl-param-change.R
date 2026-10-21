# param_change: parameters' value and change from baseline at each visit

pc_args <- list(population = "SAF", data = "ADLB", params = c("ALT", "AST"),
                visits = c("BASELINE", "CYCLE 2 DAY 1", "CYCLE 9 DAY 1", "CYCLE 10 DAY 1"),
                param_order = NULL)

# the ARD as the study's program makes it (its analysis data's steps)
pc_ard <- function(x) {
  adsl_saf <- adsl[adsl$SAFFL == "Y", ]
  d <- x$ard$analysis_data
  e <- function(s) eval(parse(text = s))
  adlb_saf <- adlb[adlb$USUBJID %in% adsl_saf$USUBJID, ]
  adlb_saf <- adlb_saf[eval(parse(text = d[[2L]]$where), adlb_saf), ]
  if (length(d[[2L]]$derive)) adlb_saf <- e(sprintf("dplyr::mutate(adlb_saf, %s)",
                                                    gsub(" | ", ", ", d[[2L]]$derive, fixed = TRUE)))
  chg <- adlb_saf[!is.na(adlb_saf$CHG), ]
  if (length(d[[3L]]$derive)) chg$AVISIT <- droplevels(chg$AVISIT)
  st <- ~ cards::continuous_summary_fns(c("N", "mean", "sd", "median", "min", "max"))
  cards::bind_ard(
    cards::ard_tabulate(adsl_saf, variables = TRT01A),
    cards::ard_summary(adlb_saf, by = c(TRT01A, PARAM, AVISIT), variables = AVAL, statistic = st),
    cards::ard_summary(chg, by = c(TRT01A, PARAM, AVISIT), variables = CHG, statistic = st))
}
pc_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
pc_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
}

test_that("expanded: the records kept, the visits in their order, the change's own data", {
  x <- tfl_template_expand("param_change", pc_args)
  d <- x$ard$analysis_data
  expect_identical(vapply(d, `[[`, "", "id"), c("adsl_saf", "adlb_saf", "adlb_saf_chg"))
  expect_match(d[[2L]]$where, "PARAMCD %in% c(\"ALT\", \"AST\")", fixed = TRUE)
  expect_match(d[[2L]]$where, "AVISIT %in% c(\"BASELINE\"", fixed = TRUE)
  expect_identical(as.character(d[[2L]]$derive), "AVISIT = reorder(factor(AVISIT), AVISITN)")
  expect_identical(as.character(d[[3L]]$where), "!is.na(CHG)")
  expect_identical(as.character(d[[3L]]$derive), "AVISIT = droplevels(AVISIT)")
  an <- x$ard$analyses
  expect_identical(vapply(an, `[[`, "", "id"), c("GROUPN", "VALUE", "CHANGE"))
  expect_identical(an[[2L]]$by, c("TRT01A", "PARAM", "AVISIT"))
  # no where of its own unless given: ANL01FL is not every study's
  y <- tfl_template_expand("param_change", list(population = "SAF", data = "ADVS"))
  expect_null(y$ard$analysis_data[[2L]]$where)
  expect_match(y$ard$analysis_data[[2L]]$derive, "PARAM = reorder(factor(PARAM), PARAMN)",
               fixed = TRUE)
  z <- tfl_template_expand("param_change", list(population = "SAF", data = "ADVS",
                                                where = "ANL01FL == \"Y\"", visit_order = NULL))
  expect_identical(as.character(z$ard$analysis_data[[2L]]$where), "ANL01FL == \"Y\"")
  expect_null(z$ard$analysis_data[[3L]]$derive)
  fns <- function(x) vapply(x$content, `[[`, "", "fn")
  expect_true("plan_col_header" %in% fns(x))
  r <- tfl_template_expand("param_change", c(pc_args, layout = "rows"))
  expect_true(all(c("plan_sort", "plan_col_header") %in% fns(r)))
})

test_that("as columns: each arm's value and change; the baseline's change empty", {
  pc_skip()
  x <- tfl_template_expand("param_change", pc_args)
  t <- pc_tab(do.call(tpl_param_change, c(list(pc_ard(x)), pc_args)))
  expect_identical(setdiff(names(t), c("group1", "group2", "label")),
                   c("Drug X____Value", "Drug X____Change from baseline",
                     "Control____Value", "Control____Change from baseline"))
  base <- t$group2 %in% "BASELINE"
  expect_true(all(is.na(t[["Drug X____Change from baseline"]][base])))
  expect_false(anyNA(t[["Drug X____Value"]][base]))
  # the visits by AVISITN: cycle 10 after cycle 9
  expect_identical(unique(t$group2), pc_args$visits)
})

test_that("as rows: the value's rows, then the change's; none at baseline", {
  pc_skip()
  a <- c(pc_args, layout = "rows")
  x <- tfl_template_expand("param_change", a)
  t <- pc_tab(do.call(tpl_param_change, c(list(pc_ard(x)), a)))
  blk <- unique(paste(t$group1, t$group2, t$group3, sep = " / "))
  alt <- "Alanine Aminotransferase (U/L)"
  expect_identical(blk[1:5], paste(alt, c("BASELINE / Value", "CYCLE 2 DAY 1 / Value",
                                          "CYCLE 2 DAY 1 / Change from baseline",
                                          "CYCLE 9 DAY 1 / Value",
                                          "CYCLE 9 DAY 1 / Change from baseline"), sep = " / "))
  expect_identical(names(t), c("group1", "group2", "group3", "label", "Drug X", "Control"))
})

test_that("the arm and its N over its two columns", {
  pc_skip()
  x <- tfl_template_expand("param_change", pc_args)
  p <- do.call(tpl_param_change, c(list(pc_ard(x)), pc_args))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  suppressWarnings(suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), rtfreporter::plan_apply(p)), f)))
  r <- paste(readLines(f), collapse = "\n")
  n <- sum(adsl$SAFFL == "Y" & adsl$TRT01A == "Drug X")
  expect_match(r, sprintf("Drug X\\line (N=%d)", n), fixed = TRUE)
  expect_false(grepl("N=NA", r, fixed = TRUE))
})

test_that("A (the call) = B (the items as the report's content)", {
  pc_skip()
  for (lay in c("cols", "rows")) {
    a <- c(pc_args, layout = lay)
    x <- tfl_template_expand("param_change", a)
    ard <- pc_ard(x)
    ta <- pc_tab(do.call(tpl_param_change, c(list(ard), a)))
    b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content),
                                              rtfreporter::normalize_ard(ard)))
    expect_identical(pc_tab(b), ta, info = lay)
  }
})

test_that("in a study: the ARD program runs and the table is made from it", {
  skip_on_cran()
  pc_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "PC-1")
  f <- file.path(d, "spec", "outputs", "T-9-3.yml")
  tfl_write_output(tfl_output("T-9-3", "table", population = "SAF", label = "Liver tests",
                              extends = "defaults",
                              template = list(name = "param_change",
                                              args = c(pc_args[-1L], list(population = "SAF")))),
                   f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-9-3", dir = d, save = FALSE))
  t <- pc_tab(suppressMessages(tfl_content_run(tfl_resolve_output(tfl_read_output(f), st), ard)))
  expect_true(nrow(t) > 0L)
  expect_identical(unique(t$group2), pc_args$visits)
})

test_that("vs_change's table (removed) from param_change: a baseline block, its labels and cells", {
  skip_on_cran()
  pc_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "advs")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "VS-1")
  pc <- list(population = "SAF", data = "ADVS", params = "SYSBP", visits = "CYCLE 9 DAY 1",
             by = "TRT01A", layout = "rows", param_order = NULL, baseline = TRUE,
             baseline_label = "Baseline (mmHg)", value_label = "Week 24 (mmHg)",
             change_label = "Change from baseline (mmHg)",
             statistics = c("N", "mean", "sd", "se", "mean_lcl", "mean_ucl", "median", "min", "max"),
             cells = list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})", SE = "{se:.2f}",
                          `95% CI of the mean` = "{mean_lcl:.1f}, {mean_ucl:.1f}",
                          Median = "{median:.1f}", `Min, Max` = "{min:.0f}, {max:.0f}"))
  o <- tfl_output("T-P", "table", population = "SAF", extends = "defaults",
                  template = list(name = "param_change", args = pc))
  f <- file.path(d, "spec", "outputs", "T-P.yml")
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, save = FALSE))
  p <- pc_tab(suppressMessages(tfl_content_run(tfl_resolve_output(tfl_read_output(f), st), ard)))
  p[] <- lapply(p, as.character)
  # vs_change's table on the package data, as its template made it before
  # it was removed (0.0.24.9193)
  v <- data.frame(
    group = rep(c("Baseline (mmHg)", "Week 24 (mmHg)", "Change from baseline (mmHg)"), each = 6L),
    label = rep(c("n", "Mean (SD)", "SE", "95% CI of the mean", "Median", "Min, Max"), 3L),
    Control = c("23", "126.6 (14.60)", "3.05", "120.3, 132.9", "126.0", "99, 150",
                "23", "124.4 (16.56)", "3.45", "117.3, 131.6", "121.0", "95, 157",
                "23", "-2.1 (8.40)", "1.75", "-5.8, 1.5", "-1.0", "-25, 13"),
    `Drug X` = c("39", "130.8 (14.72)", "2.36", "126.0, 135.5", "131.0", "104, 174",
                 "39", "135.7 (15.76)", "2.52", "130.6, 140.8", "138.0", "106, 168",
                 "39", "4.9 (9.71)", "1.56", "1.8, 8.1", "6.0", "-17, 30"),
    check.names = FALSE, stringsAsFactors = FALSE)
  # the same rows and cells; param_change's blocks of the parameter and the
  # visit (one each here) are what vs_change left to the title
  expect_identical(unique(p$group1), "Systolic Blood Pressure (mmHg)")
  expect_identical(unique(p$group2), "CYCLE 9 DAY 1")
  p <- data.frame(group = p$group3, p[setdiff(names(p), c("group1", "group2", "group3"))],
                  check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(p, v)
})
