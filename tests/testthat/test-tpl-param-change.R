# param_change: parameters' value and change from baseline at each visit

pc_args <- list(population = "SAF", data = "ADLB", params = c("ALT", "AST"),
                visits = c("BASELINE", "CYCLE 2 DAY 1", "CYCLE 9 DAY 1", "CYCLE 10 DAY 1"),
                param_order = NULL)

# the ARD as the study's program makes it (its analysis data's steps)
pc_ard <- function(x) {
  eval(parse(text = tfl_helpers_code()))  # reorder_by()
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
  expect_identical(as.character(d[[2L]]$derive), "AVISIT = reorder_by(AVISIT, AVISITN)")
  expect_identical(as.character(d[[3L]]$where), "!is.na(CHG)")
  expect_identical(as.character(d[[3L]]$derive), "AVISIT = droplevels(AVISIT)")
  an <- x$ard$analyses
  expect_identical(vapply(an, `[[`, "", "id"), c("GROUPN", "VALUE", "CHANGE"))
  expect_identical(an[[2L]]$by, c("TRT01A", "PARAM", "AVISIT"))
  # no where of its own unless given: ANL01FL is not every study's
  y <- tfl_template_expand("param_change", list(population = "SAF", data = "ADVS"))
  expect_null(y$ard$analysis_data[[2L]]$where)
  expect_match(y$ard$analysis_data[[2L]]$derive, "PARAM = reorder_by(PARAM, PARAMN)",
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

test_that("the digits: the same as fixed cells, and a parameter's own by plan_digits(PARAM = )", {
  pc_skip()
  x <- tfl_template_expand("param_change", pc_args)
  ard <- pc_ard(x)
  p <- do.call(tpl_param_change, c(list(ard), pc_args))
  t <- pc_tab(p)
  # the template's digits: what its cells used to fix
  fixed <- list(n = "{N:.0f}", `Mean (SD)` = "{mean:.1f} ({sd:.2f})",
                Median = "{median:.1f}", `Min, Max` = "{min:.1f}, {max:.1f}")
  expect_identical(t, pc_tab(do.call(tpl_param_change, c(list(ard), pc_args, list(cells = fixed)))))
  # a parameter's digits by its value, as the table holds it (PARAM's label)
  alt <- unique(as.character(adlb$PARAM[adlb$PARAMCD == "ALT"]))
  d <- pc_tab(p |> rtfreporter::plan_digits(PARAM = stats::setNames(
    list(c(mean = 2, sd = 3, min = 0, max = 0)), alt)))
  if (identical(d, t)) skip("rtfreporter without digits by a row key's value (#794)")
  m <- t$label == "Mean (SD)"
  r <- t$label == "Min, Max"
  a <- t$group1 == alt
  expect_match(d[["Drug X____Value"]][m & a], "^[0-9]+\\.[0-9]{2} \\([0-9]+\\.[0-9]{3}\\)$")
  expect_match(d[["Drug X____Value"]][r & a], "^[0-9]+, [0-9]+$")
  # the other parameter keeps the template's
  expect_identical(d[!a, ], t[!a, ])
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
  # a code list on the visits puts the baseline level back on the change's
  # records: its change cells stay empty all the same (no "0" for n)
  o <- tfl_read_output(f)
  o$codelists <- list(AVISIT = lapply(pc_args$visits, function(v) list(value = v, label = tools::toTitleCase(tolower(v)))))
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  code <- paste(tfl_ard_code(st, output_id = "T-9-3", save = FALSE), collapse = "\n")
  expect_match(code, "drop_empty_groups()", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-9-3", dir = d, save = FALSE))
  t <- pc_tab(suppressMessages(tfl_content_run(tfl_resolve_output(tfl_read_output(f), st), ard)))
  chg <- grep("Change", names(t), value = TRUE)
  expect_true(length(chg) > 0L)
  expect_true(all(is.na(unlist(t[t$group2 == "Baseline", chg]))))
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
  # (the arms in their N order now: Drug X, then Control)
  v <- v[c("group", "label", "Drug X", "Control")]
  # the same rows and cells; param_change's blocks of the parameter and the
  # visit (one each here) are what vs_change left to the title
  expect_identical(unique(p$group1), "Systolic Blood Pressure (mmHg)")
  expect_identical(unique(p$group2), "CYCLE 9 DAY 1")
  p <- data.frame(group = p$group3, p[setdiff(names(p), c("group1", "group2", "group3"))],
                  check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(p, v)
})

test_that("cells as items ({label, template}); a page a parameter, named in the titles", {
  x <- tfl_template_expand("param_change", c(pc_args, list(cells = list(
    list(label = "n", template = "{N:.0f}"), list(label = "Mean", template = "{mean:.1f}")))))
  cl <- Filter(function(i) i$fn == "plan_cells", x$content)[[1L]]$args$continuous
  expect_identical(cl, list(n = "{N:.0f}", Mean = "{mean:.1f}"))
  expect_error(tfl_template_expand("param_change", c(pc_args, list(cells = list(
    list(name = "n", cell = "{N}"))))), "label: , template:")
  pg <- Filter(function(i) i$fn == "plan_paginate_rows", x$content)[[1L]]$args
  expect_identical(pg$page_by, "group1")
  expect_identical(pg$heading, "Parameter: {value}")
  y <- tfl_template_expand("param_change", c(pc_args, list(param_page = FALSE)))
  pg <- Filter(function(i) i$fn == "plan_paginate_rows", y$content)[[1L]]$args
  expect_null(pg$page_by)
  expect_false("plan_hide" %in% vapply(y$content, `[[`, "", "fn"))
})

test_that("a page a parameter: the parameter's name in the titles, once a page", {
  pc_skip()
  x <- tfl_template_expand("param_change", pc_args)
  p <- do.call(tpl_param_change, c(list(pc_ard(x)), pc_args))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  suppressWarnings(suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), rtfreporter::plan_apply(p)), f)))
  r <- readLines(f)
  expect_length(grep("Parameter: Alanine Aminotransferase (U/L)", r, fixed = TRUE), 1L)
  expect_length(grep("Parameter: Aspartate Aminotransferase (U/L)", r, fixed = TRUE), 1L)
  expect_length(grep("Alanine Aminotransferase (U/L)", r, fixed = TRUE), 1L)
})

# the stub's heading (#641): param_change's `heading`, and a report's own plan_stub
pch_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
# a report extending the study's defaults, resolved, run on the ARD the program makes
pch_run <- function(content = list(), args = list(), extends = "defaults", template = TRUE) {
  a <- utils::modifyList(pc_args, args)
  o <- tfl_output("T-PCH", "table", population = "SAF", extends = extends, content = content,
                  template = if (template) list(name = "param_change", args = a))
  ro <- tfl_resolve_output(o, pch_study())
  ard <- pc_ard(tfl_template_expand("param_change", a))
  p <- suppressMessages(tfl_content_run(ro, ard))
  f <- tempfile(fileext = ".rtf")
  suppressWarnings(suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), rtfreporter::plan_apply(p)), f)))
  list(ro = ro, tab = pc_tab(p), rtf = paste(readLines(f), collapse = "\n"),
       code = paste(tfl_content_code(ro), collapse = "\n"))
}
pch_hdr <- function(args, ...) {
  x <- tfl_template_expand("param_change", utils::modifyList(pc_args, args))
  h <- Filter(function(i) i$fn == "plan_col_header", x$content)[[1L]]$args$header
  h
}

test_that("heading: the stub's cell in the header, on the second row with cols, the only row with rows", {
  pc_skip()
  # no heading: the header as it was
  for (lay in c("cols", "rows")) {
    h <- pch_hdr(list(layout = lay))
    expect_false("row_label" %in% unlist(lapply(h, `[[`, "cols")))
    expect_length(h, if (lay == "cols") 2L else 1L)
  }
  hc <- pch_hdr(list(heading = "Visit / Statistic"))
  expect_identical(hc[[2L]], list(line = "2", cols = "row_label", text = "Visit / Statistic"))
  expect_length(hc, 3L)
  hr <- pch_hdr(list(layout = "rows", heading = "Visit / Statistic"))
  expect_identical(hr[[1L]], list(line = "1", cols = "row_label", text = "Visit / Statistic"))
  expect_length(hr, 2L)
  # built: in the printed header, in the stub's cell of the table and the RTF
  for (lay in c("cols", "rows")) {
    r <- pch_run(args = list(layout = lay, heading = "Visit / Statistic"))
    expect_match(r$rtf, "Visit / Statistic", fixed = TRUE)
    expect_match(r$code, "Visit / Statistic", fixed = TRUE)
    # (the same table, the stub column's header the only difference)
    r0 <- pch_run(args = list(layout = lay))
    expect_false(grepl("Visit / Statistic", r0$rtf, fixed = TRUE))
    expect_identical(r$tab, r0$tab)
  }
  p <- do.call(tpl_param_change, c(list(pc_ard(tfl_template_expand("param_change", pc_args))),
                                   pc_args, list(heading = "Visit / Statistic")))
  expect_s3_class(p, "table_plan")
})

test_that("a report's plan_stub keeps the stub's name from the layers below (widths row_label builds)", {
  pc_skip()
  w <- list(tfl_item("plan_columns", list(widths = list(row_label = 2, .values = 1))))
  st <- function(r) Filter(function(it) it$fn == "plan_stub", r$ro$content)
  # the issue's draft: the report's plan_stub with a heading, and widths by the stub's name
  r <- pch_run(c(list(tfl_item("plan_stub", list(heading = "Visit / Statistic"))), w))
  expect_length(st(r), 1L)
  expect_identical(st(r)[[1L]]$args$name, "row_label")
  expect_identical(st(r)[[1L]]$args$heading, "Visit / Statistic")
  # (it builds: the widths name the stub column `row_label`; the plan was applied)
  expect_true(nrow(r$tab) > 0L)
  # a name the report writes wins: widths name `other`, and `row_label` is no column
  wo <- list(tfl_item("plan_columns", list(widths = list(other = 2, .values = 1))))
  r3 <- pch_run(c(list(tfl_item("plan_stub", list(name = "other", heading = "H"))), wo))
  expect_identical(st(r3)[[1L]]$args$name, "other")
  expect_error(pch_run(c(list(tfl_item("plan_stub", list(name = "other", heading = "H"))), w)),
               "names 'row_label', not a column")
})

test_that("the template's heading is printed, the report's plan_stub heading is not (the template's header wins)", {
  pc_skip()
  w <- list(tfl_item("plan_columns", list(widths = list(row_label = 2, .values = 1))))
  r <- pch_run(c(list(tfl_item("plan_stub", list(heading = "Own heading"))), w),
               args = list(heading = "Visit / Statistic"))
  expect_match(r$rtf, "Visit / Statistic", fixed = TRUE)
  expect_false(grepl("Own heading", r$rtf, fixed = TRUE))
})

test_that("a table with its own plan_stub and no name takes the layers' name; extends: none does not", {
  pc_skip()
  st <- function(r) Filter(function(it) it$fn == "plan_stub", r$content)
  base <- list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))))
  ro <- tfl_resolve_output(tfl_output("T-N", "table", population = "SAF", extends = "defaults",
                                      content = list(tfl_item("plan_stub", list(indent = 2L)))),
                           pch_study())
  expect_identical(st(ro)[[1L]]$args, list(indent = 2L, name = "row_label"))
  # the layers' item as it is, the report's other arguments kept; only `name` comes over
  expect_false("before" %in% names(st(ro)[[1L]]$args))
  # built: the study's header names `row_label` (it failed on the stub's old name), and the
  # program says the name
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- rtfreporter::normalize_ard(cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX))))
  ro <- tfl_resolve_output(tfl_output("T-N", "table", population = "SAF", extends = "defaults",
                                      content = c(base, list(tfl_item("plan_stub", list(indent = 2L))))),
                           pch_study())
  p <- suppressMessages(tfl_content_run(ro, ard))
  f <- withr::local_tempfile(fileext = ".rtf")
  suppressWarnings(suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), rtfreporter::plan_apply(p)), f)))
  expect_match(paste(readLines(f), collapse = "\n"), "Characteristic\\cell", fixed = TRUE)
  expect_match(paste(tfl_content_code(ro), collapse = "\n"),
               "plan_stub(indent = 2, name = \"row_label\")", fixed = TRUE)
  # no plan_stub below (extends: none): as written
  rn <- tfl_resolve_output(tfl_output("T-N", "table", population = "SAF", extends = "none",
                                      content = list(tfl_item("plan_stub", list(indent = 2L)))),
                           pch_study())
  expect_identical(st(rn)[[1L]]$args, list(indent = 2L))
  # a report with no plan_stub: the layers' item, as before
  r0 <- tfl_resolve_output(tfl_output("T-N", "table", population = "SAF", extends = "defaults",
                                      content = list(tfl_item("plan_paginate_rows", list(max_rows = 9L)))),
                           pch_study())
  expect_identical(st(r0)[[1L]]$args, list(name = "row_label", before = TRUE))
})

test_that("heading with no study plan_stub (extends: none, tpl_param_change()) builds: the template names the stub", {
  pc_skip()
  for (lay in c("cols", "rows")) {
    # no heading: no plan_stub of the template's (the expansion as it was)
    x0 <- tfl_template_expand("param_change", utils::modifyList(pc_args, list(layout = lay)))
    expect_false("plan_stub" %in% vapply(x0$content, `[[`, "", "fn"))
    r <- pch_run(args = list(layout = lay, heading = "Visit / Statistic"), extends = "none")
    expect_match(r$rtf, "Visit / Statistic", fixed = TRUE)
    a <- c(pc_args, layout = lay, heading = "Visit / Statistic")
    p <- do.call(tpl_param_change, c(list(pc_ard(tfl_template_expand("param_change", a))), a))
    expect_identical(pc_tab(p), r$tab)
  }
  # a study's own plan_stub wins, the template supplies only the name
  s <- pch_study()
  s$defaults$content <- lapply(s$defaults$content, function(it) {
    if (it$fn == "plan_stub") it$args <- list(name = "stub_x", indent = 3L)
    it
  })
  o <- tfl_output("T-PCH", "table", population = "SAF", extends = "defaults",
                  template = list(name = "param_change",
                                  args = utils::modifyList(pc_args, list(heading = "H"))))
  ro <- tfl_resolve_output(o, s)
  st <- Filter(function(it) it$fn == "plan_stub", ro$content)
  expect_length(st, 1L)
  expect_identical(st[[1L]]$args$indent, 3L)
  expect_identical(st[[1L]]$args$name, "stub_x")
})
