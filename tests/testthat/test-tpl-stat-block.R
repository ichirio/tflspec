# The layout template stat_block: one record's statistics over several rows
# and columns (plan_stat_groups()); orr's conf_level

sb_ard <- function() {
  adsl <- tflspec::adsl[tflspec::adsl$ITTFL == "Y", ]
  bor <- tflspec::adrs[tflspec::adrs$PARAMCD == "BOR" & tflspec::adrs$USUBJID %in% adsl$USUBJID, ]
  bor$RESP <- factor(ifelse(bor$AVALC %in% c("CR", "PR"), "Y", "N"), levels = c("Y", "N"))
  cards::bind_ard(
    cards::ard_tabulate(adsl, variables = "TRT01P"),
    cards::ard_strata(bor, .by = TRT01P, .f = function(.x) {
      cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y")
    }))
}
sb_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
sb_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("rtfreporter")
}
sb_cells <- list(N = "{N}", "ORR(%) 95%CI" = c("{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}"))

test_that("expanded: the groups from the cells' tokens, a block's lines as cell_rows()", {
  x <- tfl_template_expand("stat_block", list(by = "TRT01P", variable = "RESP", cells = sb_cells))
  expect_null(x$ard)   # the analyses are the report's own
  fns <- vapply(x$content, `[[`, "", "fn")
  expect_identical(fns, c("table_plan", "plan_stat_groups", "plan_cells", "plan_col_header",
                          "plan_row_group"))
  it <- function(fn) x$content[[match(fn, fns)]]$args
  expect_identical(it("table_plan")$cols, c("TRT01P", ".stat_group"))
  expect_identical(it("plan_stat_groups")[c("N", "ORR(%) 95%CI", ".variables")],
                   list(N = "N", "ORR(%) 95%CI" = c("n", "estimate", "conf.low", "conf.high"),
                        .variables = "RESP"))
  # N beside the rate's first line
  expect_match(it("plan_cells")$N, "cell_rows(\"1\" = \"{N}\")", fixed = TRUE)
  expect_match(it("plan_cells")[["ORR(%) 95%CI"]], "\"2\" = \"{conf.low:.1f%}, {conf.high:.1f%}\"",
               fixed = TRUE)
  # the YAML's records {label, template} read the same
  y <- tfl_template_expand("stat_block", list(by = "TRT01P", variable = "RESP", cells = list(
    list(label = "N", template = "{N}"),
    list(label = "ORR(%) 95%CI", template = list("{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}")))))
  expect_identical(y$content, x$content)
  # one line each, as rows: no cell_rows(), the groups a row key
  r <- tfl_template_expand("stat_block", list(variable = "RESP", direction = "rows",
                                              cells = list(N = "{N}", "ORR (%)" = "{n} ({estimate:.1f%})")))
  rp <- r$content[[1L]]$args
  expect_identical(rp$rows, list(group = "variable", stat = ".stat_group"))
  expect_identical(r$content[[3L]]$args, list(N = "{N}", "ORR (%)" = "{n} ({estimate:.1f%})"))
  expect_error(tfl_template_expand("stat_block", list(variable = "RESP")), "cells")
})

test_that("tpl_stat_block(): each arm's N and ORR (CI) side by side, or under each other", {
  sb_skip()
  ard <- sb_ard()
  t <- sb_tab(tpl_stat_block(ard, by = "TRT01P", variable = "RESP", cells = sb_cells))
  expect_identical(names(t), c("group", "Drug X____N", "Drug X____ORR(%) 95%CI",
                               "Control____N", "Control____ORR(%) 95%CI"))
  expect_identical(t[["Drug X____N"]], c("82", NA))
  expect_identical(t[["Drug X____ORR(%) 95%CI"]], c("32 (39.0)", "27.9, 50.2"))
  expect_identical(t[["Control____ORR(%) 95%CI"]], c("14 (17.9)", "8.8, 27.1"))
  r <- sb_tab(tpl_stat_block(ard, by = "TRT01P", variable = "RESP", direction = "rows", cells = sb_cells))
  expect_identical(r[["Drug X"]], c("82", "32 (39.0)", "27.9, 50.2"))
  expect_identical(r[["stat"]], c("N", "ORR(%) 95%CI", "ORR(%) 95%CI"))
  # the headings, spaces and symbols as they are, in the RTF
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  rtfreporter::generate_rtfreport(
    rtfreporter::rtf_document() |>
      rtfreporter::rtf_tables(tpl_stat_block(ard, by = "TRT01P", variable = "RESP", cells = sb_cells)),
    f, overwrite = TRUE)
  rtf <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_true(grepl("ORR(%) 95%CI", rtf, fixed = TRUE))
})

test_that("in a study: the report's own ARD built, the table made from it", {
  skip_on_cran()
  sb_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adrs")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SB-1")
  o <- tfl_output("T-SB", "table", population = "ITT", extends = "defaults",
    ard = list(
      analysis_data = list(list(id = "adrs_bor", from = "ADRS", population = "ITT",
                                where = tfl_r("PARAMCD == \"BOR\""),
                                derive = tfl_r("RESP = AVALC %in% c(\"CR\", \"PR\")"))),
      analyses = list(list(id = "ORR", method = "proportion_ci", data = "adrs_bor", by = "TRT01P",
                           variables = "RESP"))),
    template = list(name = "stat_block", args = list(by = "TRT01P", variable = "RESP", cells = sb_cells)))
  f <- file.path(d, "spec", "outputs", "T-SB.yml")
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-SB", dir = d, save = FALSE))
  t <- sb_tab(suppressMessages(tfl_content_run(tfl_resolve_output(tfl_read_output(f), st), ard)))
  expect_identical(t[["Drug X____ORR(%) 95%CI"]], c("32 (39.0)", "27.9, 50.2"))
  expect_identical(t[["Control____ORR(%) 95%CI"]], c("14 (17.9)", "8.8, 27.1"))
})

test_that("orr: conf_level, the CI's level and its row's label; 0.95 as before", {
  x <- tfl_template_expand("orr", list(conf_level = 0.9))
  expect_identical(as.character(x$ard$analyses[[2L]]$args), "method = \"clopper-pearson\", conf.level = 0.9")
  cl <- Filter(function(i) identical(i$fn, "plan_cells"), x$content)[[1L]]$args$proportion_ci
  expect_identical(names(cl), c("n (%)", "90% CI"))
  y <- tfl_template_expand("orr", list())
  expect_identical(as.character(y$ard$analyses[[2L]]$args), "method = \"clopper-pearson\"")
  expect_identical(names(Filter(function(i) identical(i$fn, "plan_cells"), y$content)[[1L]]$args$proportion_ci),
                   c("n (%)", "95% CI"))
})

test_that("orr(stat_layout = \"cols\"): each arm's rate and CI side by side, the BOR counts in the first", {
  skip_on_cran()
  sb_skip()
  skip_if_not_installed("haven")
  x <- tfl_template_expand("orr", list(stat_layout = "cols"))
  fns <- vapply(x$content, `[[`, "", "fn")
  expect_true("plan_stat_groups" %in% fns)
  expect_identical(x$content[[match("plan_stat_groups", fns)]]$args$.others, "n (%)")
  # in a study, its standard applied
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (v in c("adsl", "adrs")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", v)),
                     file.path(d, "adam", paste0(v, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SB-2")
  o <- tfl_output("T-C", "table", population = "ITT", extends = "defaults",
                  template = list(name = "orr", args = list(population = "ITT", stat_layout = "cols",
                                                            conf_level = 0.9)))
  f <- file.path(d, "spec", "outputs", "T-C.yml")
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-C", dir = d, save = FALSE))
  t <- sb_tab(suppressMessages(tfl_content_run(tfl_resolve_output(tfl_read_output(f), st), ard)))
  expect_true(all(c("Drug X____n (%)", "Drug X____90% CI") %in% names(t)))
  last <- nrow(t)
  expect_identical(t[["Drug X____n (%)"]][last], "32 (39.0)")
  expect_identical(t[["Drug X____90% CI"]][last], "[30.0, 48.7]")
  # the counts by their own cell, no CI beside them
  expect_identical(t[["Drug X____n (%)"]][1:(last - 1L)], c("2 (2.4)", "3 (3.7)", "7 (8.5)", "30 (36.6)", "40 (48.8)"))
  expect_true(all(is.na(t[["Drug X____90% CI"]][1:(last - 1L)])))
  # the arm's N over its two columns (rtfreporter#709)
  p <- suppressMessages(tfl_content_run(tfl_resolve_output(tfl_read_output(f), st), ard))
  g <- tempfile(fileext = ".rtf")
  on.exit(unlink(g), add = TRUE)
  expect_no_warning(suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), p), g, overwrite = TRUE)))
  txt <- paste(readLines(g, warn = FALSE), collapse = " ")
  n <- sum(adsl$ITTFL == "Y" & adsl$TRT01P == "Drug X")
  expect_true(grepl(sprintf("Drug X (N=%d)", n), txt, fixed = TRUE))
  expect_false(grepl("N=NA", txt, fixed = TRUE))
})
