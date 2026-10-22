# tfl_tpl(): one call for every table and listing template; the layouts'
# parameters (summary, hierarchical); demog and the AE tables as their
# parameter sets

# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

tt_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
tt_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}
tt_dm_ard <- function() {
  saf <- adsl[adsl$SAFFL == "Y", ]
  cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = c(AGE, TRTDURD)),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
}

test_that("tfl_tpl(): the same plan as the template's own function; its errors", {
  tt_skip()
  ard <- tt_dm_ard()
  expect_identical(tt_tab(tfl_tpl(ard, "demog", population = "SAF")),
                   tt_tab(tpl_demog(ard, population = "SAF")))
  s <- tt_tab(tfl_tpl(ard, "summary", population = "SAF", continuous = c("AGE", "TRTDURD")))
  expect_true(all(c("AGE", "TRTDURD") %in% s$group))
  expect_error(tfl_tpl(ard, "nope"), "no template 'nope'")
  expect_error(tfl_tpl(ard, "demog", population = "SAF", colour = "red"), "no parameter")
  expect_error(tfl_tpl(ard, c("demog", "summary")), "one text")
  # a figure: the data the program made, its data step's parameters not the call's
  expect_error(tfl_tpl(adtte, "km_simple", population = "SAF"), "the program's data step")
  # a listing: the data as the program made it
  cols <- list(list(vars = "USUBJID", label = "Subject"), list(vars = "AEDECOD", label = "Term"))
  ae <- adae[seq_len(20L), ]
  expect_identical(tt_tab(tfl_tpl(ae, "listing", data = "ADAE", columns = cols)),
                   tt_tab(tpl_listing(ae, data = "ADAE", columns = cols)))
  # the registry: the call, the label, the AI's templates
  t <- tfl_templates()
  expect_identical(t$call[t$name == "demog"], "tfl_tpl")
  expect_identical(t$call[t$name == "km_simple"], "tfl_tpl")
  expect_identical(sort(t$name[t$ai]), c("ae_disc_soc_pt", "ae_ser_soc_pt", "ae_soc_pt", "demog", "km_disc"))
  expect_match(t$label[t$name == "ae_ser_soc_pt"], "FDA STF Table 10", fixed = TRUE)
  expect_identical(t$from[t$name == "ae_ser_soc_pt"], "ae_soc_pt")
})

test_that("the layouts' parameters: what a report changes; the look is the study standard's", {
  p <- tfl_template_params("summary")
  expect_identical(p$population$tier, "required")
  expect_identical(p$by$tier, "usual")
  expect_identical(sort(names(p)), sort(c(
    "population", "subset", "data", "where", "by", "continuous", "categorical", "labels",
    "order", "cells", "cat_cell", "statistics", "cat_statistics", "total", "stack")))
  expect_identical(sort(names(tfl_template_params("hierarchical"))), sort(c(
    "population", "subset", "hierarchy", "data", "where", "by", "overall", "heading", "sort",
    "min_pct", "min_pct_in", "cell", "denominator", "total")))
  # every parameter's stage: an argument of the call, unless it says "data"
  expect_true(all(vapply(p, `[[`, "", "stage") == "call"))
  expect_identical(tfl_param("x", stage = "data")$stage, "data")
  expect_error(tfl_param("x", stage = "plot"), "should be one of")
  # the look is not a parameter: the study standard's, or |> plan_*() after
  expect_error(tfl_template_expand("summary", list(population = "SAF", continuous = "AGE",
                                                   stub_name = "row_label")),
               "no parameter 'stub_name'")
  x <- tfl_template_expand("summary", list(population = "SAF", continuous = "AGE", order = "AGE"))
  fns <- vapply(x$content, `[[`, "", "fn")
  expect_identical(fns, c("table_plan", "plan_cells", "plan_levels", "plan_paginate_rows"))
  # an item when its parameter is given (order -> plan_levels), else none
  y <- tfl_template_expand("summary", list(population = "SAF", continuous = "AGE"))
  expect_false("plan_levels" %in% vapply(y$content, `[[`, "", "fn"))
  # the layout's page: its own value, which the study standard's replaces
  expect_identical(x$content[[4L]]$args, list(max_rows = 30L, split = "group_safe"))
  # one cards::ard_stack() call, on summary itself
  st <- tfl_template_expand("summary", list(population = "SAF", continuous = "AGE",
                                            categorical = "SEX", stack = TRUE))
  expect_identical(vapply(st$ard$analyses, `[[`, "", "id"), c("STACK", "CONT", "CAT"))
  expect_identical(st$ard$analyses[[2L]]$parent, "STACK")
  # demog's the same ids, its label the template's
  dm <- tfl_template_expand("demog", list(population = "SAF", stack = TRUE))
  expect_identical(vapply(dm$ard$analyses, `[[`, "", "id"), c("STACK", "CONT", "CAT"))
  expect_match(dm$ard$analyses[[1L]]$label, "Demographics", fixed = TRUE)
  # a template's own default variables: those the data has (any_of)
  expect_true(isTRUE(tfl_template_expand("demog", list(population = "SAF"))$ard$analyses[[2L]]$optional))
  expect_null(tfl_template_expand("demog", list(population = "SAF", continuous = "AGE"))$ard$analyses[[2L]]$optional)
  # hierarchical: min_pct the shared filter, the Total column
  h <- tfl_template_expand("ae_soc_pt", list(population = "SAF", min_pct = 5, total = TRUE))
  hf <- vapply(h$content, `[[`, "", "fn")
  expect_true(all(c("plan_filter", "plan_total") %in% hf))
  expect_true(isTRUE(h$ard$analyses[[1L]]$overall))
  # the nested variant: its root's parameters, its own and its parent's defaults
  v <- tfl_template_params("ae_ser_soc_pt")
  expect_identical(v$where$default, "TRTEMFL == \"Y\" & AESER == \"Y\"")
  expect_identical(v$hierarchy$default, c("AEBODSYS", "AEDECOD"))
  expect_false(v$hierarchy$required)
})

tt_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "TT-1")
  d
}
tt_report <- function(d, id, name, args) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  tfl_write_output(tfl_output(id, "table", population = "SAF", extends = "defaults",
                              template = list(name = name, args = args)), f)
  f
}
tt_run <- function(d, id, f) {
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = id, dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  list(ro = ro, ard = ard, plan = suppressMessages(tfl_content_run(ro, ard)))
}

test_that("in a study: demog with a Total column and its own order; the program is one call", {
  skip_on_cran()
  tt_skip()
  skip_if_not_installed("haven")
  d <- tt_study(withr::local_tempdir())
  f <- tt_report(d, "T-DM", "demog", list(population = "SAF", total = TRUE,
                                          order = c("SEX", "AGE", "AGEGR1", "RACE", "ETHNIC")))
  r <- tt_run(d, "T-DM", f)
  t <- tt_tab(r$plan)
  expect_true("Total" %in% names(t))
  expect_identical(unique(t$group)[1L], "Sex, n (%)")
  n <- sum(adsl$SAFFL == "Y")
  expect_identical(t$Total[t$group == "Age (years)" & t$label == "n"], as.character(n))
  # the program: the template's call, the study standard's look after it
  code <- paste(tfl_content_code(r$ro, pipe = "|>"), collapse = "\n")
  expect_match(code, "tfl_tpl(\n  ard,\n  \"demog\",", fixed = TRUE)
  expect_false(grepl("stub_name", code, fixed = TRUE))
  expect_match(code, "# (study default)\n  plan_stub(", fixed = TRUE)
  # the program's plan is the report's
  e <- new.env()
  e$ard <- r$ard
  suppressMessages(eval(parse(text = code), envir = e))
  expect_identical(tt_tab(e$plan), t)
  # a report's own value over the study standard's: said
  f2 <- tt_report(d, "T-DM2", "demog", list(population = "SAF", cat_cell = "{n}"))
  expect_message(tfl_resolve_output(tfl_read_output(f2), tfl_read_study_spec(d)),
                 "cat_cell overrides the study standard \\(plan_cells.categorical\\)")
})

test_that("in a study: ae_ser_soc_pt is ae_soc_pt with its defaults; ae_soc_pt's min_pct", {
  skip_on_cran()
  tt_skip()
  skip_if_not_installed("haven")
  d <- tt_study(withr::local_tempdir())
  f1 <- tt_report(d, "T-SAE", "ae_ser_soc_pt", list(population = "SAF"))
  f2 <- tt_report(d, "T-SAE2", "ae_soc_pt", list(
    population = "SAF", where = "TRTEMFL == \"Y\" & AESER == \"Y\"", overall = "Any SAE"))
  a <- tt_tab(tt_run(d, "T-SAE", f1)$plan)
  b <- tt_tab(tt_run(d, "T-SAE2", f2)$plan)
  expect_identical(a, b)
  expect_identical(a$group1[1L], "Any SAE")
  # its program calls it by its name (tflspec's variant)
  code <- tfl_content_code(tt_run(d, "T-SAE", f1)$ro, pipe = "|>")
  expect_true(any(grepl("\"ae_ser_soc_pt\",", code, fixed = TRUE)))
  # min_pct: fewer rows than all (the sample's terms: 5% or more in some column)
  f3 <- tt_report(d, "T-SOC", "ae_soc_pt", list(population = "SAF"))
  f4 <- tt_report(d, "T-SOC10", "ae_soc_pt", list(population = "SAF", min_pct = 10))
  all <- tt_tab(tt_run(d, "T-SOC", f3)$plan)
  ten <- tt_tab(tt_run(d, "T-SOC10", f4)$plan)
  expect_lt(nrow(ten), nrow(all))
  expect_gt(nrow(ten), 0L)
})

test_that("the layout's own page and column header: the study standard's replace them", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  fn_args <- function(ro, fn) Filter(function(i) identical(i$fn, fn), ro$content)
  res <- function(name, args) {
    tfl_resolve_output(tfl_output("T-9", "table", population = "SAF",
                                  template = list(name = name, args = args)), s)
  }
  # no plan_paginate_rows in the standard: the layout's
  pg <- fn_args(res("summary", list(continuous = "AGE")), "plan_paginate_rows")
  expect_identical(pg[[length(pg)]]$args, list(max_rows = 30L, split = "group_safe"))
  # the standard's: it replaces the layout's
  s$defaults$content <- c(s$defaults$content,
                          list(tfl_item("plan_paginate_rows", list(max_rows = 15L))))
  ro <- res("summary", list(continuous = "AGE"))
  pg <- fn_args(ro, "plan_paginate_rows")
  expect_identical(pg[[length(pg)]]$args, list(max_rows = 15L))
  hp <- fn_args(res("ae_soc_pt", list()), "plan_paginate_rows")
  expect_identical(hp[[length(hp)]]$args, list(max_rows = 15L))
  # hierarchical's column header is its shape (the counts' "n (%)" line, the
  # levels' heading): a standard's general header (Characteristic, (N=n))
  # does not replace it
  s$defaults$content <- c(s$defaults$content, list(tfl_item("plan_col_header", list(
    header = list(list(line = "1", cols = ".values", span = "each", text = "{col}"))))))
  hh <- fn_args(res("ae_soc_pt", list()), "plan_col_header")
  expect_length(hh, 1L)
  expect_identical(hh[[1L]]$args$header[[2L]]$text, "{col}\n(N={n})\nn (%)")
})
