# A report written as a template SPEC (`template: {name, args}` in its YAML
# file): read and written, resolved between the study's layers and the
# report's own items, written out as its items (B), and its program the
# template's call with the study's lines after it (A).

ts_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

ts_output <- function(content = list(tfl_item("plan_paginate_rows", list(max_rows = 24L))),
                      args = list()) {
  tfl_output("T-9", "table", population = "SAF",
             codelists = list(TRT01A = list(list(value = "Drug X"), list(value = "Control"))),
             content = content,
             template = list(name = "demog", args = utils::modifyList(list(
               population = "SAF", continuous = "AGE", categorical = c("AGEGR1", "SEX"),
               labels = c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)", SEX = "Sex, n (%)")),
               args)))
}

ts_ard <- function() {
  saf <- adsl[adsl$SAFFL == "Y", ]
  cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = AGE),
    cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX)))
}

test_that("a template SPEC is read and written back: the template's name and arguments, named vectors as maps", {
  o <- ts_output()
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  txt <- readLines(f)
  expect_true(any(grepl("^template:", txt)))
  expect_true(any(grepl("^      AGE: Age \\(years\\)", txt)))
  back <- tfl_read_output(f)
  expect_identical(back$template$name, "demog")
  expect_identical(back$template$args$labels$SEX, "Sex, n (%)")
  expect_identical(back$template$args$categorical, c("AGEGR1", "SEX"))
  expect_output(print(back), "template: demog")
  # a figure designer's `template: km_simple` (a name) is no template SPEC
  expect_null(.tpl_spec_of(tfl_output("F-1", "figure", template = "km_simple")))
  expect_s3_class(tfl_item("plan_cells", list(), from = "template:demog"), "tfl_item")
})

test_that("resolved: the template's items between the study's layers and the report's own; its ard: under the report's", {
  s <- ts_study()
  ro <- tfl_resolve_output(ts_output(), s)
  from <- vapply(ro$content, `[[`, "", "from")
  fns <- vapply(ro$content, `[[`, "", "fn")
  expect_true("template:demog" %in% from)
  # the report's own item replaces the template's
  pr <- ro$content[[match("plan_paginate_rows", fns)]]
  expect_identical(pr$from, "own")
  expect_identical(pr$args, list(max_rows = 24L))
  # a named-dots verb: the template's key and the study's beside it
  pc <- ro$content[[match("plan_cells", fns)]]
  expect_true(all(c("continuous", "categorical") %in% names(pc$args)))
  expect_identical(vapply(ro$ard$analyses, `[[`, "", "id"), c("GROUPN", "CONT", "CAT"))
  expect_identical(ro$data$source, "ard")
  # the report's own ard: entry replaces the template's of the same id
  own_ard <- list(analyses = list(list(id = "CAT", method = "categorical", data = "adsl_saf",
                                       by = "TRT01A", variables = "SEX")))
  o2 <- ts_output(); o2$ard <- own_ard
  ro2 <- tfl_resolve_output(o2, s)
  expect_identical(ro2$ard$analyses[[3L]]$variables, "SEX")
  # provenance: by argument, the template's marked (categorical's cell: the
  # study standard's, through the template's count_cell)
  rp <- tfl_resolve_output(ts_output(), s, provenance = TRUE)
  pc <- rp$content[[match("plan_cells", vapply(rp$content, `[[`, "", "fn"))]]
  expect_identical(unname(pc$arg_from[c("continuous", "categorical")]), c("template:demog", "template:demog"))
})

test_that("A (the template's call) = B (its items) = the items written out, as tables", {
  skip_if_not_installed("cards")
  s <- ts_study()
  ard <- ts_ard()
  o <- ts_output()
  ro <- tfl_resolve_output(o, s)
  code <- tfl_content_code(ro, pipe = "|>")
  expect_match(code, "# template demog", fixed = TRUE, all = FALSE)
  expect_match(paste(code, collapse = "\n"), "plan <- tflspec::tfl_tpl(\n  ard,\n  \"demog\",", fixed = TRUE)
  # (the code lists are the ARD's: no codelists = for a template of cards' analyses)
  expect_false(any(grepl("codelists =", code, fixed = TRUE)))
  expect_match(code, "# (study default)", fixed = TRUE, all = FALSE)
  expect_match(code, "# (own)", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("table_plan(", code, fixed = TRUE)))
  e <- new.env()
  e$ard <- ard
  suppressMessages(eval(parse(text = code), envir = e))
  tab <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  b <- suppressMessages(tfl_content_run(ro, ard))
  expect_identical(tab(e$plan), tab(b))
  # B written out: a code SPEC, the same table
  w <- tfl_template_to_spec(o, s)
  expect_null(w$template)
  expect_true("template:demog" %in% vapply(w$content, `[[`, "", "from"))
  expect_identical(tab(suppressMessages(tfl_content_run(tfl_resolve_output(w, s), ard))), tab(b))
  # the items, by asking for them
  items <- tfl_content_code(ro, template = "items")
  expect_match(items, "# (template demog)", fixed = TRUE, all = FALSE)
  expect_match(items, "table_plan(", fixed = TRUE, all = FALSE)
})

test_that("a study's default for a parameter that says where it is: taken, and a SPEC's other value says so", {
  s <- ts_study()
  rows <- list(n = "{N:.0f}", Median = "{median:.1f}")
  k <- which(vapply(s$defaults$content, `[[`, "", "fn") == "plan_cells")
  s$defaults$content[[k]]$args$continuous <- rows
  ro <- tfl_resolve_output(ts_output(), s)
  expect_identical(attr(ro, "template")$study[1L], "cells")
  expect_identical(attr(ro, "template")$args$cells, rows)
  expect_match(paste(tfl_content_code(ro), collapse = "\n"), "study defaults: cells", fixed = TRUE)
  expect_message(tfl_resolve_output(ts_output(args = list(cells = list(n = "{N}"))), s),
                 "overrides the study standard")
})

test_that("what the call cannot say is written as the items; a wrong template is said", {
  s <- ts_study()
  # the report's own table roles
  o <- ts_output(content = list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(grp = "variable")))))
  ro <- tfl_resolve_output(o, s)
  expect_false(attr(ro, "template")$ok)
  expect_message(code <- tfl_content_code(ro), "its items are written")
  expect_match(code, "table_plan(", fixed = TRUE, all = FALSE)
  bad <- ts_output(); bad$template$name <- "no_such"
  expect_error(tfl_resolve_output(bad, s), "no template")
  lst <- tfl_output("L-1", "table", template = list(name = "listing", args = list(data = "ADAE",
    columns = list(list(vars = "USUBJID", label = "Subject")))))
  expect_error(tfl_resolve_output(lst, s), "makes a listing")
})

test_that("the sheets (and the study's ARD program) take a template report's analyses", {
  s <- ts_study()
  sh <- suppressMessages(tfl_sheets_from_spec(s, list(`T-9` = ts_output())))
  a <- sh$ard$analyses
  expect_identical(a$analysis_id[a$output_id %in% "T-9"], c("GROUPN", "CONT", "CAT"))
})

test_that("every template as a template SPEC: resolved as its items written out (B), the study's layers on both", {
  s <- ts_study()
  # its analysis set, as data.yml defines it (a figure's subjects)
  s$data$populations <- list(list(id = "SAF", dataset = "ADSL", where = tfl_r("SAFFL == \"Y\"")))
  tp <- tfl_templates()
  for (nm in names(.tpl_examples)) {
    # (a template whose data stage is the report's: its data: steps)
    own <- if (any(vapply(.tpl_get(nm, "test")$params, function(p) isTRUE(p$hidden) &&
                            identical(p$stage, "data"), NA))) {
      list(steps = list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = "PFS")))
    }
    o <- tfl_output(paste0("X-", nm), tp$kind[tp$name == nm], population = "SAF",
                    template = list(name = nm, args = .tpl_examples[[nm]]), data = own)
    ro <- tfl_resolve_output(o, s)
    rb <- tfl_resolve_output(tfl_template_to_spec(o, s), s)
    expect_identical(lapply(ro$content, unclass), lapply(rb$content, unclass), info = nm)
  }
})

test_that("shift: the study's plan_stub(name, before) keeps the template's stub columns (vars)", {
  s <- ts_study()
  o <- tfl_output("X-shift", "table", population = "SAF",
                  template = list(name = "shift", args = .tpl_examples$shift))
  st <- Filter(function(it) identical(it$fn, "plan_stub"), tfl_resolve_output(o, s)$content)
  expect_length(st, 1L)
  expect_identical(st[[1L]]$args$vars, c("group", "BNRIND"))
  expect_false(.tpl_is_look(tfl_item("plan_stub", list(vars = "a", before = TRUE))))
  expect_true(.tpl_is_look(tfl_item("plan_stub", list(before = TRUE))))
  expect_true(.tpl_is_look(tfl_item("plan_style", list(font_size = 8))))
})
