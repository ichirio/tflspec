# The stack family (migration batch 3): ae_soc_pt, ae_ser_soc_pt and ae_disc_soc_pt are
# templates of their own, the variable sets and `by` are required, the analysis set is the
# report's `population:`, and an appended card changes only what it writes

stack_hier <- c("AEBODSYS", "AEDECOD")
stack_all <- c("hierarchical", "ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt", "ae_pt_freq",
               "ae_max_grade", "ae_eair")
# the arguments each takes that have no default (`by` and the variable set), then a test's own
stack_args <- function(name, ...) utils::modifyList(stack_req(name), list(...))

stack_variants <- function() {
  # the three as the variant chain made them before they were templates of their own
  # (the same defaults, the hierarchy left to the report)
  op <- options(tflspec.template_source = "(session)")
  on.exit(options(op))
  tfl_template_variant("zz_soc", from = "hierarchical", ai = TRUE,
    label = "Adverse events by SOC and PT (FDA STF Table 14)",
    defaults = list(where = "TRTEMFL == \"Y\"", any_row = TRUE, any_row_label = "Any TEAE"))
  tfl_template_variant("zz_ser", from = "zz_soc",
    label = "Serious adverse events by SOC and PT (FDA STF Table 10)",
    defaults = list(where = "TRTEMFL == \"Y\" & AESER == \"Y\"", any_row_label = "Any SAE"))
  tfl_template_variant("zz_disc", from = "zz_soc",
    label = "Adverse events leading to treatment discontinuation by SOC and PT (FDA STF Table 12)",
    defaults = list(where = "TRTEMFL == \"Y\" & AEACN == \"DRUG WITHDRAWN\"",
                    any_row_label = "Subjects with at least one AE leading to treatment discontinuation"))
}
stack_drop <- function() {
  for (n in c("zz_soc", "zz_ser", "zz_disc")) {
    if (exists(n, envir = .tpl_registry, inherits = FALSE)) rm(list = n, envir = .tpl_registry)
  }
}

sk_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "SK-1"))
  d
}
# the table the template makes for a report (an appended card: `content`)
sk_table <- function(d, id, name, args, content = NULL) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  o <- tfl_output(id, "table", population = "SAF", extends = "defaults",
                  template = list(name = name, args = args), content = content %||% list())
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = id, dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}

test_that("ae_soc_pt, ae_ser_soc_pt, ae_disc_soc_pt: no variant, no parent", {
  tt <- tfl_templates()
  for (n in c("ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt")) {
    expect_true(is.na(tt$from[tt$name == n]), info = n)
    expect_null(.tpl_get(n, "test")$from)
    expect_true(tt$ai[tt$name == n], info = n)
  }
  # nothing reads another template's definition at run time: the help has no "variant of"
  out <- capture.output(tfl_tpl_help("ae_ser_soc_pt"))
  expect_false(any(grepl("variant", out, fixed = TRUE)))
  expect_false(any(grepl("variant:", tfl_tpl_args("ae_ser_soc_pt")$layer, fixed = TRUE)))
})

test_that("each standalone has the defaults and the RTF of the variant it was", {
  skip_on_cran()
  stack_variants()
  withr::defer(stack_drop())
  pairs <- c(ae_soc_pt = "zz_soc", ae_ser_soc_pt = "zz_ser", ae_disc_soc_pt = "zz_disc")
  keep <- function(p) lapply(p, function(x) x[c("name", "default", "required", "type", "tier", "choices")])
  for (n in names(pairs)) {
    v <- pairs[[n]]
    # the parameters: the same, in the same order, each with the same default
    expect_identical(keep(tfl_template_params(n)), keep(tfl_template_params(v)), info = n)
    # the table of defaults (the label of the variant's layer aside)
    a <- tfl_tpl_args(n)
    b <- tfl_tpl_args(v)
    expect_identical(a$param, b$param, info = n)
    expect_identical(a$effective, b$effective, info = n)
    expect_identical(a$required, b$required, info = n)
    # the template run: the same items and the same analyses
    x <- tfl_template_expand(n, list(population = "SAF", by = "TRT01A", hierarchy = stack_hier))
    y <- tfl_template_expand(v, list(population = "SAF", by = "TRT01A", hierarchy = stack_hier))
    strip <- function(z) {
      z$content <- lapply(z$content, function(it) { it$from <- NULL; it })
      z
    }
    expect_identical(strip(x), strip(y), info = n)
  }
})

test_that("each standalone makes the variant's RTF in a study", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  stack_variants()
  withr::defer(stack_drop())
  d <- sk_study(withr::local_tempdir())
  pairs <- c(ae_soc_pt = "zz_soc", ae_ser_soc_pt = "zz_ser", ae_disc_soc_pt = "zz_disc")
  for (n in names(pairs)) {
    for (extra in list(list(), list(total = TRUE, min_pct = 5))) {
      a <- sk_table(d, "T-A", n, utils::modifyList(stack_args(n), extra))
      b <- sk_table(d, "T-B", pairs[[n]], utils::modifyList(stack_args(n), extra))
      expect_identical(a, b, info = n)
      expect_gt(nrow(a), 1L)
    }
  }
})

test_that("hierarchy has no default: leaving it out is the plain required-arg error", {
  for (n in c("hierarchical", "ae_soc_pt", "ae_ser_soc_pt", "ae_disc_soc_pt", "ae_max_grade", "ae_eair")) {
    p <- tfl_template_params(n)$hierarchy
    expect_null(p$default, info = n)
    expect_true(p$required, info = n)
    a <- tfl_tpl_args(n)
    expect_identical(a$tier[a$param == "hierarchy"], "required", info = n)
    err <- expect_error(
      tfl_template_expand(n, list(population = "SAF", by = "TRT01A")),
      sprintf("template %s needs 'hierarchy'", n), fixed = TRUE, info = n)
    # (no message that points to a new form)
    expect_false(grepl("migrat|moved|renamed|used to", conditionMessage(err), ignore.case = TRUE), info = n)
  }
})

test_that("ae_pt_freq: term has no default", {
  p <- tfl_template_params("ae_pt_freq")$term
  expect_null(p$default)
  expect_true(p$required)
  expect_error(tfl_template_expand("ae_pt_freq", list(population = "SAF", by = "TRT01A")),
               "template ae_pt_freq needs 'term'", fixed = TRUE)
  x <- tfl_template_expand("ae_pt_freq", list(population = "SAF", by = "TRT01A", term = "AEDECOD"))
  expect_identical(x$ard$analyses[[1L]]$variables, "AEDECOD")
})

test_that("by has no default: required in the seven", {
  for (n in stack_all) {
    p <- tfl_template_params(n)$by
    expect_null(p$default, info = n)
    expect_true(p$required, info = n)
    args <- stack_args(n)
    args$by <- NULL
    expect_error(tfl_template_expand(n, c(list(population = "SAF"), args)),
                 "needs 'by'", fixed = TRUE, info = n)
  }
})

test_that("the single ADaM variables and the TEAE where stay as defaults", {
  expect_identical(tfl_template_params("ae_max_grade")$grade$default, "AESEV")
  expect_identical(tfl_template_params("ae_max_grade")$data$default, "ADAE")
  expect_identical(tfl_template_params("ae_eair")$exposure$default, "TRTDURD")
  expect_identical(tfl_template_params("ae_pt_freq")$where$default, "TRTEMFL == \"Y\"")
  expect_identical(tfl_template_params("ae_soc_pt")$where$default, "TRTEMFL == \"Y\"")
  expect_identical(tfl_template_params("ae_ser_soc_pt")$where$default, "TRTEMFL == \"Y\" & AESER == \"Y\"")
  expect_identical(tfl_template_params("ae_disc_soc_pt")$where$default,
                   "TRTEMFL == \"Y\" & AEACN == \"DRUG WITHDRAWN\"")
  expect_null(tfl_template_params("hierarchical")$where$default)
})

test_that("population is not a template parameter: the report's population: is the analysis set", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  for (n in stack_all) {
    p <- tfl_template_params(n)$population
    expect_true(isTRUE(p$hidden), info = n)
    expect_false("population" %in% tfl_tpl_args(n)$param, info = n)
  }
  d <- sk_study(withr::local_tempdir())
  s <- tfl_read_study_spec(d)
  # the report's field is it ...
  o <- tfl_output("T-P", "table", population = "SAF", extends = "defaults",
                  template = list(name = "ae_soc_pt", args = stack_args("ae_soc_pt")))
  ro <- tfl_resolve_output(o, s)
  ds <- vapply(ro$ard$analysis_data, function(e) e$id, "")
  expect_true("adsl_saf" %in% ds)
  # ... and the template: SPEC does not take it
  o2 <- tfl_output("T-P2", "table", population = "SAF", extends = "defaults",
                   template = list(name = "ae_soc_pt", args = c(list(population = "SAF"), stack_args("ae_soc_pt"))))
  expect_error(tfl_resolve_output(o2, s), "population")
})

# an appended plan_paginate_rows(max_rows =) keeps the template's split and page_by; an appended
# plan_labels changes one label only
sk_resolve <- function(name, args, content) {
  s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  o <- tfl_output("T-9", "table", population = "SAF", template = list(name = name, args = args),
                  content = content %||% list())
  tfl_resolve_output(o, s)
}

test_that("an appended plan_paginate_rows(max_rows =) keeps the template's split and page_by", {
  for (n in stack_all) {
    base <- Filter(function(it) identical(it$fn, "plan_paginate_rows"),
                   sk_resolve(n, stack_args(n), NULL)$content)
    ro <- sk_resolve(n, stack_args(n), list(tfl_item("plan_paginate_rows", list(max_rows = 15))))
    pr <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), ro$content)
    expect_length(pr, 1L)
    expect_identical(pr[[1L]]$args$max_rows, 15, info = n)
    keep <- setdiff(names(base[[length(base)]]$args), "max_rows")
    # (ae_pt_freq's page is max_rows alone: nothing else to keep)
    if (!identical(n, "ae_pt_freq")) expect_true(length(keep) >= 1L, info = n)
    expect_identical(pr[[1L]]$args[keep], base[[length(base)]]$args[keep], info = n)
  }
  expect_identical(
    Filter(function(it) identical(it$fn, "plan_paginate_rows"),
           sk_resolve("ae_soc_pt", stack_args("ae_soc_pt"), list(tfl_item("plan_paginate_rows", list(max_rows = 15)))
           )$content)[[1L]]$args$split, "group_force")
})

test_that("an appended plan_labels changes one label only", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  d <- sk_study(withr::local_tempdir())
  for (n in stack_all) {
    base <- sk_table(d, "T-L", n, stack_args(n))
    key <- if ("group1" %in% names(base)) base$group1[1L] else base$label[1L]
    # (ae_max_grade: a plan_labels on an SOC, the any-record block or a grade stops the table, as
    # it did before this batch -- an open point; a PT's is the one to change here)
    if (identical(n, "ae_max_grade")) key <- base$group2[!is.na(base$group2)][1L]
    expect_false(is.na(key), info = n)
    one <- sk_table(d, "T-L", n, stack_args(n),
                    list(tfl_item("plan_labels", stats::setNames(list("Renamed label"), key))))
    expect_identical(dim(one), dim(base), info = n)
    changed <- which(as.matrix(base) != as.matrix(one) | xor(is.na(as.matrix(base)), is.na(as.matrix(one))),
                     arr.ind = TRUE)
    # only the cells that held that label, and they hold the new one
    held <- which(as.matrix(base) == key, arr.ind = TRUE)
    expect_identical(unname(changed[order(changed[, 1L], changed[, 2L]), , drop = FALSE]),
                     unname(held[order(held[, 1L], held[, 2L]), , drop = FALSE]), info = n)
    expect_true(all(as.matrix(one)[changed] == "Renamed label"), info = n)
  }
})
