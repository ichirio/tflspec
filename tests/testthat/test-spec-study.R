# The study's YAML files and the layering of a report over its defaults

yml_dir <- function() system.file("extdata", "spec-yaml", package = "tflspec")

test_that("the study spec reads its files; missing ones are empty", {
  s <- tfl_read_study_spec(yml_dir())
  expect_s3_class(s, "tfl_study_spec")
  fns <- vapply(s$defaults$content, `[[`, "", "fn")
  expect_identical(fns, c("plan_cells", "plan_blanks", "plan_stub", "plan_col_header", "plan_style"))
  expect_true(all(vapply(s$defaults$content, `[[`, "", "from") == "defaults"))
  expect_s3_class(s$defaults$content[[5L]]$args$border, "tfl_call")
  expect_identical(s$defaults$page$header[["2"]]$left, "Protocol: {STUDY_ID}")
  expect_identical(s$patterns, stats::setNames(list(), character()))
  expect_identical(s$toc, list())
  outs <- tfl_read_outputs(yml_dir())
  expect_identical(names(outs), "T-14-1-1")
})

test_that("an id in two files stops the read", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "outputs", "sub"), recursive = TRUE)
  writeLines(c("id: T-1", "kind: table"), file.path(d, "outputs", "T-1.yml"))
  writeLines(c("id: T-1", "kind: table"), file.path(d, "outputs", "sub", "again.yml"))
  expect_error(tfl_read_outputs(d), "two files")
})

test_that("a report over the defaults: maps by key, items by function, order kept", {
  s <- tfl_read_study_spec(yml_dir())
  out <- tfl_resolve_output(tfl_read_outputs(yml_dir())[["T-14-1-1"]], s)
  expect_true(isTRUE(attr(out, "resolved")))
  fns <- vapply(out$content, `[[`, "", "fn")
  from <- vapply(out$content, `[[`, "", "from")
  # the defaults' plan_cells replaced by the report's (in the defaults' place),
  # the other defaults kept in order, the report's own functions after them
  # table_plan first (the table itself), the defaults' plan_cells replaced by
  # the report's in the defaults' place, the other defaults kept in order,
  # the report's own functions after them
  expect_identical(fns, c("table_plan", "plan_cells", "plan_blanks", "plan_stub", "plan_col_header",
                          "plan_style", "plan_labels", "plan_digits", "plan_paginate_rows",
                          "plan_cell_style"))
  expect_identical(from, c("own", "own", "defaults", "defaults", "defaults", "defaults",
                           rep("own", 4L)))
  # plan_cells merges by key: the defaults' categorical cell stays beside the report's continuous
  expect_identical(out$content[[2L]]$args$categorical, "{n:.0f} ({p:.1f%})")
  # page: the defaults' header lines stay, the report's titles and tokens join
  expect_identical(out$page$header[["1"]]$right, "DRAFT")
  expect_identical(out$page$titles[["2"]]$center, "Demographic Characteristics")
  expect_identical(out$page$tokens$COMPANY, "Sample Pharma")
  expect_identical(out$page$tokens$OUTPUT_POPULATION, "Safety Analysis Set")
  expect_identical(out$page$rtf_tables$auto_section, TRUE)
})

test_that("!unset removes a key of the layers; extends: none uses no layer", {
  s <- tfl_read_study_spec(yml_dir())
  o <- tfl_read_outputs(yml_dir())[["T-14-1-1"]]
  o$page$header <- list(`2` = tfl_unset(), `6` = list(center = "<{OUTPUT_POPULATION}>"))
  o$page$tokens$ANALYSIS_TYPE <- tfl_unset()
  r <- tfl_resolve_output(o, s)
  expect_identical(names(r$page$header), c("1", "4", "5", "6"))
  expect_identical(names(r$page$tokens), c("COMPANY", "OUTPUT_POPULATION"))
  expect_false(any(rapply(r$page, function(x) inherits(x, "tfl_unset"), how = "unlist")))
  o$extends <- "none"
  r0 <- tfl_resolve_output(o, s)
  expect_identical(vapply(r0$content, `[[`, "", "from"), rep("own", 6L))
  expect_null(r0$page$header[["1"]])
})

test_that("a pattern between the defaults and the report, nearest last", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "patterns"))
  dir.create(file.path(d, "outputs"))
  writeLines(c("content:", "  - plan_stub: {name: row_label}", "  - plan_blanks: {first: true}",
               "page:", "  tokens: {A: defaults, B: defaults}"), file.path(d, "defaults.yml"))
  writeLines(c("content:", "  - plan_blanks: {first: false, last: true}",
               "page:", "  tokens: {B: pattern, C: pattern}"), file.path(d, "patterns", "compact.yml"))
  writeLines(c("id: T-1", "kind: table", "extends: [patterns/compact, defaults]",
               "content:", "  - table_plan: {cols: ARM}",
               "page:", "  tokens: {C: own}"), file.path(d, "outputs", "T-1.yml"))
  s <- tfl_read_study_spec(d)
  r <- tfl_resolve_output(tfl_read_outputs(d)[["T-1"]], s)
  expect_identical(vapply(r$content, function(it) paste(it$fn, it$from), ""),
                   c("table_plan own", "plan_stub defaults", "plan_blanks pattern:compact"))
  expect_identical(r$content[[3L]]$args, list(first = FALSE, last = TRUE))
  expect_identical(r$page$tokens, list(A = "defaults", B = "pattern", C = "own"))
  expect_error(tfl_resolve_output(tfl_output("T-2", "table", extends = "patterns/nope"), s), "No pattern")
})

test_that("study.yml's spec_version: this reader's is read, a newer one is refused", {
  d <- withr::local_tempdir()
  writeLines(c("study_id: V-1", "spec_version: 1"), file.path(d, "study.yml"))
  expect_identical(tfl_read_study_spec(d)$study$spec_version, 1L)
  writeLines(c("study_id: V-1", "spec_version: 99"), file.path(d, "study.yml"))
  expect_error(tfl_read_study_spec(d), "Update tflspec")
  writeLines(c("study_id: V-1", "spec_version: new"), file.path(d, "study.yml"))
  expect_error(tfl_read_study_spec(d), "not a number")
  # none: an old study.yml reads
  writeLines("study_id: V-1", file.path(d, "study.yml"))
  expect_identical(tfl_read_study_spec(d)$study$study_id, "V-1")
})

test_that("a named-dots verb (plan_cells) merges by key; another verb is replaced whole", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "outputs"))
  writeLines(c("content:",
               "  - plan_cells: {default: '{n} ({p}%)', continuous: {n: '{N}'}}",
               "  - plan_blanks: {first: true, last: true}",
               "  - plan_labels: {AGE: Age, SEX: Sex}"), file.path(d, "defaults.yml"))
  writeLines(c("id: T-1", "kind: table", "content:",
               "  - table_plan: {cols: ARM}",
               "  - plan_cells: {continuous: {n: '{N}', Mean: '{mean}'}}",
               "  - plan_blanks: {last: false}",
               "  - plan_labels: {SEX: Gender}"), file.path(d, "outputs", "T-1.yml"))
  s <- tfl_read_study_spec(d)
  r <- tfl_resolve_output(tfl_read_outputs(d)[["T-1"]], s)
  fns <- vapply(r$content, `[[`, "", "fn")
  expect_identical(fns, c("table_plan", "plan_cells", "plan_blanks", "plan_labels"))
  cells <- r$content[[2L]]$args
  # the layers' `default` stays, the report's `continuous` map merges by key
  expect_identical(names(cells), c("default", "continuous"))
  expect_identical(cells$continuous, list(n = "{N}", Mean = "{mean}"))
  # plan_blanks is replaced whole: no `first` any more
  expect_identical(r$content[[3L]]$args, list(last = FALSE))
  # plan_labels by key: AGE stays, SEX is the report's
  expect_identical(r$content[[4L]]$args, list(AGE = "Age", SEX = "Gender"))
})
