# The study's YAML files and the layering of a report over its defaults

yml_dir <- function() system.file("extdata", "spec-yaml", package = "tflspec")

test_that("the study spec reads its files; missing ones are empty", {
  s <- tfl_read_study_spec(yml_dir())
  expect_s3_class(s, "tfl_study_spec")
  fns <- vapply(s$defaults$content, `[[`, "", "fn")
  expect_identical(fns, c("plan_cells", "plan_col_header", "plan_stub", "plan_blanks", "plan_style"))
  expect_true(all(vapply(s$defaults$content, `[[`, "", "from") == "defaults"))
  expect_s3_class(s$defaults$content[[which(fns == "plan_style")]]$args$border_header, "tfl_call")
  expect_identical(s$defaults$page, list())
  expect_identical(tflspec:::.tpl_style_set("tfl_standard", s)$page$header$first[["2"]]$left, "Protocol: {STUDY_ID}")
  expect_identical(s$study$tokens$COMPANY, "Sample Pharma")
  expect_null(s$patterns)
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
  # table_plan first (the table itself); the defaults' plan_cells merged with
  # the report's by key; the other defaults kept; the report's own functions
  # in the generators' order (the schema's rank), so the whole is in rank order
  expect_identical(fns[1L], "table_plan")
  expect_setequal(fns, c("table_plan", "plan_cells", "plan_blanks", "plan_stub", "plan_col_header",
                         "plan_style", "plan_labels", "plan_digits", "plan_paginate_rows",
                         "plan_cell_style"))
  ranks <- vapply(fns[-1L], function(f) tflspec:::.verb_rank(f), 1)
  expect_false(is.unsorted(ranks, na.rm = TRUE))
  expect_identical(from[fns %in% c("plan_blanks", "plan_stub", "plan_col_header", "plan_style")], rep("defaults", 4L))
  expect_identical(from[!fns %in% c("plan_blanks", "plan_stub", "plan_col_header", "plan_style")], rep("own", 6L))
  # plan_cells merges by key: the defaults' categorical cell stays beside the report's continuous
  cells <- out$content[[which(fns == "plan_cells")]]$args
  # (the zero guard, then the template)
  expect_identical(vapply(cells$categorical, as.character, ""), c("n == 0 ~ '0'", "{n:.0f} ({p:.1f%})"))
  expect_identical(names(cells$continuous)[1L], "n")
  # page: the configuration's header lines (tfl_standard), the report's titles and tokens join
  expect_identical(out$page$header[["1"]]$right, "{STATUS}")
  expect_identical(out$page$titles[["2"]]$center, "Demographic Characteristics")
  expect_identical(out$page$tokens$COMPANY, "Sample Pharma")
  expect_identical(out$page$tokens$OUTPUT_POPULATION, "Safety Analysis Set")
  expect_identical(out$page$rtf_tables$auto_section, TRUE)
})

test_that("!unset removes a key of the layers; extends: none uses no layer", {
  s <- tfl_read_study_spec(yml_dir())
  o <- tfl_read_outputs(yml_dir())[["T-14-1-1"]]
  o$page$header <- list(`2` = tfl_unset(), `6` = list(center = "A body line"))
  o$page$tokens$ANALYSIS_TYPE <- tfl_unset()
  r <- tfl_resolve_output(o, s)
  # (the configuration's five lines, then the body: a body line never takes
  # a configuration line's place)
  expect_identical(names(r$page$header), as.character(1:6))
  expect_identical(r$page$header[["6"]]$center, "A body line")
  expect_identical(names(r$page$tokens), c("COMPANY", "STATUS", "OUTPUT_POPULATION"))
  expect_false(any(rapply(r$page, function(x) inherits(x, "tfl_unset"), how = "unlist")))
  o$extends <- "none"
  r0 <- tfl_resolve_output(o, s)
  expect_identical(vapply(r0$content, `[[`, "", "from"), rep("own", 6L))
  # (no file layer: the report keeps its page configuration)
  expect_identical(r0$page$header[["1"]]$left, "{COMPANY}")
})

test_that("a pattern stops: spec/patterns/ and extends: patterns/<name> are old forms (canon :380)", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "patterns"))
  dir.create(file.path(d, "outputs"))
  writeLines(c("content:", "  - plan_stub: {name: row_label}"), file.path(d, "defaults.yml"))
  write_study_yml(d, "tokens: {A: study}")
  # (an empty folder holds no old form: nothing to stop on)
  s <- tfl_read_study_spec(d)
  writeLines(c("content:", "  - plan_blanks: {first: false}"), file.path(d, "patterns", "compact.yml"))
  e <- tryCatch(tfl_read_study_spec(d), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  expect_identical(e$form, "patterns_dir")
  expect_match(conditionMessage(e), "styles: <name>:", fixed = TRUE)
  e <- tryCatch(tfl_output("T-2", "table", extends = c("patterns/compact", "defaults")), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  expect_identical(e$form, "extends_pattern")
  expect_error(tfl_output("T-2", "table", extends = list(content = "patterns/compact")),
               "`extends: patterns/compact` is no longer read", fixed = TRUE)
  expect_error(tflspec:::.layer_of("patterns/compact", s), class = "tflspec_old_form")
  expect_error(tflspec:::.layer_of("other", s), "names \"defaults\" or \"none\"")
  expect_error(tfl_write_study_part(s, "patterns"), class = "tflspec_old_form")
  expect_error(tfl_write_study_part(s, "defaults", tempfile(), name = "compact"), class = "tflspec_old_form")
})

test_that("study.yml's spec_version: 2 is read; none, an older or a newer one stops", {
  d <- withr::local_tempdir()
  writeLines(c("study_id: V-1", "spec_version: 2"), file.path(d, "study.yml"))
  expect_identical(tfl_read_study_spec(d)$study$spec_version, 2L)
  writeLines(c("study_id: V-1", "spec_version: 99"), file.path(d, "study.yml"))
  expect_error(tfl_read_study_spec(d), "Update tflspec")
  writeLines(c("study_id: V-1", "spec_version: new"), file.path(d, "study.yml"))
  expect_error(tfl_read_study_spec(d), "not a number")
  # an older one, and none: an old form, with the list of every old form
  # and what to write instead (canon 11-10 :377)
  for (x in list(c("study_id: V-1", "spec_version: 1"), "study_id: V-1")) {
    writeLines(x, file.path(d, "study.yml"))
    e <- tryCatch(tfl_read_study_spec(d), error = function(e) e)
    expect_s3_class(e, "tflspec_old_form")
    expect_identical(e$form, "spec_version")
    m <- conditionMessage(e)
    expect_match(m, "write `spec_version: 2` in study.yml", fixed = TRUE)
    for (l in tflspec:::.old_form_lines()) expect_true(grepl(l, m, fixed = TRUE), info = l)
  }
  expect_match(conditionMessage(tryCatch(tfl_read_study_spec(d), error = function(e) e)),
               "a study.yml with no `spec_version`", fixed = TRUE)
  # an empty study.yml says no version either
  writeLines(character(), file.path(d, "study.yml"))
  expect_error(tfl_read_study_spec(d), class = "tflspec_old_form")
  # the other reader of study.yml, the file and the folder
  writeLines("study_id: V-1", file.path(d, "study.yml"))
  expect_error(tfl_resolve_output(tfl_output("T-1"), study = d), class = "tflspec_old_form")
  # no study.yml: the current form
  file.remove(file.path(d, "study.yml"))
  expect_null(tfl_read_study_spec(d)$study$spec_version)
})

test_that("the writers of study.yml stamp the current spec_version", {
  d <- withr::local_tempdir()
  sample <- system.file("extdata", "spec-yaml", package = "tflspec")
  expect_identical(tfl_read_study_spec(sample)$study$spec_version, tflspec:::.spec_version)
  # an answer's study.yml without the line gets it
  ans <- c("### study.yml", "", "```yaml", "study_id: U-1", "```", "")
  suppressMessages(tfl_ai_unpack(ans, d))
  expect_identical(readLines(file.path(d, "study.yml"))[1L], "spec_version: 2")
  expect_identical(tfl_read_study_spec(d)$study$study_id, "U-1")
  # one with it keeps its own
  ans[4L] <- "spec_version: 2\nstudy_id: U-2"
  suppressMessages(tfl_ai_unpack(ans, d, overwrite = TRUE))
  expect_identical(sum(grepl("^spec_version:", readLines(file.path(d, "study.yml")))), 1L)
  # the AI kit's blank study says it
  expect_true("spec_version: 2" %in% trimws(sub("#.*", "", tflspec:::.ai_blank_spec_md())))
  # a new study's
  skip_if_not_installed("haven")
  s <- file.path(d, "new")
  dir.create(file.path(s, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(s, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(s, adam = "adam", study_id = "N-1"))
  expect_identical(tfl_read_study_spec(s)$study$spec_version, tflspec:::.spec_version)
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
  # plan_blanks merges by argument (an appearance verb): `first` stays, `last` is the report's
  expect_identical(r$content[[3L]]$args, list(first = TRUE, last = FALSE))
  # plan_labels by key: AGE stays, SEX is the report's
  expect_identical(r$content[[4L]]$args, list(AGE = "Age", SEX = "Gender"))
})

test_that("a report's content as R code (one !r) is read, written and wins whole", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "outputs"))
  writeLines(c("content:", "  - plan_stub: {name: row_label}"), file.path(d, "defaults.yml"))
  writeLines(c("id: T-1", "kind: table",
               "data: {source: code, code: !r 'data <- head(adsl)'}",
               "content: !r |", "  plan <- table_plan(data, cols = \"ARM\")"), file.path(d, "outputs", "T-1.yml"))
  o <- tfl_read_outputs(d)[["T-1"]]
  expect_s3_class(o$content, "tfl_r")
  expect_s3_class(o$data$code, "tfl_r")
  expect_output(print(o), "R code")
  r <- tfl_resolve_output(o, tfl_read_study_spec(d))
  expect_s3_class(r$content, "tfl_r")
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  expect_identical(as.character(tfl_read_output(f)$content), as.character(o$content))
  sh <- tfl_sheets_from_yaml(d)
  rp <- sh$table$report
  expect_match(rp$content_code[rp$output_id %in% "T-1"], "table_plan(data", fixed = TRUE)
  expect_identical(rp$data_code[rp$output_id %in% "T-1"], "data <- head(adsl)")
})

test_that("a YAML report's RTF path: the TOC's file, else the study's, in output_path", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "spec", "outputs"), recursive = TRUE)
  write_study_yml(dir, c("study_id: S-1", "settings:", "  output_path: output/tfl",
               "  file: '{output_id}.rtf'"))
  writeLines(c("outputs:", "  - id: T-1", "    kind: table",
               "  - id: T-2", "    kind: table", "    file: t2_{output_id}.rtf"),
             file.path(dir, "spec", "toc.yml"))
  s <- tfl_read_study_spec(dir)
  expect_identical(tfl_report_path(tfl_output("T-1", "table"), study = s), "output/tfl/T-1.rtf")
  expect_identical(tfl_report_path(tfl_output("T-2", "table"), study = s), "output/tfl/t2_T-2.rtf")
  # no settings: the report's id, no folder
  s$study$settings <- NULL
  expect_identical(tfl_report_path(tfl_output("T-1", "table"), study = s), "T-1.rtf")
  expect_error(tfl_report_path(tfl_output("T-1", "table")), "study")
})

test_that("a YAML report's RTF path: a file of the report's own, the study's output_path as it is", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "spec", "outputs"), recursive = TRUE)
  # (as a study converted from sheets with no default file: each report's
  # own file in the TOC, the study's output_path relative)
  write_study_yml(dir, c("study_id: S-1", "settings:", "  output_path: ../tables"))
  writeLines(c("outputs:", "  - id: T-1", "    kind: table", "    file: t_{output_id}.rtf",
               "  - id: T-2", "    kind: table"), file.path(dir, "spec", "toc.yml"))
  s <- tfl_read_study_spec(dir)
  expect_identical(tfl_report_path(tfl_output("T-1", "table"), study = s), "../tables/t_T-1.rtf")
  expect_identical(tfl_report_path(tfl_output("T-2", "table"), study = s), "../tables/T-2.rtf")
  # the report's own settings: file first
  o <- tfl_output("T-2", "table")
  o$settings <- list(file = "own_{output_id}.rtf")
  expect_identical(tfl_report_path(o, study = s), "../tables/own_T-2.rtf")
})

test_that("the defaults' content is a table's: a figure or a listing takes their page, not it", {
  s <- tfl_read_study_spec(yml_dir())
  for (kind in c("figure", "listing")) {
    o <- tfl_output("X-1", kind, content = list(tfl_item("plan_listing", list())))
    r <- tfl_resolve_output(o, s)
    # (none of the defaults'; a listing with no page size is given its 22 rows, #638;
    # a figure is given nothing)
    expect_identical(vapply(r$content, `[[`, "", "fn"),
                     if (kind == "listing") c("plan_listing", "plan_paginate_rows") else "plan_listing",
                     info = kind)
    # the page still comes from the defaults
    expect_identical(r$page$header[["1"]]$right, "{STATUS}", info = kind)
  }
  # a table keeps them
  o <- tfl_output("X-1", "table", content = list(tfl_item("table_plan", list())))
  expect_gt(length(tfl_resolve_output(o, s)$content), 1L)
})

test_that("page: rtf_tables' font size (an earlier converter's) is the plan's on resolve", {
  s <- tfl_read_study_spec(yml_dir())
  o <- tfl_output("T-5", "table", content = list(tfl_item("table_plan", list(cols = "TRT"))),
                  page = list(rtf_tables = list(font_size_half_points = 16L, auto_title = TRUE)))
  r <- tfl_resolve_output(o, s)
  sty <- Filter(function(it) identical(it$fn, "plan_style"), r$content)
  expect_identical(sty[[1L]]$args$font_size_half_points, 16L)
  expect_null(r$page$rtf_tables$font_size_half_points)
  expect_true(isTRUE(r$page$rtf_tables$auto_title))
  # the report's own plan_style key wins
  o$content <- c(o$content, list(tfl_item("plan_style", list(font_size_half_points = 18L))))
  r <- tfl_resolve_output(o, s)
  sty <- Filter(function(it) identical(it$fn, "plan_style"), r$content)
  expect_identical(sty[[1L]]$args$font_size_half_points, 18L)
})
