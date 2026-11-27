# What a report extends goes through the sheets: the report row's `extends`
# cell (#689, part 2), there only when the chain is not [defaults], which a
# blank cell reads back as.  A pattern is an old form (canon 11-12 :380): a
# `patterns/<name>` in the extends cell and a `pattern` cell that names no
# page configuration stop the conversion.

# a study of fictional reports, one per chain
ex_study <- function(chains) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  dir.create(file.path(d, "outputs"))
  file.copy(file.path(src, "defaults.yml"), file.path(d, "defaults.yml"))
  ol <- readLines(file.path(src, "outputs", "T-14-1-1.yml"))
  for (id in names(chains)) {
    x <- sub("id: T-14-1-1", paste0("id: ", id), ol, fixed = TRUE)
    x <- sub("extends: [defaults]", chains[[id]], x, fixed = TRUE)
    writeLines(x, file.path(d, "outputs", paste0(id, ".yml")))
  }
  d
}

ex_chains <- list(
  "T-none" = "extends: none",
  "T-map" = "extends: {content: none}",
  "T-def" = "extends: [defaults]")

ex_chain <- function(d, id) tfl_read_outputs(d)[[id]]$extends

ex_roundtrip <- function(d) {
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  to <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(tfl_yaml_from_sheets(sh, to))
  list(sheets = sh, dir = to)
}

test_that("the chains come back as written, in memory", {
  d <- ex_study(ex_chains)
  rt <- ex_roundtrip(d)
  for (id in names(ex_chains)) {
    expect_identical(ex_chain(rt$dir, id), ex_chain(d, id), info = id)
  }
  expect_identical(ex_chain(rt$dir, "T-none"), "none")
  expect_identical(ex_chain(rt$dir, "T-map"), list(content = "none"))
})

test_that("the unchanged form gets no extends cell; the others do", {
  d <- ex_study(ex_chains)
  rp <- suppressMessages(tfl_sheets_from_yaml(d))$table$report
  cell <- function(id) rp$extends[rp$output_id %in% id]
  expect_true(is.na(cell("T-def")))
  expect_identical(cell("T-none"), "none")
  expect_identical(cell("T-map"), "content=none")
  # (no report picks a page configuration of its own: no pattern cell)
  expect_true(all(is.na(rp$pattern[rp$output_id %in% names(ex_chains)])))
  d2 <- ex_study(ex_chains["T-def"])
  rp2 <- suppressMessages(tfl_sheets_from_yaml(d2))$table$report
  expect_true(all(is.na(rp2$extends)))
})

test_that("the chains also go through the spec workbook", {
  skip_if_not_installed("openxlsx")
  d <- ex_study(ex_chains)
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tfl_write_specs(f, table = sh$table, report = sh$table))
  wb <- openxlsx::loadWorkbook(f)
  openxlsx::addWorksheet(wb, "_tflplanner")
  openxlsx::writeData(wb, "_tflplanner", sh$planner, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  # the workbook holds the report sheet's cell
  rb <- tfl_read_report_spec(f)$report
  expect_identical(rb$extends[rb$output_id %in% "T-none"], "none")
  from <- withr::local_tempdir()
  file.copy(f, file.path(from, "table_spec.xlsx"))
  write_study_yml(from, c("study_id: EX-1", "title: A study", "phase: '2'"))
  to <- withr::local_tempdir()
  suppressMessages(tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE))
  for (id in names(ex_chains)) {
    expect_identical(ex_chain(to, id), ex_chain(d, id), info = id)
  }
})

test_that("a report with no extends: reads as [defaults] and comes back unchanged", {
  d <- ex_study(list("T-x" = "extends: [defaults]"))
  p <- file.path(d, "outputs", "T-x.yml")
  writeLines(grep("^extends:", readLines(p), value = TRUE, invert = TRUE), p)
  expect_identical(ex_chain(d, "T-x"), "defaults")
  rt <- ex_roundtrip(d)
  expect_identical(ex_chain(rt$dir, "T-x"), "defaults")
  expect_true(is.na(rt$sheets$table$report$extends[rt$sheets$table$report$output_id %in% "T-x"]))
})

test_that("a pattern in the sheets stops the conversion: an old form", {
  d <- ex_study(ex_chains)
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  to <- withr::local_tempdir()
  run <- function(s) suppressMessages(tfl_yaml_from_sheets(s, to, overwrite = TRUE))
  set <- function(s, col, id, v) {
    s$table$report[[col]][s$table$report$output_id %in% id] <- v
    s
  }
  # a pattern in the extends cell, a chain or a step's
  for (v in c("patterns/p | defaults", "content=patterns/p")) {
    e <- tryCatch(run(set(sh, "extends", "T-none", v)), error = function(e) e)
    expect_s3_class(e, "tflspec_old_form")
    expect_identical(e$form, "extends_pattern")
    expect_match(conditionMessage(e), "T-none", fixed = TRUE)
  }
  # a pattern cell that names no page configuration: the sheets' own check
  # stops it first (the conversion's old-form stop is behind it)
  expect_error(run(set(sh, "pattern", "T-none", "@p")), "no page pattern of that name")
  # the sheets as written convert
  expect_no_error(run(sh))
})
