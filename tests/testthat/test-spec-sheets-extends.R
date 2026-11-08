# What a report extends goes through the sheets: the report row's `extends`
# cell (#689, part 2).  The `pattern` cell says the first pattern; the
# `extends` cell is there only when the chain is not what that alone reads
# back as, [first pattern, defaults].

# a study of fictional reports, one per chain; the defaults and the patterns
# set the token COMPANY (and others), so the winning layer shows
ex_study <- function(chains) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  dir.create(file.path(d, "outputs"))
  dir.create(file.path(d, "patterns"))
  dl <- readLines(file.path(src, "defaults.yml"))
  dl <- sub("tokens: {COMPANY: Sample Pharma,", "tokens: {COMPANY: FromDefaults, TONLYD: d,", dl, fixed = TRUE)
  writeLines(dl, file.path(d, "defaults.yml"))
  writeLines(c("page:", "  tokens: {COMPANY: FromP, TONLYP: p}"), file.path(d, "patterns", "p.yml"))
  writeLines(c("page:", "  tokens: {COMPANY: FromQ, TONLYQ: q}"), file.path(d, "patterns", "q.yml"))
  ol <- readLines(file.path(src, "outputs", "T-14-1-1.yml"))
  for (id in names(chains)) {
    x <- sub("id: T-14-1-1", paste0("id: ", id), ol, fixed = TRUE)
    x <- sub("extends: [defaults]", chains[[id]], x, fixed = TRUE)
    writeLines(x, file.path(d, "outputs", paste0(id, ".yml")))
  }
  d
}

ex_chains <- list(
  "T-flip" = "extends: [defaults, patterns/p]",
  "T-none" = "extends: none",
  "T-alone" = "extends: [patterns/p]",
  "T-two" = "extends: [patterns/p, patterns/q, defaults]",
  "T-twoflip" = "extends: [patterns/q, patterns/p]",
  "T-def" = "extends: [defaults]",
  "T-pdef" = "extends: [patterns/p, defaults]")

ex_chain <- function(d, id) as.character(tfl_read_outputs(d)[[id]]$extends)

ex_roundtrip <- function(d) {
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  to <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(tfl_yaml_from_sheets(sh, to))
  list(sheets = sh, dir = to)
}

ex_token <- function(d, id, tok = "COMPANY") {
  s <- tfl_read_study_spec(d)
  o <- tfl_resolve_output(tfl_read_outputs(d)[[id]], s)
  o$page$tokens[[tok]]
}

test_that("the chains of the issue's table come back as written, in memory", {
  d <- ex_study(ex_chains)
  rt <- ex_roundtrip(d)
  for (id in names(ex_chains)) {
    expect_identical(ex_chain(rt$dir, id), ex_chain(d, id), info = id)
  }
  expect_identical(ex_chain(rt$dir, "T-flip"), c("defaults", "patterns/p"))
  expect_identical(ex_chain(rt$dir, "T-none"), "none")
  expect_identical(ex_chain(rt$dir, "T-alone"), "patterns/p")
  expect_identical(ex_chain(rt$dir, "T-two"), c("patterns/p", "patterns/q", "defaults"))
})

test_that("the winning layer is the same after the round trip", {
  d <- ex_study(ex_chains)
  rt <- ex_roundtrip(d)
  for (id in names(ex_chains)) {
    for (tok in c("COMPANY", "TONLYD", "TONLYP", "TONLYQ")) {
      expect_identical(ex_token(rt$dir, id, tok), ex_token(d, id, tok), info = paste(id, tok))
    }
  }
  # COMPANY is set by the defaults and the pattern: the order decides
  expect_identical(ex_token(rt$dir, "T-flip"), "FromDefaults")
  expect_identical(ex_token(rt$dir, "T-pdef"), "FromP")
  expect_identical(ex_token(rt$dir, "T-two"), "FromP")
  expect_identical(ex_token(rt$dir, "T-twoflip"), "FromQ")
  expect_null(ex_token(rt$dir, "T-none"))
  expect_null(ex_token(rt$dir, "T-none", "TONLYD"))
  expect_identical(ex_token(rt$dir, "T-two", "TONLYQ"), "q")
})

test_that("the unchanged forms get no extends cell; the others do", {
  d <- ex_study(ex_chains)
  rp <- suppressMessages(tfl_sheets_from_yaml(d))$table$report
  cell <- function(id) rp$extends[rp$output_id %in% id]
  for (id in c("T-def", "T-pdef")) expect_true(is.na(cell(id)), info = id)
  expect_identical(cell("T-flip"), "defaults | patterns/p")
  expect_identical(cell("T-none"), "none")
  expect_identical(cell("T-alone"), "patterns/p")
  expect_identical(cell("T-two"), "patterns/p | patterns/q | defaults")
  # the pattern cell is the first pattern as before, for the app
  expect_identical(rp$pattern[rp$output_id %in% "T-two"], "@p")
  expect_identical(rp$pattern[rp$output_id %in% "T-alone"], "@p")
  expect_identical(rp$pattern[rp$output_id %in% "T-twoflip"], "@q")
  expect_true(is.na(rp$pattern[rp$output_id %in% "T-none"]))
  expect_identical(rp$pattern[rp$output_id %in% "T-flip"], "@p")
  # a study of only the unchanged forms: the cell is blank everywhere
  d2 <- ex_study(ex_chains[c("T-def", "T-pdef")])
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
  expect_identical(rb$extends[rb$output_id %in% "T-two"], "patterns/p | patterns/q | defaults")
  from <- withr::local_tempdir()
  file.copy(f, file.path(from, "table_spec.xlsx"))
  writeLines(c("study_id: EX-1", "title: A study", "phase: '2'"), file.path(from, "study.yml"))
  to <- withr::local_tempdir()
  suppressMessages(tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE))
  for (id in names(ex_chains)) {
    expect_identical(ex_chain(to, id), ex_chain(d, id), info = id)
  }
  expect_identical(ex_token(to, "T-flip"), "FromDefaults")
  expect_identical(ex_token(to, "T-two"), "FromP")
  expect_null(ex_token(to, "T-none"))
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

test_that("a pattern cell and an extends cell naming different patterns stop the conversion", {
  d <- ex_study(ex_chains[c("T-two", "T-none")])
  sh <- suppressMessages(tfl_sheets_from_yaml(d))
  to <- withr::local_tempdir()
  run <- function(s) suppressMessages(tfl_yaml_from_sheets(s, to, overwrite = TRUE))
  set <- function(s, col, id, v) {
    s$table$report[[col]][s$table$report$output_id %in% id] <- v
    s
  }
  # extends edited to another first pattern
  err <- tryCatch(run(set(sh, "extends", "T-two", "patterns/q | defaults")), error = conditionMessage)
  expect_match(err, "T-two", fixed = TRUE)
  expect_match(err, "@p", fixed = TRUE)
  expect_match(err, "patterns/q", fixed = TRUE)
  # pattern edited to another one
  expect_match(tryCatch(run(set(sh, "pattern", "T-two", "@q")), error = conditionMessage),
               "T-two", fixed = TRUE)
  # a blank pattern cell with an extends cell naming a pattern
  err3 <- tryCatch(run(set(sh, "pattern", "T-two", NA)), error = conditionMessage)
  expect_match(err3, "T-two", fixed = TRUE)
  expect_match(err3, "blank", fixed = TRUE)
  # a pattern in the pattern cell, an extends cell with none in it
  expect_match(tryCatch(run(set(sh, "extends", "T-two", "defaults")), error = conditionMessage),
               "T-two", fixed = TRUE)
  # an extends cell with no pattern in it and a blank pattern cell is fine
  expect_no_error(run(sh))
  expect_no_error(run(set(sh, "extends", "T-none", "defaults")))
  expect_identical(ex_chain(to, "T-none"), "defaults")
})
