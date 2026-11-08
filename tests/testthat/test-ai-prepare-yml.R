# ai/prepare.yml: tfl_ai_prepare() keeps its arguments with the study;
# tfl_ai_prepare(dir) repeats them (tflplanner#512)

pyml_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  # the workbook inside the study folder, so its path is kept from there
  file.copy(system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
            file.path(d, "onco_toc_shell.xlsx"))
  d
}
pyml_run <- function(d, ...) suppressMessages(tfl_ai_prepare(d, ..., unit = "toc", quiet = TRUE))
pyml_files <- function(d) {
  f <- c(list.files(file.path(d, "ai", "requests"), full.names = TRUE),
         file.path(d, "ai", "agent_prompt.md"))
  stats::setNames(unname(tools::md5sum(f)), basename(f))
}

test_that("the call's arguments are written to ai/prepare.yml, paths from the study", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  pyml_run(d, file.path(d, "onco_toc_shell.xlsx"), study_id = "XYZ-ONC-01", language = "en",
           agent_reads = c("workbook", "adam"), size = 1L, overwrite = TRUE)
  f <- file.path(d, "ai", "prepare.yml")
  expect_true(file.exists(f))
  y <- yaml::read_yaml(f)
  expect_identical(names(y)[1:3], c("version", "written", "tflspec"))
  expect_identical(y$version, 1L)
  expect_identical(y$tflspec, as.character(utils::packageVersion("tflspec")))
  expect_match(y$written, "^\\d{4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d$")
  expect_identical(y$toc, "onco_toc_shell.xlsx")
  expect_identical(y$study_id, "XYZ-ONC-01")
  expect_identical(y$agent_reads, c("workbook", "adam"))
  expect_identical(y$unit, "toc")
  # only what was given; nothing about the call itself
  expect_false(any(c("dir", "overwrite", "dry_run", "quiet", "progress", "add", "adam",
                     "templates", "adam_summary", "numbers") %in% names(y)))
  # in the keys' order, strings quoted, the comment says what is left out
  expect_identical(names(y), c("version", "written", "tflspec", "toc", "study_id", "size",
                               "language", "unit", "agent_reads"))
  txt <- readLines(f, warn = FALSE)
  expect_true(any(grepl('^toc: "onco_toc_shell.xlsx"$', txt)))
  expect_true(any(grepl("^# Not kept: dir, overwrite", txt)))
  # the reader: absolute again, the kinds back
  s <- tfl_ai_prepare_settings(d)
  expect_identical(s$toc, normalizePath(file.path(d, "onco_toc_shell.xlsx"), winslash = "/"))
  expect_identical(s$agent_reads, c("workbook", "adam"))
  expect_identical(s$size, 1L)
  expect_identical(attr(s, "tflspec"), y$tflspec)
  expect_null(tfl_ai_prepare_settings(file.path(d, "nowhere")))
})

test_that("a path outside the study is written as given, absolute", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  out <- withr::local_tempdir()
  wb <- file.path(out, "book.xlsx")
  file.copy(file.path(d, "onco_toc_shell.xlsx"), wb)
  pyml_run(d, wb, adam = "adam")
  y <- yaml::read_yaml(file.path(d, "ai", "prepare.yml"))
  expect_identical(y$toc, normalizePath(wb, winslash = "/"))
  expect_identical(y$adam, "adam")
})

test_that("a dry run writes no file", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  pyml_run(d, file.path(d, "onco_toc_shell.xlsx"), dry_run = TRUE)
  expect_false(file.exists(file.path(d, "ai", "prepare.yml")))
  expect_false(dir.exists(file.path(d, "ai")))
})

test_that("tfl_ai_prepare(dir) repeats the last call, the requests the same bytes", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  r1 <- pyml_run(d, file.path(d, "onco_toc_shell.xlsx"), study_id = "XYZ-ONC-01", size = 2L)
  before <- pyml_files(d)
  y1 <- yaml::read_yaml(file.path(d, "ai", "prepare.yml"))
  r2 <- pyml_run(d)
  expect_identical(pyml_files(d), before)
  expect_identical(r2$requests, r1$requests)
  y2 <- yaml::read_yaml(file.path(d, "ai", "prepare.yml"))
  expect_identical(y2[setdiff(names(y2), "written")], y1[setdiff(names(y1), "written")])
  # and without quiet = TRUE, the messages as a call's
  expect_no_error(suppressMessages(tfl_ai_prepare(d)))
  expect_identical(pyml_files(d), before)
})

test_that("an argument given when repeating wins, and is kept", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  r1 <- pyml_run(d, file.path(d, "onco_toc_shell.xlsx"), size = 1L, language = "en")
  r2 <- pyml_run(d, size = 3L)
  # (size 3: the requests are named after their families, not their reports)
  expect_false(identical(r2$requests$request, r1$requests$request))
  y <- yaml::read_yaml(file.path(d, "ai", "prepare.yml"))
  expect_identical(y$size, 3L)
  expect_identical(y$toc, "onco_toc_shell.xlsx")
  expect_identical(y$language, "en")
  # NULL takes a setting out of the file's
  pyml_run(d, language = NULL)
  expect_false("language" %in% names(yaml::read_yaml(file.path(d, "ai", "prepare.yml"))))
})

test_that("a file moved: the error names the file and the setting", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  pyml_run(d, file.path(d, "onco_toc_shell.xlsx"))
  file.rename(file.path(d, "onco_toc_shell.xlsx"), file.path(d, "moved.xlsx"))
  err <- tryCatch(pyml_run(d), error = conditionMessage)
  expect_match(err, "prepare.yml", fixed = TRUE)
  expect_match(err, "`toc`", fixed = TRUE)
  expect_match(err, "onco_toc_shell.xlsx", fixed = TRUE)
  # giving it again repeats the rest
  expect_no_error(pyml_run(d, file.path(d, "moved.xlsx")))
  expect_identical(yaml::read_yaml(file.path(d, "ai", "prepare.yml"))$toc, "moved.xlsx")
})

test_that("no file and no toc: toc is asked for, and the file's absence said", {
  d <- withr::local_tempdir()
  expect_error(tfl_ai_prepare(d), "argument \"toc\" is missing, with no default")
  expect_error(tfl_ai_prepare(d), "ai/prepare.yml is not there")
  expect_error(tfl_ai_prepare(file.path(d, "nowhere"), quiet = TRUE), "\"toc\" is missing")
  # a toc given: as before
  expect_error(tfl_ai_prepare(d, file.path(d, "nope.xlsx")), "workbook's path")
})

test_that("a newer format version is not read", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "ai"))
  writeLines(c("version: 2", 'toc: "a.xlsx"'), file.path(d, "ai", "prepare.yml"))
  expect_error(tfl_ai_prepare_settings(d), "version 2")
})

test_that("the kinds of value round-trip; what is not plain is left out and named", {
  d <- withr::local_tempdir()
  given <- list(toc = file.path(d, "t.xlsx"), toc_sheet = "TOC", sheets = c("A 1", "B"),
                shells = file.path(d, "shells", "s.xlsx"),
                toc_cols = c(output_id = "Output No", title = "Title 1", title = "Title 2"),
                sheet_roles = c("Topline TOC" = "other"), docs = file.path(d, "docs", "sap.docx"),
                datasets = data.frame(x = 1), max_levels = 10, templates = FALSE,
                area = TRUE, title = "Study: a \"quoted\" title")
  tflspec:::.ai_prepare_write(d, given)
  txt <- readLines(file.path(d, "ai", "prepare.yml"), warn = FALSE)
  expect_true(any(grepl("^# Left out, not plain text: datasets[.]$", txt)))
  expect_false(any(grepl("^datasets:", txt)))
  s <- tfl_ai_prepare_settings(d)
  expect_identical(s$shells, normalizePath(given$shells, winslash = "/", mustWork = FALSE))
  expect_identical(s$sheets, c("A 1", "B"))
  expect_identical(s$toc_cols, list(output_id = "Output No", title = c("Title 1", "Title 2")))
  expect_identical(s$sheet_roles, c("Topline TOC" = "other"))
  expect_identical(s$max_levels, 10L)
  expect_false(s$templates)
  expect_identical(s$title, "Study: a \"quoted\" title")
  expect_false("datasets" %in% names(s))
  y <- yaml::read_yaml(file.path(d, "ai", "prepare.yml"))
  expect_identical(y$toc, "t.xlsx")
  expect_identical(y$shells, "shells/s.xlsx")
})

test_that("add: the first workbook's settings are kept, docs joined; no file, none written", {
  d <- withr::local_tempdir()
  w <- tflspec:::.ai_prepare_write
  w(d, list(toc = file.path(d, "b2.xlsx")), add = TRUE)
  expect_false(file.exists(file.path(d, "ai", "prepare.yml")))
  w(d, list(toc = file.path(d, "b1.xlsx"), sheets = "S1", language = "en",
            docs = file.path(d, "docs", "a.md")))
  w(d, list(toc = file.path(d, "b2.xlsx"), sheets = "S2", language = "ja",
            docs = file.path(d, "docs", "b.md"), size = 2L), add = TRUE)
  s <- tfl_ai_prepare_settings(d)
  expect_identical(basename(s$toc), "b1.xlsx")
  expect_identical(s$sheets, "S1")
  expect_identical(s$language, "ja")
  expect_identical(s$size, 2L)
  expect_identical(basename(s$docs), c("a.md", "b.md"))
  expect_identical(names(yaml::read_yaml(file.path(d, "ai", "prepare.yml")))[1:3],
                   c("version", "written", "tflspec"))
})

test_that("the solver's files do not carry ai/prepare.yml", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- pyml_study(withr::local_tempdir())
  pyml_run(d, file.path(d, "onco_toc_shell.xlsx"))
  read <- c(list.files(file.path(d, "ai", "requests"), full.names = TRUE),
            file.path(d, c("AGENTS.md", "ai/AGENTS.md", "ai/agent_prompt.md", "run.R",
                           ".github/copilot-instructions.md")))
  txt <- unlist(lapply(read[file.exists(read)], readLines, warn = FALSE, encoding = "UTF-8"))
  expect_false(any(grepl("prepare.yml", txt, fixed = TRUE)))
})
