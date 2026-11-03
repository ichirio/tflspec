# The trial's issues: a missing data file named (tflplanner#515), a
# warning's reason in the manifest (#511), the tool's own assumption texts in
# the study's language (#513)

ti_book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

ti_study <- function(root) {
  skip_if_not_installed("haven")
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  suppressMessages(tfl_ai_prepare(unit = "toc", d, ti_book(), adam = ad, study_id = "XYZ-ONC-01"))
  list(dir = d, adam = ad)
}

# data.yml's `.xpt` paths as `.rds` (the files are not there)
ti_rds_paths <- function(d) {
  f <- file.path(d, "spec", "data.yml")
  txt <- readLines(f, encoding = "UTF-8")
  writeLines(sub("[.]xpt$", ".rds", txt), f, useBytes = TRUE)
  invisible(f)
}

test_that("a data.yml path with no file is named: the dataset, the path, the file that is there", {
  skip_on_cran()
  s <- ti_study(withr::local_tempdir())
  ai_drafts_from_fixtures(s$dir, only = "demographics-1")
  suppressMessages(tfl_ai_adopt(s$dir))
  ti_rds_paths(s$dir)
  st <- tfl_read_study_spec(s$dir)
  msg <- tflspec:::.data_missing_message(st$data$datasets, dir = s$dir)
  expect_match(msg, "^AD[A-Z]+: no file at .*[.]rds \\(spec/data[.]yml\\)\\. Point it at the study's file, e[.]g\\. .*[.]xpt[.]$")
  e <- tryCatch(suppressMessages(tfl_build_ard(st, dir = s$dir, save = FALSE)), error = function(e) e)
  expect_s3_class(e, "error")
  expect_false(grepl("cannot open the connection", conditionMessage(e), fixed = TRUE))
  expect_match(conditionMessage(e), "^AD[A-Z]+: no file at .*[.]rds \\(spec/data[.]yml\\)")
  expect_match(conditionMessage(e), "e[.]g\\. .*[.]xpt[.]$")
  # no file of that name in another type: no example
  m <- tflspec:::.data_missing_message(list(list(dataset = "ADXX", path = "adam/adxx.rds")), dir = s$dir)
  expect_identical(m, "ADXX: no file at adam/adxx.rds (spec/data.yml). Point it at the study's file.")
  # a file that is there: nothing to say
  expect_null(tflspec:::.data_missing_message(list(list(dataset = "ADSL", path = "adam/adsl.xpt")),
                                              dir = dirname(s$adam)))
})

test_that("tfl_ai_prepare(): data.yml's .rds paths against the .xpt files given: a warning and the table", {
  skip_on_cran()
  s <- ti_study(withr::local_tempdir())
  # as made: the paths are the files given
  r0 <- suppressMessages(tfl_ai_prepare(unit = "toc", s$dir, ti_book(), adam = s$adam))
  expect_s3_class(r0$data_paths, "data.frame")
  expect_true(all(r0$data_paths$exists))
  expect_false(any(r0$data_paths$differs))
  f <- ti_rds_paths(s$dir)
  before <- readLines(f, encoding = "UTF-8")
  expect_warning(
    r <- suppressMessages(tfl_ai_prepare(unit = "toc", s$dir, ti_book(), adam = s$adam)),
    "ADSL: spec/data.yml has .*adsl[.]rds \\(no such file\\); the ADaM given is .*adsl[.]xpt")
  dp <- r$data_paths
  expect_named(dp, c("dataset", "in_data_yml", "given", "exists", "differs"))
  expect_true(all(c("ADSL", "ADAE") %in% dp$dataset))
  expect_false(any(dp$exists))
  expect_true(all(dp$differs))
  expect_true(all(grepl("[.]rds$", dp$in_data_yml)))
  expect_true(all(grepl("[.]xpt$", dp$given)))
  # data.yml is not changed
  expect_identical(readLines(f, encoding = "UTF-8"), before)
  # a new study has none
  new <- file.path(dirname(s$dir), "new")
  dir.create(new)
  r2 <- suppressMessages(tfl_ai_prepare(unit = "toc", new, ti_book(), adam = s$adam, dry_run = TRUE))
  expect_null(r2$data_paths)
})

test_that("the manifest: a warning or an error has its reason, a table and a figure alike", {
  skip_on_cran()
  s <- ti_study(withr::local_tempdir())
  ai_drafts_from_fixtures(s$dir)
  res0 <- suppressMessages(tfl_ai_collect_answers(s$dir))
  expect_true("reason" %in% names(res0$manifest))
  expect_true(all(is.na(res0$manifest$reason[res0$manifest$status == "ok"])))
  area <- file.path(s$dir, "ai", "spec", "outputs")
  # a table with no label is a warning (its title), a figure naming a
  # dataset not given too
  tb <- grep("^T-", list.files(area), value = TRUE)[1L]
  tf <- file.path(area, tb)
  txt <- readLines(tf, encoding = "UTF-8")
  writeLines(txt[!grepl("^label:", txt)], tf, useBytes = TRUE)
  ff <- file.path(area, "F-14-2-2.yml")
  ftxt <- readLines(ff, encoding = "UTF-8")
  writeLines(sub("dataset: ADTTE", "dataset: ADXX", ftxt, fixed = TRUE), ff, useBytes = TRUE)
  res <- suppressMessages(tfl_ai_collect_answers(s$dir))
  man <- res$manifest
  bad <- man$status %in% c("warning", "error")
  expect_true(any(man$status == "warning" & grepl("^T-", man$output_id)))
  expect_true(any(bad & man$output_id == "F-14-2-2"))
  expect_true(all(!is.na(man$reason[bad]) & nzchar(man$reason[bad])))
  expect_true(all(is.na(man$reason[man$status == "ok"])))
  # as written to the file
  csv <- utils::read.csv(file.path(s$dir, res$manifest_file), encoding = "UTF-8", na.strings = "")
  expect_identical(names(csv)[length(names(csv))], "reason")
  expect_true(all(!is.na(csv$reason[csv$status %in% c("warning", "error")])))
})

test_that("the tool's own width assumption is in the study's language", {
  en <- tflspec:::.ai_widths_assumptions("T-1", "en")
  ja <- tflspec:::.ai_widths_assumptions("T-1", "ja")
  expect_match(en$assumption, "^Column widths: the default proportions")
  expect_true(grepl("\u5217\u5e45", ja$assumption))
  expect_false(grepl("Column widths", ja$assumption, fixed = TRUE))
  expect_true(grepl("\u5217\u5e45", ja$question))
  expect_identical(ja$value, en$value)
  expect_identical(ja$item, "widths")
  expect_true(tflspec:::.ai_is_ja("Japanese"))
  expect_false(tflspec:::.ai_is_ja(NULL))
})

test_that("a Japanese study's width assumption, as the unpack writes it, is Japanese", {
  skip_on_cran()
  s <- ti_study(withr::local_tempdir())
  tflspec:::.ai_study_yml_set(s$dir, "language", "ja")
  expect_identical(tflspec:::.ai_spec_settings(s$dir)$language, "ja")
  # a table answer with no widths, unpacked: its assumptions
  txt <- readLines(test_path("fixtures", "answers-toc", "demographics-1.md"), encoding = "UTF-8")
  a <- tfl_ai_parse(txt, "table")
  unset <- tflspec:::.ai_widths_unset(a$outputs)
  skip_if(!length(unset), "the fixture's reports carry widths")
  aw <- tflspec:::.ai_widths_assumptions(unset, tflspec:::.ai_spec_settings(s$dir)$language)
  expect_true(all(grepl("\u5217\u5e45", aw$assumption)))
})

test_that("AGENTS.md has the language rule with the draft rules and in Before you stop", {
  f <- system.file("ai", "agent", "AGENTS.md", package = "tflspec")
  skip_if(!nzchar(f))
  txt <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  rule <- "in the chat's language"
  at <- gregexpr(rule, txt, fixed = TRUE)[[1L]]
  expect_length(at, 2L)
  stop_at <- regexpr("## Before you stop", txt, fixed = TRUE)
  expect_true(sum(at < stop_at) == 1L && sum(at > stop_at) == 1L)
})
