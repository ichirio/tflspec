# tfl_ai_prepare() and tfl_ai_write_requests(unit = "toc", workbook = ): the workbook
# attached, the requests saying where each shell is in it (#364)

prep_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  d
}
book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
req_text <- function(d, r) {
  paste(readLines(file.path(d, r$requests$file[1L]), encoding = "UTF-8"), collapse = "\n")
}

test_that("tfl_ai_prepare(): the guide's step 3 in one call; a study.yml there is kept", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_study(withr::local_tempdir())
  r <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "XYZ-ONC-01"))
  expect_identical(nrow(r$requests), 5L)
  expect_true(all(file.exists(file.path(d, c("study.yml", "spec/data.yml", "ai/adam_summary.md",
                                             "ai/toc_shell.md", "ai/requests/index.md")))))
  # again: the study's settings are kept, the requests written again
  y <- file.path(d, "study.yml")
  writeLines(c(readLines(y), "# kept"), y)
  r2 <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "OTHER", size = 1L))
  expect_true("# kept" %in% readLines(y))
  expect_identical(nrow(r2$requests), 5L)
  # and the drafts of the example still pass, as in the guide (its
  # requests: the default size's, size = 1 names them after the reports)
  suppressMessages(tfl_ai_prepare(unit = "toc", d, book()))
  ai_drafts_from_fixtures(d)
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(unique(res$reports$status), "ok")
  expect_error(tfl_ai_prepare(unit = "toc", d, file.path(d, "nope.xlsx")), "workbook's path")
})

test_that("workbook = \"text\": the workbook's text in each request", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_study(withr::local_tempdir())
  r_text <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "XYZ-ONC-01"))
  t_text <- req_text(d, r_text)
  expect_false(grepl("is attached to this message", t_text, fixed = TRUE))
  expect_match(t_text, "# Workbook onco_toc_shell.xlsx", fixed = TRUE)
  expect_error(tfl_ai_prepare(unit = "toc", d, book(), workbook = "attach"), "should be one of")
})

test_that("the places: a report's rows of a sheet with several, else the whole sheet", {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(No = c("Table 14.1.1", "Table 14.1.2", "Table 14.3.1"),
                                            Title = c("Demographics", "Disposition", "AEs")))
  openxlsx::addWorksheet(wb, "Shells")
  openxlsx::writeData(wb, "Shells", data.frame(x = c("Table 14.1.1", "Age", "Sex", "", "Table 14.1.2",
                                                     "Completed")), colNames = FALSE)
  openxlsx::addWorksheet(wb, "T14.3.1")
  openxlsx::writeData(wb, "T14.3.1", data.frame(x = c("Table 14.3.1", "SOC", "PT")), colNames = FALSE)
  openxlsx::saveWorkbook(wb, f)
  x <- tfl_ai_read_shell(f)
  w <- tflspec:::.ai_shell_where(x, c("Table 14.1.1", "Table 14.1.2", "Table 14.3.1"))
  expect_match(w[1L], "sheet 'Shells', cells A1:A3", fixed = TRUE)
  expect_match(w[2L], "sheet 'Shells', cells A5:A6", fixed = TRUE)
  expect_match(w[3L], "sheet 'T14.3.1', the whole sheet", fixed = TRUE)
  expect_match(w[1L], "its TOC row: sheet 'TOC', row 2", fixed = TRUE)
})

test_that("tfl_ai_prepare(): a list for a screen; dry_run writes nothing; the ADaM summary reused", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_study(withr::local_tempdir())
  dry <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "XYZ-ONC-01", dry_run = TRUE))
  expect_s3_class(dry, "tfl_ai_prepared")
  expect_named(dry, c("requests", "toc", "shells", "counts", "sheets", "numbers", "adam", "files", "hint",
                      "next_steps", "dry_run", "agent_prompt", "agent_files", "templates", "workbook"))
  expect_type(dry$agent_prompt, "character")
  expect_identical(dry$agent_files, character())
  expect_false(file.exists(file.path(d, "study.yml")))
  expect_false(dir.exists(file.path(d, "ai")))
  expect_length(dry$files, 0L)
  expect_true(all(is.na(dry$requests$file)))
  expect_match(dry$next_steps[length(dry$next_steps)], "dry_run = FALSE")
  expect_named(dry$toc, c("toc_id", "title", "type", "toc_sheets", "sheets", "shell",
                          "shell_found", "request", "book", "shell_id", "population",
                          "program", "file", "info"))
  expect_true(all(dry$toc$shell_found))
  expect_setequal(dry$toc$request, dry$requests$request)
  expect_s3_class(dry$adam, "tfl_ai_adam")
  expect_output(print(dry), "dry run")
  # the define given: its value level in the summary
  skip_if_not_installed("xml2")
  dd <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "XYZ-ONC-01", dry_run = TRUE,
                                        define = test_path("fixtures", "define", "define-vl.xml")))
  expect_identical(nrow(dd$adam$value_level), 3L)
  expect_match(format(dd$adam), "## Value level (define.xml)", fixed = TRUE)
  # written, the ADaM summary of the look before passed back
  r <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "XYZ-ONC-01",
                                       adam_summary = dry$adam))
  expect_identical(r$requests$request, dry$requests$request)
  expect_identical(r$toc$request, dry$toc$request)
  expect_true(all(c("study.yml", "ai/adam_summary.md", "ai/toc_shell.md",
                    "ai/requests/index.md", r$requests$file) %in% r$files))
  expect_true(all(file.exists(file.path(d, r$files))))
  expect_match(r$next_steps[length(r$next_steps)], "tfl_ai_collect_answers()", fixed = TRUE)
  expect_error(tfl_ai_prepare(unit = "toc", d, book(), adam_summary = list()), "tfl_ai_adam_summary")
})

test_that("tfl_ai_collect_answers(): the drafts, the files, the repairs, the assumptions", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- prep_study(withr::local_tempdir())
  r <- suppressMessages(tfl_ai_prepare(unit = "toc", d, book(), study_id = "XYZ-ONC-01"))
  ex <- test_path("fixtures", "answers-toc")
  one <- r$requests$request[1L]
  txt <- readLines(file.path(ex, "demographics-1.md"), encoding = "UTF-8")
  ai_drafts_from_answer(d, txt, "table")
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_s3_class(res, "tfl_ai_collected")
  expect_named(res, c("reports", "next_steps", "next_kind", "next_args", "assumptions", "stray",
                      "requests", "templates", "toc_items", "manifest", "manifest_file"))
  # the step's kind and values beside its text, the same names
  expect_identical(names(res$next_kind), names(res$next_steps))
  expect_identical(names(res$next_args), names(res$next_steps))
  expect_true(all(res$next_kind %in% c("answer", "fix_answer", "answer_again", "repair",
                                       "see_problem", "warnings", "done", "sheet_missing",
                                       "add_missing", "none_asked")))
  expect_identical(unname(res$next_kind[res$reports$request[res$reports$status == "no answer"][1L]]),
                   "answer")
  expect_true(file.exists(file.path(d, res$manifest_file)))
  rp <- res$reports
  expect_true(all(c("problems_text", "file", "repair") %in% names(rp)))
  done <- rp$request == one
  expect_true(all(file.exists(file.path(d, rp$file[done]))))
  expect_true(all(is.na(rp$file[!done])))
  expect_true(all(rp$status[!done] == "no answer"))
  expect_identical(names(res$next_steps), r$requests$request)
  expect_named(res$assumptions, c("output_id", "where", "item", "assumption", "value",
                                  "question", "id", "kind"))
  expect_output(print(res), "no answer")
  # a draft with an error: its repair prompt, every problem; the draft stays
  bad <- sub("population: [A-Z]+", "population: NOPE", txt)
  ai_drafts_from_answer(d, bad, "table")
  res2 <- suppressMessages(tfl_ai_collect_answers(d))
  rp2 <- res2$reports[res2$reports$request == one, ]
  expect_true(all(rp2$status == "error"))
  expect_true(all(rp2$repair == paste0("ai/repairs/", one, ".md")))
  expect_true(file.exists(file.path(d, rp2$repair[1L])))
  expect_true(any(grepl("NOPE", rp2$problems_text)))
  expect_true(file.exists(file.path(d, "ai", "spec", "outputs", paste0(one, ".yml"))))
})

test_that("tfl_ai_prepare(quiet, datasets); tfl_ai_requests(); no ADaM folder in the requests", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  # the ADaM outside the study, by its full path
  ad <- file.path(prep_study(file.path(root, "secret-server")), "adam")
  ad <- normalizePath(ad, winslash = "/")
  expect_no_message(dry <- tfl_ai_prepare(unit = "toc", d, book(), adam = ad, dry_run = TRUE, quiet = TRUE))
  expect_type(dry$hint, "character")
  expect_setequal(dry$adam$datasets$dataset, c("ADAE", "ADLB", "ADRS", "ADSL", "ADTTE"))
  expect_no_message(r <- tfl_ai_prepare(unit = "toc", d, book(), adam = ad, adam_summary = dry$adam,
                                        datasets = c("ADSL", "ADAE"), max_levels = 5L,
                                        quiet = TRUE))
  expect_setequal(r$adam$datasets$dataset, c("ADSL", "ADAE"))
  txt <- unlist(lapply(file.path(d, r$requests$file), readLines, encoding = "UTF-8"))
  expect_false(any(grepl("secret-server", txt, fixed = TRUE)))
  # the study's own file keeps the full path
  expect_true(any(grepl("secret-server", readLines(file.path(d, "spec", "data.yml")), fixed = TRUE)))
  # read again, as written
  expect_identical(tfl_ai_requests(d), r$requests)
  expect_identical(nrow(tfl_ai_requests(file.path(root, "none"))), 0L)
})

test_that(".ai_data_yml_names(): a dataset's path as its file's name", {
  y <- paste("datasets:", "  - dataset: ADSL", "    path: //server/study/adam/adsl.xpt",
             "  - dataset: ADAE", "    path: 'C:/adam/adae.xpt'  # the server copy",
             "  - {dataset: ADLB, level: ADaM}", sep = "\n")
  out <- strsplit(.ai_data_yml_names(y), "\n", fixed = TRUE)[[1L]]
  expect_identical(out[3:5], c("    path: adsl.xpt", "  - dataset: ADAE", "    path: adae.xpt"))
  expect_null(.ai_data_yml_names(NULL))
})
