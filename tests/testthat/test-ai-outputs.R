# The AI's reports listed for the person (tfl_ai_outputs()), the list the
# person expects (ai/expected_outputs.csv) and run.R's comparison with it

outs_study <- function(root) {
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  book <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  tfl_ai_prepare(unit = "toc", d, book, adam = normalizePath(ad, winslash = "/"),
                 quiet = TRUE, mode = "agent", size = 3L)
  invisible(tfl_ai_migrate(d))
  file.copy(list.files(test_path("fixtures", "answers-toc"), full.names = TRUE),
            file.path(d, "ai", "answers"))
  d
}

test_that("tfl_ai_outputs(): a row a report, its request's sheets, status, questions, adoption", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- outs_study(root)
  res <- suppressMessages(tfl_ai_run(d, rtf = FALSE))
  o <- tfl_ai_outputs(d)
  expect_named(o, c("sheet", "shell_code", "toc_id", "output_id", "title", "population",
                    "split", "request", "status", "rtf", "problem", "questions",
                    "adopted", "file"))
  made <- res$reports$output_id[!is.na(res$reports$file)]
  expect_true(all(made %in% o$output_id))
  expect_true(all(startsWith(stats::na.omit(o$file), "ai/spec/outputs/")))
  expect_true(all(o$adopted == "no"))
  expect_true(all(o$status %in% c("ok", "warning", "error", "no answer", "not answered", "draft")))
  expect_true(any(!is.na(o$population)))
  # no list yet: run.R compares nothing
  expect_null(res$expected)
  # adopted: so; the AI redrafting it later: "draft newer"
  id <- made[1L]
  invisible(tfl_ai_adopt(d, id))
  expect_identical(tfl_ai_outputs(d)$adopted[match(id, o$output_id)], "yes")
  f <- file.path(d, o$file[match(id, o$output_id)])
  writeLines(c(readLines(f, warn = FALSE), "# redrafted"), f)
  expect_identical(tfl_ai_outputs(d)$adopted[match(id, o$output_id)], "yes, draft newer")
})

test_that("the person's list: written once from the AI's, read back; run.R says what differs", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- outs_study(root)
  suppressMessages(tfl_ai_run(d, rtf = FALSE))
  f <- tfl_ai_expected_write(d)
  expect_true(file.exists(f))
  expect_error(tfl_ai_expected_write(d), "overwrite = TRUE")
  e <- tfl_ai_expected_read(d)
  expect_named(e, c("sheet", "toc_id", "output_id", "title", "population", "split",
                    "action", "was", "reason", "note"))
  have <- e$output_id[!is.na(e$output_id)]
  made <- tfl_ai_outputs(d)
  made <- made$output_id[!is.na(made$file)]
  expect_gte(length(made), 3L)
  # the person: one dropped, one's title changed, one added, one taken out
  # of the list altogether
  e$action[e$output_id == made[1L]] <- "drop"
  e$reason[e$output_id == made[1L]] <- "not wanted"
  e$action[e$output_id == made[2L]] <- "change"
  e$was[e$output_id == made[2L]] <- paste("title:", e$title[e$output_id == made[2L]])
  e$title[e$output_id == made[2L]] <- "A Title Of The Person's"
  e <- e[e$output_id != made[3L], , drop = FALSE]
  e <- rbind(e, data.frame(sheet = NA, toc_id = "Table 14.9.9", output_id = "T-14-9-9",
                           title = "New one", population = "SAF", split = NA, action = "add",
                           was = NA, reason = NA, note = "as T-14-1-1, SAF"))
  tfl_ai_expected_write(d, e, overwrite = TRUE)
  expect_error(tfl_ai_expected_write(d, transform(e, action = "remove"), overwrite = TRUE),
               "action is blank")
  res <- suppressMessages(tfl_ai_run(d, rtf = FALSE))
  x <- res$expected
  expect_identical(x$result[x$output_id == made[1L]], "still there")
  expect_identical(x$result[x$output_id == made[2L]], "differs")
  expect_match(x$detail[x$output_id == made[2L]], "expected \"A Title Of The Person's\"", fixed = TRUE)
  expect_identical(x$result[x$output_id == made[3L]], "not expected")
  expect_identical(x$result[x$output_id == "T-14-9-9"], "missing")
  expect_match(attr(x, "text"), "missing 1, still there 1, differs 1, not expected 1", fixed = TRUE)
  st <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true("## Expected outputs (ai/expected_outputs.csv)" %in% st)
  expect_true(any(grepl("still there", st, fixed = TRUE)))
})
