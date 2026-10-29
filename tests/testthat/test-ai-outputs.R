# The AI's reports listed for the person (tfl_ai_outputs())

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
                 quiet = TRUE, size = 3L)
  invisible(tfl_ai_migrate(d))
  ai_drafts_from_fixtures(d)
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
  # adopted: so; the AI redrafting it later: "draft newer"
  id <- made[1L]
  invisible(tfl_ai_adopt(d, id))
  expect_identical(tfl_ai_outputs(d)$adopted[match(id, o$output_id)], "yes")
  f <- file.path(d, o$file[match(id, o$output_id)])
  writeLines(c(readLines(f, warn = FALSE), "# redrafted"), f)
  expect_identical(tfl_ai_outputs(d)$adopted[match(id, o$output_id)], "yes, draft newer")
})
