# A study's requests as files, one paste each, and their answers read back.

req_study <- function() {
  skip_if_not_installed("haven")
  skip_if_not_installed("readxl")
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "adam"))
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01"))
  ex <- system.file("ai", "example", package = "tflspec")
  list(dir = d, ex = ex,
       shell = tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx")),
       adam = tfl_ai_adam_summary(file.path(d, "adam")))
}

test_that("tfl_ai_write_requests(): one file a request, an index, the contexts", {
  s <- req_study()
  r <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, size = 8))
  expect_setequal(unlist(strsplit(r$outputs, ", ")), s$shell$toc$output_id)
  f <- list.files(file.path(s$dir, "ai", "requests"))
  expect_true(all(paste0(r$request, ".md") %in% f))
  expect_true(all(c("index.md", "_contexts.rds") %in% f))
  # each is a whole prompt: the instructions and its own reports
  one <- readLines(file.path(s$dir, r$file[1L]), encoding = "UTF-8")
  expect_true(any(grepl("^## Schema", one)))
  expect_identical(nchar(paste(one, collapse = "\n")), r$chars[1L])
  # written again only when asked
  expect_error(tfl_ai_write_requests(s$dir, s$shell, s$adam), "overwrite")
  # the instructions once: a file of their own, the requests without them
  r1 <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam,
                                               instructions = "once", overwrite = TRUE))
  expect_true(file.exists(file.path(s$dir, "ai", "requests", "00-instructions.md")))
  # (a figure's request keeps its own instructions)
  expect_true(all((r1$chars < r$chars - 15000)[r$type == "table"]))
})

test_that("tfl_ai_write_requests(max_chars = ): a request too long is split", {
  s <- req_study()
  shell <- s$shell
  shell$toc$family <- "demographics"         # one family: one request of four
  r <- suppressMessages(tfl_ai_write_requests(s$dir, shell, s$adam, size = 8, types = "table"))
  expect_identical(nrow(r), 1L)
  lim <- r$chars - 500
  r2 <- suppressMessages(suppressWarnings(tfl_ai_write_requests(
    s$dir, shell, s$adam, size = 8, max_chars = lim, types = "table", overwrite = TRUE)))
  expect_gt(nrow(r2), 1L)
  expect_true(all(r2$request %in% c("demographics-1a", "demographics-1b",
                                    "demographics-1aa", "demographics-1ab",
                                    "demographics-1ba", "demographics-1bb")))
  expect_setequal(unlist(strsplit(r2$outputs, ", ")), shell$toc$output_id[shell$toc$type == "table"])
})

test_that("tfl_ai_collect_answers(): checked, written, one row a report", {
  s <- req_study()
  r <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, size = 8))
  dir.create(file.path(s$dir, "ai", "answers"), showWarnings = FALSE)
  # the sample's answer holds all four reports; given as the first request's
  first <- r$request[1L]
  file.copy(file.path(s$ex, "onco-answer.md"),
            file.path(s$dir, "ai", "answers", paste0(first, ".md")))
  res <- tfl_ai_collect_answers(s$dir)
  expect_s3_class(res, "tfl_ai_collected")
  expect_setequal(res$reports$output_id, s$shell$toc$output_id)
  mine <- strsplit(r$outputs[1L], ", ")[[1L]]
  expect_true(all(res$reports$status[res$reports$output_id %in% mine] %in% c("ok", "warning")))
  expect_true(all(res$reports$status[!res$reports$output_id %in% mine] == "no answer"))
  # only the reports it asked for are written
  expect_setequal(sub("[.]yml$", "", list.files(file.path(s$dir, "spec", "outputs"))), mine)
  expect_output(print(res), "no answer")

  # an answer that does not read: an error, a repair prompt, no file
  writeLines("no yaml here", file.path(s$dir, "ai", "answers", paste0(r$request[2L], ".md")))
  res2 <- tfl_ai_collect_answers(s$dir)
  expect_true(all(res2$reports$status[res2$reports$request == r$request[2L]] == "error"))
})

test_that("tfl_ai_write_requests(examples = ): named by family, only to that family", {
  s <- req_study()
  r <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, size = 8))
  dir.create(file.path(s$dir, "ai", "answers"), showWarnings = FALSE)
  file.copy(file.path(s$ex, "onco-answer.md"),
            file.path(s$dir, "ai", "answers", paste0(r$request[1L], ".md")))
  res <- tfl_ai_collect_answers(s$dir)
  id <- strsplit(r$outputs[1L], ", ")[[1L]][1L]
  ex <- c(stats::setNames(file.path("spec", "outputs", paste0(id, ".yml")), r$family[1L]))
  r2 <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, size = 8,
                                               examples = ex, overwrite = TRUE))
  grew <- r2$chars > r$chars[match(r2$request, r$request)]
  expect_identical(grew, r2$family == r$family[1L])
})

test_that("tfl_ai_collect_answers(): no requests, said", {
  expect_error(tfl_ai_collect_answers(tempfile()), "tfl_ai_write_requests")
})
