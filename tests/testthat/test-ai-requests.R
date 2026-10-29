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
  r <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, s$shell, s$adam, size = 8))
  expect_setequal(unlist(strsplit(r$outputs, ", ")), s$shell$toc$output_id)
  f <- list.files(file.path(s$dir, "ai", "requests"))
  expect_true(all(paste0(r$request, ".md") %in% f))
  expect_true(all(c("index.md", "_contexts.rds") %in% f))
  # each is the agent's request: the instructions are named, not repeated
  one <- readLines(file.path(s$dir, r$file[1L]), encoding = "UTF-8")
  expect_identical(one[1L], "<!-- templates: TRUE -->")
  expect_false(any(grepl("^## Schema", one)))
  expect_identical(nchar(paste(one, collapse = "\n")), r$chars[1L])
  # written again only when asked
  expect_error(tfl_ai_write_requests(unit = "toc", s$dir, s$shell, s$adam), "overwrite")
  # the instructions once: files of their own, never in the requests
  r1 <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, s$shell, s$adam, overwrite = TRUE))
  expect_false(file.exists(file.path(s$dir, "ai", "requests", "00-instructions.md")))
  expect_true(all(file.exists(file.path(s$dir, "ai", c("instructions-table.md",
                                                       "instructions-figure.md", "templates.md")))))
  expect_false(dir.exists(file.path(s$dir, "ai", "answers")))
  # (the default: a request a report, named after it)
  expect_setequal(r1$request, s$shell$toc$output_id)
})

test_that("tfl_ai_write_requests(max_chars = ): a request too long is split", {
  s <- req_study()
  shell <- s$shell
  shell$toc$family <- "demographics"         # one family: one request of four
  r <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, shell, s$adam, size = 8, types = "table"))
  expect_identical(nrow(r), 1L)
  r <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, shell, s$adam, size = 8, types = "table",
                                              templates = FALSE, overwrite = TRUE))
  lim <- r$chars - 500
  r2 <- suppressMessages(suppressWarnings(tfl_ai_write_requests(unit = "toc", 
    s$dir, shell, s$adam, size = 8, max_chars = lim, types = "table", overwrite = TRUE,
    templates = FALSE)))
  expect_gt(nrow(r2), 1L)
  expect_true(all(r2$request %in% c("demographics-1a", "demographics-1b",
                                    "demographics-1aa", "demographics-1ab",
                                    "demographics-1ba", "demographics-1bb")))
  expect_setequal(unlist(strsplit(r2$outputs, ", ")), shell$toc$output_id[shell$toc$type == "table"])
})

test_that("tfl_ai_collect_answers(): checked, written, one row a report", {
  s <- req_study()
  r <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, s$shell, s$adam, size = 8))
  # the sample's answer holds all four reports; the drafts of the first request's only
  first <- r$request[1L]
  mine <- strsplit(r$outputs[1L], ", ")[[1L]]
  ids <- ai_drafts_from_answer(s$dir, file.path(s$ex, "onco-answer.md"), "table")
  area <- file.path(s$dir, "ai", "spec", "outputs")
  unlink(file.path(area, paste0(setdiff(ids, mine), ".yml")))
  res <- tfl_ai_collect_answers(s$dir)
  expect_s3_class(res, "tfl_ai_collected")
  expect_setequal(res$reports$output_id, s$shell$toc$output_id)
  expect_true(all(res$reports$status[res$reports$output_id %in% mine] %in% c("ok", "warning")))
  expect_true(all(res$reports$status[!res$reports$output_id %in% mine] == "no answer"))
  # only the reports it asked for are written
  expect_setequal(sub("[.]yml$", "", list.files(area)), mine)
  expect_output(print(res), "no answer")

  # a draft that does not read: an error, said why, the draft stays
  id2 <- strsplit(r$outputs[2L], ", ")[[1L]][1L]
  writeLines("a: [", file.path(area, paste0(id2, ".yml")))
  res2 <- tfl_ai_collect_answers(s$dir)
  expect_true(all(res2$reports$status[res2$reports$request == r$request[2L]] == "error"))
  expect_match(res2$reports$first[res2$reports$output_id == id2],
               sprintf("ai/spec/outputs/%s.yml: the YAML cannot be read", id2), fixed = TRUE)
  expect_true(file.exists(file.path(area, paste0(id2, ".yml"))))
})

test_that("tfl_ai_write_requests(examples = ): named by family, only to that family", {
  s <- req_study()
  r <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, s$shell, s$adam, size = 8))
  id <- strsplit(r$outputs[1L], ", ")[[1L]][1L]
  ai_drafts_from_answer(s$dir, file.path(s$ex, "onco-answer.md"), "table")
  # a reviewed report file: the study's, copied from the draft
  dir.create(file.path(s$dir, "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(s$dir, "ai", "spec", "outputs", paste0(id, ".yml")),
            file.path(s$dir, "spec", "outputs", paste0(id, ".yml")))
  ex <- c(stats::setNames(file.path("spec", "outputs", paste0(id, ".yml")), r$family[1L]))
  r2 <- suppressMessages(tfl_ai_write_requests(unit = "toc", s$dir, s$shell, s$adam, size = 8,
                                               examples = ex, overwrite = TRUE))
  grew <- r2$chars > r$chars[match(r2$request, r$request)]
  expect_identical(grew, r2$family == r$family[1L])
})

test_that("tfl_ai_collect_answers(): no requests, said", {
  expect_error(tfl_ai_collect_answers(tempfile()), "tfl_ai_write_requests")
})
