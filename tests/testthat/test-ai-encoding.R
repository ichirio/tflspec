# The kit on a Japanese Windows PC: answers saved by Notepad (UTF-8 with a
# BOM, CRLF, UTF-16, Shift-JIS by mistake, a .txt added), every file the kit
# writes in UTF-8, Japanese labels kept from the workbook to the YAML.

enc_study <- function() {
  skip_if_not_installed("haven")
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "adam"))
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01"))
  ex <- system.file("ai", "example", package = "tflspec")
  shell <- tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
  adam <- tfl_ai_adam_summary(file.path(d, "adam"))
  r <- suppressMessages(tfl_ai_write_requests(unit = "toc", d, shell, adam, size = 3L))
  list(dir = d, ex = ex, r = r)
}

raw_utf8 <- function(f) {
  x <- rawToChar(readBin(f, "raw", file.size(f)))
  Encoding(x) <- "UTF-8"
  x
}

write_raw <- function(x, f) writeBin(x, f)

# an answer's text with a Japanese label in place of the English one
jp_label <- "人口統計学的特性及びベースライン特性"
answer_text <- function(s, request, jp = FALSE) {
  x <- readLines(file.path(test_path("fixtures"), "answers-toc", paste0(request, ".md")), encoding = "UTF-8")
  if (jp) x <- sub("label: Demographic and Baseline Characteristics", paste("label:", jp_label),
                   x, fixed = TRUE)
  x
}

test_that("a draft with a Japanese label: collected, kept in UTF-8", {
  s <- enc_study()
  ai_drafts_from_answer(s$dir, answer_text(s, "demographics-1", jp = TRUE), "table")
  res <- tfl_ai_collect_answers(s$dir)
  st <- stats::setNames(res$reports$status, res$reports$output_id)
  expect_identical(unname(st["T-14-1-1"]), "ok")
  expect_identical(unname(st["T-14-3-5"]), "no answer")
  # the Japanese label is in the draft's YAML, in UTF-8
  f <- file.path(s$dir, "ai", "spec", "outputs", "T-14-1-1.yml")
  y <- raw_utf8(f)
  expect_true(validUTF8(y))
  expect_true(grepl(jp_label, y, fixed = TRUE))
  expect_identical(tfl_read_output(f)$label, jp_label)
})

test_that("a draft under a name that is no request's is pointed out", {
  s <- enc_study()
  area <- file.path(s$dir, "ai", "spec", "outputs")
  dir.create(area, recursive = TRUE, showWarnings = FALSE)
  tfl_write_output(tfl_output("AE-answer", kind = "table", label = "x"),
                   file.path(area, "AE-answer.yml"))
  res <- tfl_ai_collect_answers(s$dir)
  expect_identical(unique(res$reports$status), "no answer")
  expect_identical(res$stray, "ai/spec/outputs/AE-answer.yml")
  out <- capture.output(print(res))
  expect_true(any(grepl("Not read (no request asks for them): ai/spec/outputs/AE-answer.yml", out,
                        fixed = TRUE)))
})

test_that("a draft with its errors: the repair and the next step; the draft stays", {
  s <- enc_study()
  ai_drafts_from_answer(s$dir, answer_text(s, "ae-1"), "table")
  f <- file.path(s$dir, "ai", "spec", "outputs", "T-14-3-1.yml")
  x <- readLines(f, encoding = "UTF-8")
  writeLines(sub("dataset: ADAE", "dataset: ADXX", x, fixed = TRUE), f, useBytes = TRUE)
  res <- tfl_ai_collect_answers(s$dir)
  expect_identical(res$reports$status[res$reports$output_id == "T-14-3-1"], "error")
  expect_true(file.exists(file.path(s$dir, "ai", "repairs", "ae-1.md")))
  expect_true(file.exists(f))
  out <- capture.output(print(res))
  expect_true(any(grepl("paste ai/repairs/ae-1.md into the agent's chat: it fixes", out, fixed = TRUE)))
})

test_that("every file the kit writes is UTF-8, Japanese text kept", {
  s <- enc_study()
  f <- c(list.files(file.path(s$dir, "ai"), recursive = TRUE, full.names = TRUE,
                    pattern = "[.]md$"),
         list.files(file.path(s$dir, "spec"), recursive = TRUE, full.names = TRUE),
         file.path(s$dir, "study.yml"))
  expect_true(all(vapply(f, function(z) validUTF8(raw_utf8(z)), NA)))
  # a Japanese study id and population labels
  d2 <- withr::local_tempdir()
  dir.create(file.path(d2, "adam"))
  adsl <- tflspec::adsl[1:20, c("USUBJID", "TRT01A", "SAFFL")]
  adsl$TRT01A <- as.character(adsl$TRT01A)
  attr(adsl$SAFFL, "label") <- "安全性解析対象集団 Population Flag"
  saveRDS(adsl, file.path(d2, "adam", "adsl.rds"))
  tfl_ai_study_init(d2, study_id = "試験-01")
  for (z in c("study.yml", "spec/data.yml", "spec/defaults.yml")) {
    txt <- raw_utf8(file.path(d2, z))
    expect_true(validUTF8(txt))
  }
  expect_true(grepl("試験-01", raw_utf8(file.path(d2, "study.yml")), fixed = TRUE))
  expect_true(grepl("安全性", raw_utf8(file.path(d2, "spec", "data.yml")),
                    fixed = TRUE))
  a <- tfl_ai_adam_summary(file.path(d2, "adam"), file = file.path(d2, "ai", "adam_summary.md"))
  expect_true(grepl("安全性", raw_utf8(file.path(d2, "ai", "adam_summary.md")),
                    fixed = TRUE))
})

test_that("in a Shift-JIS session the files written are UTF-8 all the same", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("haven")
  sjis <- grep("^ja_JP[.](sjis|SJIS|shiftjis)", system("locale -a", intern = TRUE,
                                                       ignore.stderr = TRUE), value = TRUE)
  skip_if(!length(sjis), "no Shift-JIS locale")
  d <- withr::local_tempdir()
  script <- file.path(d, "run.R")
  writeLines(c(
    sprintf(".libPaths(c(%s, .libPaths()))", paste(deparse(.libPaths()), collapse = "")),
    "stopifnot(!l10n_info()[['UTF-8']])",
    "suppressMessages(library(tflspec))",
    sprintf("d <- %s", deparse(d)),
    "dir.create(file.path(d, 'adam'))",
    "adsl <- tflspec::adsl; adsl[] <- lapply(adsl, function(v) if (is.factor(v)) as.character(v) else v)",
    "haven::write_xpt(adsl, file.path(d, 'adam', 'adsl.xpt'))",
    "haven::write_xpt(as.data.frame(lapply(tflspec::adae, function(v) if (is.factor(v)) as.character(v) else v)), file.path(d, 'adam', 'adae.xpt'))",
    "tfl_ai_study_init(d, study_id = '\\u8a66\\u9a13-01')",
    "ex <- system.file('ai', 'example', package = 'tflspec')",
    "shell <- tfl_ai_read_shell(file.path(ex, 'onco_toc_shell.xlsx'), file = file.path(d, 'ai', 'toc_shell.md'))",
    "adam <- tfl_ai_adam_summary(file.path(d, 'adam'), file = file.path(d, 'ai', 'adam_summary.md'))",
    # (a request needs the schema, which R/schema.R reads in the session's
    # encoding: not run here; a draft is read and written as collecting does)
    "o <- tfl_read_output(file.path(ex, 'drafts', 'T-14-1-1.yml'))",
    "o$label <- '\\u4eba\\u53e3\\u7d71\\u8a08'",
    "dir.create(file.path(d, 'spec', 'outputs'), recursive = TRUE)",
    "tfl_write_output(o, file.path(d, 'spec', 'outputs', 'T-14-1-1.yml'))",
    "cat('DONE\\n')"), script)
  out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
                 stdout = TRUE, stderr = TRUE, env = c(paste0("LC_ALL=", sjis[1L]),
                                                       paste0("LANG=", sjis[1L])))
  expect_true("DONE" %in% out, info = paste(out, collapse = "\n"))
  f <- list.files(d, recursive = TRUE, full.names = TRUE, pattern = "[.](md|yml)$")
  ok <- vapply(f, function(z) validUTF8(raw_utf8(z)), NA)
  expect_true(all(ok), info = paste(basename(f[!ok]), collapse = ", "))
  expect_true(grepl("試験-01", raw_utf8(file.path(d, "study.yml")), fixed = TRUE))
  expect_true(grepl("人口統計",
                    raw_utf8(file.path(d, "spec", "outputs", "T-14-1-1.yml")), fixed = TRUE))
})
