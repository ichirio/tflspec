# The kit on a Japanese Windows PC: answers saved by Notepad (UTF-8 with a
# BOM, CRLF, UTF-16, Shift-JIS by mistake, a .txt added), every file the kit
# writes in UTF-8, Japanese labels kept from the workbook to the YAML.

enc_study <- function() {
  skip_if_not_installed("haven")
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "adam"))
  for (x in c("adsl", "adae", "adrs", "adlb")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01"))
  ex <- system.file("ai", "example", package = "tflspec")
  shell <- tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
  adam <- tfl_ai_adam_summary(file.path(d, "adam"))
  r <- suppressMessages(tfl_ai_write_requests(d, shell, adam))
  list(dir = d, ex = ex, r = r, ans = file.path(d, "ai", "answers"))
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
  x <- readLines(file.path(s$ex, "answers", paste0(request, ".md")), encoding = "UTF-8")
  if (jp) x <- sub("label: Demographic and Baseline Characteristics", paste("label:", jp_label),
                   x, fixed = TRUE)
  x
}

test_that("answers saved by Notepad: UTF-8 with BOM and CRLF, UTF-16, .md.txt", {
  s <- enc_study()
  dm <- enc2utf8(paste(answer_text(s, "demographics-1", jp = TRUE), collapse = "\r\n"))
  # UTF-8 with the BOM, CRLF line ends
  write_raw(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(dm)), file.path(s$ans, "demographics-1.md"))
  # Notepad's "Unicode": UTF-16LE with its BOM
  u16 <- iconv(paste(answer_text(s, "efficacy-1"), collapse = "\r\n"), "UTF-8", "UTF-16LE",
               toRaw = TRUE)[[1L]]
  write_raw(c(as.raw(c(0xff, 0xfe)), u16), file.path(s$ans, "efficacy-1.md"))
  # Notepad added .txt
  writeLines(answer_text(s, "ae-1"), file.path(s$ans, "ae-1.md.txt"), useBytes = TRUE)
  res <- tfl_ai_collect_answers(s$dir)
  st <- stats::setNames(res$status, res$output_id)
  expect_identical(unname(st[c("T-14-1-1", "T-14-2-1", "T-14-3-1")]), rep("ok", 3L))
  expect_identical(unname(st["T-14-3-5"]), "no answer")
  # the Japanese label is in the YAML, in UTF-8
  y <- raw_utf8(file.path(s$dir, "spec", "outputs", "T-14-1-1.yml"))
  expect_true(validUTF8(y))
  expect_true(grepl(jp_label, y, fixed = TRUE))
  expect_identical(tfl_read_output(file.path(s$dir, "spec", "outputs", "T-14-1-1.yml"))$label,
                   jp_label)
  # the print: each request's status, reports and next step
  out <- capture.output(print(res))
  expect_true(any(grepl("4 reports in 4 requests: ok 3", out, fixed = TRUE)))
  expect_true(any(grepl("^labs-1 +no answer +T-14-3-5$", out)))
  expect_true(any(grepl("next: paste ai/requests/labs-1.md into a new chat", out, fixed = TRUE)))
  expect_true(any(grepl("next: done", out, fixed = TRUE)))
})

test_that("an answer in Shift-JIS (CP932) is an error that says how to save it", {
  s <- enc_study()
  sj <- iconv(paste(answer_text(s, "demographics-1", jp = TRUE), collapse = "\r\n"), "UTF-8",
              "CP932", toRaw = TRUE)[[1L]]
  skip_if(is.null(sj), "no CP932 converter")
  write_raw(sj, file.path(s$ans, "demographics-1.md"))
  res <- tfl_ai_collect_answers(s$dir)
  r1 <- res[res$request == "demographics-1", ]
  expect_identical(r1$status, "error")
  expect_match(r1$first, "is not UTF-8", fixed = TRUE)
  expect_match(r1$first, "Encoding: UTF-8", fixed = TRUE)
  expect_false(file.exists(file.path(s$dir, "spec", "outputs", "T-14-1-1.yml")))
  # tfl_ai_unpack() says the same
  expect_error(tfl_ai_unpack(file.path(s$ans, "demographics-1.md"), s$dir), "not UTF-8")
})

test_that("an answer under a name that is no request is pointed out", {
  s <- enc_study()
  writeLines(answer_text(s, "ae-1"), file.path(s$ans, "AE answer.md"), useBytes = TRUE)
  res <- tfl_ai_collect_answers(s$dir)
  expect_identical(unique(res$status), "no answer")
  out <- capture.output(print(res))
  expect_true(any(grepl("Not read (no request of that name): ai/answers/AE answer.md", out,
                        fixed = TRUE)))
})

test_that("an answer of a request with its errors: the repair and the next step", {
  s <- enc_study()
  x <- answer_text(s, "ae-1")
  x <- sub("dataset: ADAE", "dataset: ADXX", x, fixed = TRUE)
  writeLines(x, file.path(s$ans, "ae-1.md"), useBytes = TRUE)
  res <- tfl_ai_collect_answers(s$dir)
  expect_identical(res$status[res$output_id == "T-14-3-1"], "error")
  expect_true(file.exists(file.path(s$dir, "ai", "repairs", "ae-1.md")))
  out <- capture.output(print(res))
  expect_true(any(grepl("paste ai/repairs/ae-1.md into the SAME chat", out, fixed = TRUE)))
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
    # encoding: not run here; the answer is unpacked as collecting does)
    "x <- readLines(file.path(ex, 'answers', 'demographics-1.md'), encoding = 'UTF-8')",
    "x <- sub('label: Demographic and Baseline Characteristics', 'label: \\u4eba\\u53e3\\u7d71\\u8a08', x)",
    "f <- file.path(d, 'ai', 'answers', 'demographics-1.md')",
    "dir.create(dirname(f))",
    "writeLines(enc2utf8(x), f, useBytes = TRUE)",
    "tfl_ai_unpack(f, d)",
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
