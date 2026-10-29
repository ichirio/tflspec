# A TOC of a company's shape: every row a report, its number and its
# shell's apart, several rows sharing a shell, a row with no shell; the
# other columns in its request; a request a shell

test_that("every TOC row is a report: its number, its shell's, its columns", {
  skip_if_not_installed("openxlsx")
  f <- company_toc_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f))
  w <- attr(f, "toc")
  x <- tfl_ai_read_shell(f)
  t <- x$toc
  expect_identical(nrow(t), nrow(w))
  expect_identical(t$output_id, w$`Output No`)
  expect_identical(t$shell_id, w$Shell)
  expect_identical(t$title, paste(w$`Title 1`, w$`Title 2`, sep = " / "))
  expect_identical(t$population, w$Population)
  expect_identical(t$program, w$Program)
  expect_match(t$info[3L], "Phase: both", fixed = TRUE)
  expect_match(t$info[1L], "Doc No: D001", fixed = TRUE)
  # a shell several rows share: their sheet; a shell with no sheet: none
  shared <- w$Shell[duplicated(w$Shell)][1L]
  expect_true(all(t$shell[t$shell_id == shared] == shared))
  expect_identical(sum(is.na(t$shell)), 3L)
  expect_true(all(t$output_id[is.na(t$shell)] %in% paste("Table", c("16.9.1", "16.9.2", "16.9.3"))))
})

test_that("toc_cols says the columns when the headers do not", {
  skip_if_not_installed("openxlsx")
  f <- company_toc_book(tempfile(fileext = ".xlsx"), n_shells = 10L, n_rows = 15L)
  on.exit(unlink(f))
  w <- attr(f, "toc")
  # by header and by letter: the title the second title alone, the shell D
  x <- tfl_ai_read_shell(f, toc_cols = c(output_id = "Output No", shell = "C", title = "Title 2"))
  expect_identical(x$toc$title, w$`Title 2`)
  expect_identical(x$toc$shell_id, w$Shell)
  expect_error(tfl_ai_read_shell(f, toc_cols = c(output_id = "Nope")), "no TOC column")
  expect_error(tfl_ai_read_shell(f, toc_cols = c(what = "Shell")), "names the columns of")
})

test_that("the batches: a shell's reports together; size counts shells", {
  skip_if_not_installed("openxlsx")
  f <- company_toc_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f))
  x <- tfl_ai_read_shell(f)
  b <- tfl_ai_batches(x, size = 1)
  expect_identical(length(unique(b$batch)), length(unique(x$toc$shell_key)))
  by_shell <- tapply(b$batch, b$shell_id, function(v) length(unique(v)))
  expect_true(all(by_shell == 1L))
  b3 <- tfl_ai_batches(x, size = 3)
  per <- tapply(b3$shell_id, b3$batch, function(v) length(unique(v)))
  expect_true(all(per <= 3L))
})

test_that("a TOC without a shell column reads as before: the number, its sheet", {
  f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$shell_key, x$toc$key)
  expect_false(anyNA(x$toc$shell))
})


test_that("a company's TOC: a request a shell sheet, the TOC items sharing it its candidates", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- company_toc_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, templates = FALSE)
  x <- tfl_ai_read_shell(f)
  # 60 shell sheets and one request of the TOC items with no shell
  expect_identical(nrow(r$requests), nrow(x$shells) + 1L)
  expect_identical(nrow(x$shells), 60L)
  expect_identical(r$counts[["no_shell"]], 3L)
  one <- r$requests[r$requests$request == "t.15.1.1.1", , drop = FALSE]
  txt <- paste(readLines(file.path(d, one$file), encoding = "UTF-8"), collapse = "\n")
  for (id in c("Table 15.1.1.1", "Table 15.1.1.1.2", "Table 15.1.1.1.3")) {
    expect_match(txt, paste0("- ", id, " ("), fixed = TRUE)
  }
  expect_match(txt, "Phase: both", fixed = TRUE)
  ns <- r$requests[r$requests$request == "no-shell", , drop = FALSE]
  expect_identical(ns$outputs, "Table 16.9.1, Table 16.9.2, Table 16.9.3")
})
