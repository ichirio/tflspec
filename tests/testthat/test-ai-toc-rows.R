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

test_that("a request a shell, named after it; its rows, a shared shell, no shell, both", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- company_toc_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE,
                      size = 1)
  x <- tfl_ai_read_shell(f)
  expect_identical(nrow(r$requests), length(unique(x$toc$shell_key)))
  one <- r$requests[r$requests$request == "t.15.1.1.1", , drop = FALSE]
  expect_identical(one$outputs, "Table 15.1.1.1, Table 15.1.1.1.2, Table 15.1.1.1.3")
  txt <- paste(readLines(file.path(d, one$file), encoding = "UTF-8"), collapse = "\n")
  expect_match(txt, "- Table 15.1.1.1.3: shell t.15.1.1.1 (sheet 't.15.1.1.1'); title: Summary of Measure 3 / Subgroup 3; population: Safety Analysis Set; program: t_15_1_1_1_3.sas; Doc No: D003; Seq: 3; Phase: both",
               fixed = TRUE)
  expect_match(txt, "Reports sharing a shell are each a report file of their own", fixed = TRUE)
  expect_match(txt, "A TOC row can be several reports", fixed = TRUE)
  # the shell's sheet in the request
  expect_match(txt, "## Sheet \"t.15.1.1.1\"", fixed = TRUE)
  no <- r$requests[grepl("16.9.1", r$requests$outputs), , drop = FALSE]
  txt <- paste(readLines(file.path(d, no$file), encoding = "UTF-8"), collapse = "\n")
  expect_match(txt, "Table 16.9.1: no shell (the TOC names t.16.9.1", fixed = TRUE)
  # every row in spec/toc.yml
  toc <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  expect_identical(length(toc), nrow(x$toc))
})

test_that("a TOC without a shell column reads as before: the number, its sheet", {
  f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$shell_key, x$toc$key)
  expect_false(anyNA(x$toc$shell))
})

test_that("a shell in parts that two reports share: each report's parts, its number and the shell's letter", {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    Shell = c("t.15.1.5.1", "t.15.1.5.1"), `Output No` = c("Table 15.1.5.1", "Table 15.1.5.2"),
    Title = c("Demographics", "Demographics, Japan"), check.names = FALSE))
  s <- "t.15.1.5.1"
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, "Table 15.1.5.1.a  Demographics - Phase 1 Part", startRow = 1)
  openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 3, colNames = FALSE)
  openxlsx::writeData(wb, s, "Table 15.1.5.1.b  Demographics - Phase 2 Part", startRow = 20)
  openxlsx::saveWorkbook(wb, f)
  x <- tfl_ai_read_shell(f)
  expect_identical(x$toc$parts, c("Table 15.1.5.1.a | Table 15.1.5.1.b",
                                  "Table 15.1.5.2.a | Table 15.1.5.2.b"))
  expect_identical(x$toc$parts_row, c("1 | 20", "1 | 20"))
  lines <- tflspec:::.ai_parts_lines(x, "Table 15.1.5.2")
  expect_match(lines[1L], "Table 15.1.5.2.a: from row 1", fixed = TRUE)
  expect_match(lines[1L], "its title as the shell writes it", fixed = TRUE)
})
