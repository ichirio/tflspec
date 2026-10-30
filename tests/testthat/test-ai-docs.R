# A document as Markdown for the agent: .docx, .pdf, a workbook; the
# documents tfl_ai_prepare(docs = ) places, as text, named in AGENTS.md

test_that("a workbook: each sheet a heading and a table of its cells", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  xl <- file.path(withr::local_tempdir(), "sap-tables.xlsx")
  openxlsx::write.xlsx(list(
    Sets = data.frame(Set = c("SAF", "FAS"), Definition = c("all treated", "a | b")),
    Empty = data.frame()), xl)
  txt <- tfl_ai_doc_text(xl)
  expect_match(txt, "## Sets\n\n| Set | Definition |\n| --- | --- |\n| SAF | all treated |\n| FAS | a \\| b |",
               fixed = TRUE)
  expect_match(txt, "## Empty\n\n(empty)", fixed = TRUE)
  f <- file.path(dirname(xl), "out.md")
  expect_invisible(tfl_ai_doc_text(xl, file = f))
  expect_identical(paste(readLines(f, encoding = "UTF-8"), collapse = "\n"), txt)
})

test_that("a Word file: headings, paragraphs, list items and tables", {
  skip_if_not_installed("officer")
  d <- officer::read_docx()
  d <- officer::body_add_par(d, "Statistical Analysis Plan", style = "heading 1")
  d <- officer::body_add_par(d, "Some text here.", style = "Normal")
  d <- officer::body_add_par(d, "Analysis sets", style = "heading 2")
  d <- officer::body_add_table(d, data.frame(Set = c("SAF", "FAS"),
                                             Definition = c("all treated", "all randomized")),
                               style = "table_template")
  d <- officer::body_add_par(d, "After the table.", style = "Normal")
  f <- file.path(withr::local_tempdir(), "sap.docx")
  print(d, target = f)
  txt <- tfl_ai_doc_text(f)
  expect_match(txt, "# Statistical Analysis Plan\n\nSome text here.\n\n## Analysis sets\n\n| Set | Definition |\n| --- | --- |\n| SAF | all treated |\n| FAS | all randomized |\n\nAfter the table.",
               fixed = TRUE)
})

test_that("a PDF: its text page by page, each page marked", {
  skip_if_not_installed("pdftools")
  f <- file.path(withr::local_tempdir(), "sap.pdf")
  grDevices::pdf(f)
  graphics::plot.new()
  graphics::text(0.5, 0.9, "Statistical Analysis Plan")
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Page two")
  grDevices::dev.off()
  txt <- tfl_ai_doc_text(f)
  expect_match(txt, "<!-- page 1 -->", fixed = TRUE)
  expect_match(txt, "Statistical Analysis Plan", fixed = TRUE)
  expect_match(txt, "<!-- page 2 -->\n\n", fixed = TRUE)
  expect_match(txt, "Page two", fixed = TRUE)
})

test_that("what is not a document, or a package missing, is said", {
  f <- file.path(withr::local_tempdir(), "x.rtf")
  writeLines("x", f)
  expect_error(tfl_ai_doc_text(f), "not a document this reads")
  expect_error(tfl_ai_doc_text(file.path(dirname(f), "none.docx")), "no file")
  md <- file.path(dirname(f), "note.md")
  writeLines(c("# A note", "", "text"), md)
  expect_identical(tfl_ai_doc_text(md), "# A note\n\ntext")
})

test_that("a document already in ai/docs/ and named from there is kept, not copied onto itself", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  dir.create(file.path(d, "ai", "docs"), recursive = TRUE)
  sap <- file.path(d, "ai", "docs", "sap.md")
  writeLines(c("# SAP", "", "the analysis sets"), sap)
  r <- tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                      adam = "adam", quiet = TRUE, size = 3L, docs = sap)
  expect_identical(readLines(sap, encoding = "UTF-8"), c("# SAP", "", "the analysis sets"))
  expect_identical(r$agent_files, "ai/docs/sap.md")
  # a study's documents live in its docs/: inside the study they are read
  # where they are, never copied; a workbook gets its text in ai/docs/
  dir.create(file.path(d, "docs"))
  own <- file.path(d, "docs", "notes.md")
  writeLines(c("# Notes", "", "a rule"), own)
  skip_if_not_installed("openxlsx")
  xl <- file.path(d, "docs", "guide.xlsx")
  openxlsx::write.xlsx(list(Rules = data.frame(Topic = "Baseline", Rule = "last before dose")), xl)
  r2 <- tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                       adam = "adam", quiet = TRUE, size = 3L, docs = c(own, xl), overwrite = TRUE)
  expect_setequal(r2$agent_files, c("docs/notes.md", "ai/docs/guide.md"))
  expect_false(file.exists(file.path(d, "ai", "docs", "notes.md")))
  expect_false(file.exists(file.path(d, "ai", "docs", "guide.xlsx")))
  expect_match(paste(readLines(file.path(d, "ai", "docs", "guide.md"), encoding = "UTF-8"), collapse = "\n"),
               "| Baseline | last before dose |", fixed = TRUE)
  ag <- paste(readLines(file.path(d, "ai", "AGENTS.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ag, "`docs/notes.md`, `ai/docs/guide.md` (the text of docs/guide.xlsx).", fixed = TRUE)
  # the helper itself: the same file by another spelling, a folder target, a real copy
  f <- file.path(root, "a.txt"); writeLines("keep", f)
  expect_true(.file_copy_safe(f, file.path(root, ".", "a.txt")))
  expect_identical(readLines(f), "keep")
  expect_true(.file_copy_safe(f, root))
  expect_identical(readLines(f), "keep")
  g <- file.path(root, "b.txt")
  expect_true(.file_copy_safe(f, g))
  expect_identical(readLines(g), "keep")
})

test_that("tfl_ai_prepare(docs = ): a workbook document placed as text beside it, named in AGENTS.md", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("openxlsx")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  xl <- file.path(root, "data-guide.xlsx")
  openxlsx::write.xlsx(list(Notes = data.frame(Topic = "Baseline", Rule = "last before dose")), xl)
  note <- file.path(root, "sap.md")
  writeLines(c("# SAP", "", "text"), note)
  r <- tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                      adam = "adam", quiet = TRUE, size = 3L, docs = c(xl, note))
  expect_true(file.exists(file.path(d, "ai", "docs", "data-guide.xlsx")))
  expect_true(file.exists(file.path(d, "ai", "docs", "data-guide.md")))
  expect_match(paste(readLines(file.path(d, "ai", "docs", "data-guide.md"), encoding = "UTF-8"), collapse = "\n"),
               "| Baseline | last before dose |", fixed = TRUE)
  expect_setequal(r$agent_files, c("ai/docs/data-guide.md", "ai/docs/sap.md"))
  ag <- paste(readLines(file.path(d, "ai", "AGENTS.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ag, "`ai/docs/data-guide.md` (the text of ai/docs/data-guide.xlsx), `ai/docs/sap.md`.", fixed = TRUE)
})
