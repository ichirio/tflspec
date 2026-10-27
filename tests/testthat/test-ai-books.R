# The AI steps over several workbooks: a TOC and its shells in workbooks of
# their own (tfl_ai_read_shell(shells = )), and a study's requests added to
# one workbook at a time (tfl_ai_prepare(unit = "toc", add = TRUE)) (#378)

books_adam <- function(root) {
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  normalizePath(ad, winslash = "/")
}
ex_workbook <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

# the example split in two: its TOC alone, its shells alone
split_books <- function(dir) {
  wb <- openxlsx::loadWorkbook(ex_workbook())
  for (s in setdiff(names(wb), "TOC")) openxlsx::removeWorksheet(wb, s)
  openxlsx::saveWorkbook(wb, file.path(dir, "toc.xlsx"), overwrite = TRUE)
  wb <- openxlsx::loadWorkbook(ex_workbook())
  openxlsx::removeWorksheet(wb, "TOC")
  openxlsx::saveWorkbook(wb, file.path(dir, "shells.xlsx"), overwrite = TRUE)
  file.path(dir, c("toc.xlsx", "shells.xlsx"))
}

# a second workbook: a TOC of one more table, with its shell (named as one
# of the first workbook's sheets)
more_book <- function(dir) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    No = "T-14-3-2", Type = "Table", Title = "Serious Adverse Events by System Organ Class",
    Population = "Safety Population"))
  openxlsx::addWorksheet(wb, "Safety")
  openxlsx::writeData(wb, "Safety", c("Table 14.3.2", "Serious Adverse Events by System Organ Class",
                                      "(Safety Population)", "", "System Organ Class | Drug X | Control"))
  f <- file.path(dir, "more.xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

test_that("tfl_ai_read_shell(shells = ): the TOC in one workbook, its shells in another", {
  skip_if_not_installed("openxlsx")
  d <- withr::local_tempdir()
  f <- split_books(d)
  x <- tfl_ai_read_shell(f[1L], shells = f[2L])
  expect_identical(x$books$book, c("toc.xlsx", "shells.xlsx"))
  expect_identical(x$books$role, c("toc", "shells"))
  expect_identical(x$toc$output_id, c("T-14-1-1", "T-14-2-1", "F-14-2-2", "T-14-3-1", "T-14-3-5"))
  expect_true(all(!is.na(x$toc$shell)))
  expect_identical(unique(x$toc$book), "shells.xlsx")
  expect_identical(x$sheets$role[x$sheets$sheet == "TOC"], "toc")
  # a part a workbook
  txt <- format(tfl_ai_read_shell(f[1L], shells = f[2L], outputs = "F-14-2-2"))
  expect_match(txt, "# Workbook toc.xlsx: 1 sheet(s)", fixed = TRUE)
  expect_match(txt, "# Workbook shells.xlsx: 1 sheet(s)", fixed = TRUE)
  expect_match(txt, "Y axis: Survival Probability", fixed = TRUE)
  # where each shell is: the workbook, the sheet's own name, the TOC row's workbook
  w <- .ai_shell_where(x, "T-14-3-5")
  expect_match(w, "workbook `shells.xlsx`, sheet 'Safety', cells A", fixed = TRUE)
  expect_match(w, "its TOC row: workbook `toc.xlsx`, sheet 'TOC'", fixed = TRUE)
  expect_output(print(x), "2 workbooks: toc.xlsx (the TOC), shells.xlsx", fixed = TRUE)
  # a sheet named as one already there: named by its workbook
  y <- tfl_ai_read_shell(ex_workbook(), shells = more_book(d))
  expect_true("Safety [more.xlsx]" %in% y$sheets$sheet)
  expect_identical(y$sheets$name[y$sheets$sheet == "Safety [more.xlsx]"], "Safety")
  expect_error(tfl_ai_read_shell(f[1L], shells = file.path(d, "none.xlsx")), "no file")
})

test_that("tfl_ai_prepare(shells = , add = TRUE): one workbook at a time, the requests kept", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("openxlsx")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- books_adam(root)
  f <- split_books(root)
  r <- tfl_ai_prepare(unit = "toc", d, f[1L], shells = f[2L], adam = ad, quiet = TRUE, mode = "agent", size = 3L,
                      agent_reads = "workbook")
  expect_identical(unique(r$requests$book), "shells.xlsx")
  expect_identical(unique(r$toc$book), "shells.xlsx")
  # an agent request names both files
  txt <- paste(readLines(file.path(d, r$requests$file[1L]), encoding = "UTF-8"), collapse = "\n")
  expect_match(txt, "The TOC is the workbook `", fixed = TRUE)
  expect_match(txt, "shells.xlsx`.", fixed = TRUE)
  # an answer saved before the next workbook is added
  tfl_ai_save_answer(d, "ae-1", "an answer")
  m <- more_book(root)
  r2 <- tfl_ai_prepare(unit = "toc", d, m, adam = ad, quiet = TRUE, mode = "agent", size = 3L, add = TRUE,
                       agent_reads = "workbook")
  expect_identical(r2$requests$request, c(r$requests$request, "ae-2"))
  expect_identical(r2$requests$book[r2$requests$request == "ae-2"], "more.xlsx")
  expect_identical(r2$requests$outputs[r2$requests$request == "ae-2"], "T-14-3-2")
  expect_true(all(file.exists(file.path(d, r2$requests$file))))
  expect_true(file.exists(file.path(d, "ai", "answers", "ae-1.md")))
  expect_true(file.exists(file.path(d, "ai", "toc_shell-more.md")))
  expect_identical(tfl_ai_requests(d), r2$requests)
  idx <- readLines(file.path(d, "ai", "requests", "index.md"), encoding = "UTF-8")
  expect_true(any(grepl("| ae-2 |", idx, fixed = TRUE)))
  expect_true(any(grepl("| figure-1 |", idx, fixed = TRUE)))
  # the study's TOC: the new report at its end
  toc <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  expect_identical(vapply(toc, `[[`, "", "id"),
                   c("T-14-1-1", "T-14-2-1", "F-14-2-2", "T-14-3-1", "T-14-3-5", "T-14-3-2"))
  # the same workbook again: stopped; asked again with overwrite
  expect_error(tfl_ai_prepare(unit = "toc", d, m, adam = ad, quiet = TRUE, add = TRUE),
               "T-14-3-2 already asked for \\(ae-2\\)")
  # the study's own files kept, whatever overwrite says
  keep <- c("study.yml", "spec/data.yml", "spec/defaults.yml", "AGENTS.md")
  for (f in file.path(d, keep)) cat("# the study's own line\n", file = f, append = TRUE)
  before <- lapply(file.path(d, keep), readLines)
  r3 <- tfl_ai_prepare(unit = "toc", d, m, adam = ad, quiet = TRUE, add = TRUE, overwrite = TRUE)
  expect_identical(lapply(file.path(d, keep), readLines), before)
  expect_identical(sum(r3$requests$outputs == "T-14-3-2"), 1L)
  expect_identical(nrow(r3$requests), nrow(r2$requests))
  # without add, the requests are written anew (as before)
  r4 <- tfl_ai_prepare(unit = "toc", d, m, adam = ad, quiet = TRUE)
  expect_identical(r4$requests$request, "ae-1")
})
