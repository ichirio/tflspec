# The TOC workbook's batches: a second TOC sheet named for a batch, a
# column of marks; written to spec/toc.yml as the item's `sets`

batch_book <- function(f) {
  toc <- data.frame(
    `No.` = c("T-14-1-1", "T-14-2-1", "L-16-1-1"),
    Type = c("Table", "Table", "Listing"),
    Title = c("Demographics", "Response", "Adverse Events"),
    Population = c("Safety", "ITT", "Safety"),
    Topline = c("x", "", "x"),
    Program = c("t_dm.sas", "t_orr.sas", "l_ae.sas"),
    check.names = FALSE, stringsAsFactors = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", "Table of Contents", startRow = 1)
  openxlsx::writeData(wb, "TOC", toc, startRow = 3)
  # the safety batch: a TOC sheet of its own, the numbers alone
  openxlsx::addWorksheet(wb, "TOC_safety")
  openxlsx::writeData(wb, "TOC_safety", data.frame(`No.` = c("T-14-1-1", "L-16-1-1"),
                                                   check.names = FALSE), startRow = 1)
  for (s in c("T-14-1-1", "T-14-2-1", "L-16-1-1")) {
    openxlsx::addWorksheet(wb, s)
    openxlsx::writeData(wb, s, paste(s, "shell"), startRow = 1)
    openxlsx::writeData(wb, s, t(c("", "Placebo", "Drug X")), startRow = 3, colNames = FALSE)
    openxlsx::writeData(wb, s, t(c("n", "xx", "xx")), startRow = 4, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

test_that("a mark column and a TOC_<batch> sheet give each item its batches", {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  batch_book(f)
  sh <- suppressWarnings(tfl_ai_read_shell(f))
  toc <- sh$toc
  b <- stats::setNames(toc$batches, toc$output_id)
  expect_identical(unname(b[["T-14-1-1"]]), "safety | Topline")
  expect_identical(unname(b[["L-16-1-1"]]), "safety | Topline")
  expect_true(is.na(b[["T-14-2-1"]]))
  # the marks are not noise in the item's other information
  expect_false(any(grepl("Topline:", toc$info, fixed = TRUE)))
  # written to toc.yml as `sets`
  d <- tempfile("toc-batches")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tflspec:::.ai_toc_yml(d, sh)
  y <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  sets <- lapply(y, function(e) unlist(e$sets))
  names(sets) <- vapply(y, `[[`, "", "id")
  expect_identical(sets[["T-14-1-1"]], c("safety", "Topline"))
  expect_null(sets[["T-14-2-1"]])
})

test_that("a batch's name is one a batch folder can carry; a mark is one of the known ones", {
  expect_identical(tflspec:::.ai_batch_name_of(c("TOC_topline", "Topline TOC", "TOC", "Final_TOC")),
                   c("topline", "Topline", "", "Final"))
  expect_true(all(tflspec:::.ai_is_mark(c("x", "X", " Y ", "1", "○", "yes"))))
  expect_false(any(tflspec:::.ai_is_mark(c("", "xx", "2", NA, "p1"))))
})
