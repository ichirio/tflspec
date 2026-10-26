# A TOC item with no shell matched to one sheet by priority: shells of a
# similar layout put together, a sheet holding two or three reports whose
# numbers are part headings in its body

# A company-style workbook: t15.2.4.1 writes its own number in its title
# rows and, as part headings in its body, 15.2.4.2.a/.b and 15.2.4.3.a;
# t15.2.5 writes its own (15.2.5.1) and 15.2.4.3.a in its title rows, and
# 15.3.2.a and 15.9.1.a in its body; t15.3.1, after it, writes its own and
# 15.3.2.a and 15.9.1.a in its body
priority_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  ids <- c("15.2.4.1", "15.2.4.2", "15.2.4.3", "15.2.5.1", "15.3.1", "15.3.2", "15.9.1")
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    `Output No.` = paste("Table", ids),
    Title = c("Adverse Events by System Organ Class", "Adverse Events by Severity",
              "Adverse Events by Relationship", "Serious Adverse Events", "Laboratory Values",
              "Laboratory Shifts", "Other Findings"),
    check.names = FALSE))
  hdr <- t(c("", "Placebo", "Drug X"))
  row <- t(c("Any event", "xx (xx.x)", "xx (xx.x)"))
  shell <- function(s, lines) {
    openxlsx::addWorksheet(wb, s)
    for (k in seq_along(lines)) {
      openxlsx::writeData(wb, s, lines[[k]], startRow = k, colNames = FALSE)
    }
  }
  shell("t15.2.4.1", list("Table 15.2.4.1  Adverse Events by System Organ Class",
                          "Safety Analysis Set", hdr, row,
                          "Table 15.2.4.2.a  Adverse Events by Severity: Mild", hdr, row,
                          "Table 15.2.4.2.b  Adverse Events by Severity: Severe", hdr, row,
                          "Table 15.2.4.3.a  Adverse Events by Relationship", hdr, row))
  shell("t15.2.5", list("Table 15.2.5.1  Serious Adverse Events",
                        "Table 15.2.4.3.a  Adverse Events by Relationship (same layout)",
                        "Safety Analysis Set", hdr, row,
                        "Table 15.3.2.a  Laboratory Shifts", hdr, row,
                        "Table 15.9.1.a  Other Findings", hdr, row))
  shell("t15.3.1", list("Table 15.3.1  Laboratory Values", "Safety Analysis Set", hdr, row,
                        "Table 15.3.2.a  Laboratory Shifts", hdr, row,
                        "Table 15.9.1.a  Other Findings", hdr, row))
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

priority_study <- function() {
  d <- tempfile("sheetpri-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  d
}

prep_priority <- function(d, f, ...) {
  tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE, ...)
}

test_that("a TOC item with no shell goes to one sheet by priority, the others candidates", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- priority_book(tempfile(fileext = ".xlsx"))
  d <- priority_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dr <- prep_priority(d, f, dry_run = TRUE)
  n <- dr$numbers
  g <- n[n$source == "guess", , drop = FALSE]
  at <- function(s, num) g[g$sheet == s & g$number == paste("Table", num), , drop = FALSE]
  # the parts of one report in the body of the sheet holding several: made
  for (p in c("15.2.4.2.a", "15.2.4.2.b")) {
    r <- at("t15.2.4.1", p)
    expect_true(r$make)
    expect_identical(r$reason, sprintf(paste0("part letter differs (Table %s for Table 15.2.4.2); ",
                                              "in the body only; not an exact match"), p))
  }
  # in one sheet's title rows, another's body only: the title rows win
  expect_true(at("t15.2.5", "15.2.4.3.a")$make)
  expect_identical(at("t15.2.5", "15.2.4.3.a")$reason, paste0(
    "part letter differs (Table 15.2.4.3.a for Table 15.2.4.3); in the sheet's title rows; ",
    "not an exact match; 1st choice of 2 sheets"))
  expect_false(at("t15.2.4.1", "15.2.4.3.a")$make)
  expect_identical(at("t15.2.4.1", "15.2.4.3.a")$reason, paste0(
    "2nd choice; part letter differs (Table 15.2.4.3.a for Table 15.2.4.3); in the body only; ",
    "not an exact match; chosen: sheet 't15.2.5'"))
  # both in the body: the sheet whose own number is nearest (15.3.1 for
  # 15.3.2), though later in the workbook
  expect_true(at("t15.3.1", "15.3.2.a")$make)
  expect_false(at("t15.2.5", "15.3.2.a")$make)
  expect_match(at("t15.3.1", "15.3.2.a")$reason, "its own number is nearest", fixed = TRUE)
  # alike and as near: the first in the workbook
  expect_true(at("t15.2.5", "15.9.1.a")$make)
  expect_false(at("t15.3.1", "15.9.1.a")$make)
  expect_match(at("t15.2.5", "15.9.1.a")$reason, "first in the workbook of those alike",
               fixed = TRUE)
  # one sheet a TOC item, every item a sheet's: no no-shell request
  expect_identical(sum(g$make), 5L)
  expect_false("no-shell" %in% dr$requests$request)
  rq <- dr$requests
  expect_identical(rq$outputs[rq$request == "t15.2.4.1"],
                   "Table 15.2.4.1, Table 15.2.4.2.a, Table 15.2.4.2.b")
  expect_identical(rq$outputs[rq$request == "t15.2.5"],
                   "Table 15.2.5.1, Table 15.2.4.3.a, Table 15.9.1.a")
  expect_identical(rq$outputs[rq$request == "t15.3.1"], "Table 15.3.1, Table 15.3.2.a")
  expect_identical(dr$toc$shell[dr$toc$toc_id == "Table 15.2.4.3"], "t15.2.5")
  # (the rule's rows: no reason)
  expect_true(all(is.na(n$reason[n$source == "rule"])))
})

test_that("the requests say why a sheet was chosen, not an exact match, the other candidates", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- priority_book(tempfile(fileext = ".xlsx"))
  d <- priority_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  r <- prep_priority(d, f)
  rq <- r$requests
  text <- function(s) paste(readLines(file.path(d, rq$file[rq$request == s]), encoding = "UTF-8"),
                            collapse = "\n")
  p <- text("t15.2.5")
  expect_match(p, paste0("- Table 15.2.4.3.a: the TOC item Table 15.2.4.3 has no shell of its own; ",
                         "this sheet was chosen for it by priority (part letter differs"), fixed = TRUE)
  expect_match(p, "It is NOT an exact match", fixed = TRUE)
  expect_match(p, "Other candidate sheets (references only): 't15.2.4.1' (2nd choice: part letter",
               fixed = TRUE)
  p <- text("t15.2.4.1")
  expect_match(p, paste0("- Table 15.2.4.3.a: the TOC item Table 15.2.4.3: its shell is more likely ",
                         "sheet 't15.2.5' (1st choice: part letter differs"), fixed = TRUE)
  expect_match(p, "this sheet is its 2nd choice (part letter differs", fixed = TRUE)
  # (one sheet alone: no other candidate listed)
  expect_false(grepl("Other candidate sheets", p, fixed = TRUE))
})

test_that("numbers = over a priority guess: the person's sheet makes it, the others refer", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- priority_book(tempfile(fileext = ".xlsx"))
  d <- priority_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dr <- prep_priority(d, f, dry_run = TRUE, numbers = data.frame(
    sheet = "t15.2.4.1", number = "Table 15.2.4.3.a", make = TRUE))
  n <- dr$numbers
  w <- n$number == "Table 15.2.4.3.a"
  expect_identical(n$sheet[w], c("t15.2.4.1", "t15.2.5"))
  expect_identical(n$make[w], c(TRUE, FALSE))
  expect_identical(n$source[w], c("given", "guess"))
  expect_identical(n$reason[w], c("set by the person", "the person made it in sheet 't15.2.4.1'"))
  rq <- dr$requests
  expect_match(rq$outputs[rq$request == "t15.2.4.1"], "Table 15.2.4.3.a", fixed = TRUE)
  expect_identical(rq$outputs[rq$request == "t15.2.5"], "Table 15.2.5.1, Table 15.9.1.a")
  expect_identical(dr$toc$shell[dr$toc$toc_id == "Table 15.2.4.3"], "t15.2.4.1")
  # the chosen sheet's guess made a reference: no promotion of the 2nd, the
  # item in no sheet (the no-shell request's)
  dr <- prep_priority(d, f, dry_run = TRUE, numbers = data.frame(
    sheet = "t15.2.5", number = "Table 15.2.4.3.a", make = FALSE))
  expect_identical(dr$requests$outputs[dr$requests$request == "no-shell"], "Table 15.2.4.3")
})

test_that("each part its own report: two parts in two sheets both made, one part once", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- tempfile(fileext = ".xlsx")
  d <- priority_study()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(`Output No.` = paste("Table", c("20.1", "20.2", "20.3")),
                                            Title = c("Disposition", "Demographics", "Exposure"),
                                            check.names = FALSE))
  hdr <- t(c("", "Placebo", "Drug X"))
  shell <- function(s, lines) {
    openxlsx::addWorksheet(wb, s)
    for (k in seq_along(lines)) {
      openxlsx::writeData(wb, s, lines[[k]], startRow = k, colNames = FALSE)
    }
  }
  # (the .b part's sheet first in the workbook; the .a part in the body of
  # the other, with 20.3.a, which the first sheet writes too)
  shell("t20.2", list("Table 20.2  Demographics", "Table 20.1.b  Disposition: Part B", hdr,
                      "Table 20.3.a  Exposure", hdr))
  shell("t20.9", list("Listing 20.9  Notes", hdr, "Table 20.1.a  Disposition: Part A", hdr,
                      "Table 20.3.a  Exposure", hdr))
  openxlsx::saveWorkbook(wb, f)
  dr <- prep_priority(d, f, dry_run = TRUE)
  g <- dr$numbers[dr$numbers$source == "guess", , drop = FALSE]
  made <- g[g$make, , drop = FALSE]
  expect_identical(sort(paste(made$sheet, made$number)),
                   c("t20.2 Table 20.1.b", "t20.2 Table 20.3.a", "t20.9 Table 20.1.a"))
  # one part in two sheets: made once
  expect_identical(sum(g$number == "Table 20.3.a" & g$make), 1L)
  # the item's shell: its first part's sheet, though later in the workbook
  t <- dr$toc[dr$toc$toc_id == "Table 20.1", , drop = FALSE]
  expect_identical(t$shell, "t20.9")
  expect_identical(t$request, "t20.2 | t20.9")
  expect_false("no-shell" %in% dr$requests$request)
})
