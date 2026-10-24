# The shell sheets first, the TOC as candidates: a workbook of the real
# shape (helper-ai-sheets.R)

test_that("the sheets' roles: every TOC sheet named so, the shells, sheet_roles over them", {
  skip_if_not_installed("openxlsx")
  f <- real_shape_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f))
  x <- tfl_ai_read_shell(f)
  expect_identical(unname(x$roles[c("TOC", "Topline_TOC", "Cover", "Notes", "t.99.9")]),
                   c("toc", "toc", "shell", "other", "shell"))
  y <- tfl_ai_read_shell(f, sheet_roles = c(Topline_TOC = "other", Cover = "other"))
  expect_identical(unname(y$roles[c("Topline_TOC", "Cover")]), c("other", "other"))
  expect_false("Cover" %in% y$shells$sheet)
  expect_error(tfl_ai_read_shell(f, sheet_roles = c(Nope = "other")), "no sheet")
  expect_error(tfl_ai_read_shell(f, sheet_roles = c(Cover = "page")), "the roles are")
})

test_that("TOC items: every TOC's, an item once, the sheets listing it", {
  skip_if_not_installed("openxlsx")
  f <- real_shape_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f))
  t <- tfl_ai_read_shell(f)$toc
  expect_identical(t$output_id, paste("Table", c("14.1.1", "15.1.2", "15.1.4.2", "15.3.1",
                                                 "16.1.1", "15.2.1", "15.9.1")))
  expect_identical(t$toc_sheets[t$output_id == "Table 15.1.2"], "TOC | Topline_TOC")
  expect_identical(t$toc_sheets[t$output_id == "Table 15.9.1"], "Topline_TOC")
  expect_match(t$info[t$output_id == "Table 15.9.1"], "listed only in TOC sheet 'Topline_TOC'",
               fixed = TRUE)
  expect_identical(t$title[t$output_id == "Table 15.3.1"], "Adverse Events by SOC and PT")
})

test_that("a sheet's candidates: by what it writes, then its name, then a close number or title", {
  skip_if_not_installed("openxlsx")
  f <- real_shape_book(tempfile(fileext = ".xlsx"))
  on.exit(unlink(f))
  x <- tfl_ai_read_shell(f)
  first <- function(s) x$toc$output_id[x$cand[[s]]$i[1L]]
  # a sheet named wrong: its number written in it
  expect_identical(first("t.99.9"), "Table 15.3.1")
  expect_match(x$shells$candidates[x$shells$sheet == "t.99.9"], "its number is written in the sheet")
  # Part sheets the TOC names one mistyped shell for
  expect_identical(first("t.15.1.4.2.2-1"), "Table 15.1.4.2")
  expect_identical(first("t.15.1.4.2.2-2"), "Table 15.1.4.2")
  # a sheet writing no number: its name
  expect_identical(first("t.15.2.1"), "Table 15.2.1")
  # a cover: no number, no candidate -- a shell by its content, marked
  expect_true(x$shells$guessed[x$shells$sheet == "Cover"])
  # TOC items no sheet chose: no shell found
  expect_identical(x$toc$output_id[is.na(x$toc$sheets)], c("Table 16.1.1", "Table 15.9.1"))
  # a format sample in a heading is no part: no parts read
  expect_null(x$toc$parts)
})

test_that("the requests: a sheet each, the TOC items with no shell one; what they say", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- real_shape_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  dr <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE,
                       dry_run = TRUE)
  expect_identical(unname(dr$counts), c(7L, 7L, 2L))
  expect_identical(dr$requests$request, c("t.14.1.1", "t.15.1.2", "t.15.1.4.2.2-1",
                                          "t.15.1.4.2.2-2", "t.99.9", "t.15.2.1", "Cover", "no-shell"))
  expect_identical(dr$shells$request, dr$shells$sheet)
  expect_identical(dr$toc$request[dr$toc$toc_id == "Table 16.1.1"], "no-shell")
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE)
  expect_identical(r$requests$request, dr$requests$request)
  expect_identical(r$requests$sheets[r$requests$request == "t.99.9"], "t.99.9")
  txt <- function(nm) paste(readLines(file.path(d, r$requests$file[r$requests$request == nm]),
                                      encoding = "UTF-8"), collapse = "\n")
  p <- txt("t.15.1.4.2.2-1")
  expect_match(p, "This request is the shell sheet 't.15.1.4.2.2-1' (its sheet name, not a report's number).",
               fixed = TRUE)
  expect_match(p, "- Table 15.1.4.2 (a close number", fixed = TRUE)
  expect_match(p, "Read the reports from the sheet first", fixed = TRUE)
  expect_match(p, "The TOC and the sheet names can hold mistakes", fixed = TRUE)
  expect_match(p, "## Sheet \"t.15.1.4.2.2-1\"", fixed = TRUE)
  expect_false(grepl("## Sheet \"t.15.1.4.2.2-2\"", p, fixed = TRUE))
  expect_match(p, "write the ids you chose in the header's `outputs`", fixed = TRUE)
  expect_match(txt("Cover"), "If it is not a shell (a cover, notes, a change history), write no",
               fixed = TRUE)
  ns <- txt("no-shell")
  expect_match(ns, "- Table 16.1.1; title: Listing of Deaths", fixed = TRUE)
  expect_match(ns, "These TOC items were found in no shell sheet", fixed = TRUE)
})

test_that("a sheet's answer: its ids the AI's; one no TOC item has, said; none, said", {
  o <- function(id) tfl_output(id, "table", population = "SAF")
  ctx <- list(sheet_mode = TRUE, toc_keys = tflspec:::.ai_id_key(c("Table 15.1.4.2", "Table 15.3.1")))
  pr <- .ai_check_table(list(outputs = list(`Table-15.1.4.2.a` = o("Table-15.1.4.2.a"))), ctx)
  # (the report files are empty here: only what the ids say is looked at)
  expect_false(any(grepl("asked for|was not asked", pr$problem)))
  expect_false(any(grepl("no TOC item", pr$problem)))
  pr <- .ai_check_table(list(outputs = list(`Table-15.7.7` = o("Table-15.7.7"))), ctx)
  expect_true(any(grepl("is no TOC item's number", pr$problem) & pr$severity == "warning"))
  pr <- .ai_check_table(list(outputs = list()), ctx)
  expect_identical(pr$severity, "warning")
  expect_match(pr$problem, "the sheet is no shell", fixed = TRUE)
})

test_that("unit = \"toc\": a request a TOC item's reports, as before", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- real_shape_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE,
                      unit = "toc", dry_run = TRUE)
  expect_false("no-shell" %in% r$requests$request)
  expect_true(all(r$toc$toc_id %in% unlist(strsplit(r$requests$outputs, ", "))))
})

test_that("a TOC header of two rows, Title merged over its parts: the title both", {
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", t(c("Shell No.", "Output No.", "Title", "", "Population")),
                      startRow = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, "TOC", cols = 3:4, rows = 1)
  openxlsx::writeData(wb, "TOC", t(c("", "", "1", "2", "")), startRow = 2, colNames = FALSE)
  openxlsx::writeData(wb, "TOC", rbind(c("t.14.1.1", "Table 14.1.1", "Disposition", "All Subjects", "ENR"),
                                       c("t.14.1.2", "Table 14.1.2", "Demographics", "Safety", "SAF")),
                      startRow = 3, colNames = FALSE)
  openxlsx::addWorksheet(wb, "t.14.1.1")
  openxlsx::writeData(wb, "t.14.1.1", "Table 14.1.1  Disposition", colNames = FALSE)
  openxlsx::saveWorkbook(wb, f)
  t <- tfl_ai_read_shell(f)$toc
  expect_identical(t$title, c("Disposition / All Subjects", "Demographics / Safety"))
  expect_identical(t$population, c("ENR", "SAF"))
})

test_that("a sheet's cells read at once are those read row by row", {
  f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  members <- utils::unzip(f, list = TRUE)$Name
  sst <- .xl_member(f, "xl/sharedStrings.xml", members)
  shared <- .xl_si_text(regmatches(sst, gregexpr("(?s)<si>.*?</si>|<si/>", sst, perl = TRUE))[[1L]])
  indent <- .xl_indents(.xl_member(f, "xl/styles.xml", members))
  for (m in grep("^xl/worksheets/sheet[0-9]+[.]xml$", members, value = TRUE)) {
    x <- .xl_member(f, m, members)
    expect_identical(.xl_sheet_cells(x, "s", shared, indent),
                     .xl_sheet_cells_rows(x, "s", shared, indent), info = m)
  }
})

test_that("add = TRUE: a TOC item asked for already stops; overwrite asks it anew, once", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- real_shape_book(tempfile(fileext = ".xlsx"))
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE)
  expect_error(tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE,
                              add = TRUE), "already asked for")
  r2 <- suppressMessages(tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent",
                                        templates = FALSE, add = TRUE, overwrite = TRUE))
  # (the old requests asked anew, not beside them: a TOC item can be a
  # candidate of several sheets, each sheet's request once)
  expect_setequal(tfl_ai_requests(d)$request, r$requests$request)
})

test_that("a sheet is asked for the TOC items it is the best sheet of; a close number elsewhere's stays a candidate", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- tempfile(fileext = ".xlsx")
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    `Output No.` = paste("Table", c("14.1.1", "14.1.2", "14.1.3")),
    Title = c("Subject Disposition", "Subject Disposition by Site", "Subject Disposition by Country"),
    check.names = FALSE), startRow = 2)
  body <- list(t(c("", "Placebo", "Drug X")), t(c("n", "xx", "xx")), t(c("Mean (SD)", "xx.x", "xx.x")))
  for (k in 1:3) {
    s <- sprintf("t.14.1.%d", k)
    openxlsx::addWorksheet(wb, s)
    lines <- c(list(sprintf("Table 14.1.%d  Subject Disposition", k), "All Subjects"), body)
    for (j in seq_along(lines)) openxlsx::writeData(wb, s, lines[[j]], startRow = j, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  r <- tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, mode = "agent", templates = FALSE)
  rq <- r$requests
  # each sheet its own TOC item, not the close numbers of the others
  expect_identical(rq$n[rq$request %in% sprintf("t.14.1.%d", 1:3)], c(1L, 1L, 1L))
  expect_false(grepl("14.1.2", rq$outputs[rq$request == "t.14.1.1"], fixed = TRUE))
  p <- paste(readLines(file.path(d, rq$file[rq$request == "t.14.1.1"]), encoding = "UTF-8"),
             collapse = "\n")
  expect_match(p, "sheet 't.14.1.2' fits it better", fixed = TRUE)
  # an answer of its own report alone: no error for the others' (theirs to answer)
  own <- list(a = "Table-14.1.1", b = "Table-14.1.2", c = "Table-14.1.3")
  ans <- function(ids) list(outputs = stats::setNames(as.list(ids), ids))
  expect_identical(.ai_sheet_answer_ids("Table-14.1.1", ans("Table-14.1.1"), "a", own),
                   "Table-14.1.1")
  # another sheet's report in the answer: that request's, not this one's
  expect_identical(.ai_sheet_answer_ids("Table-14.1.1", ans(c("Table-14.1.1", "Table-14.1.2")),
                                        "a", own), "Table-14.1.1")
  # a report the sheet writes that no request was asked for: written
  expect_identical(.ai_sheet_answer_ids("Table-14.1.1", ans(c("Table-14.1.1", "Table-14.1.9")),
                                        "a", own), c("Table-14.1.1", "Table-14.1.9"))
  # a sheet asked for none (its name stands in): the answer's reports
  own$g <- character()
  expect_identical(.ai_sheet_answer_ids("Cover", ans("Table-14.1.8"), "g", own), "Table-14.1.8")
})
