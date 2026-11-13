# The page's heading (the "Parameter: ..." line of shift, lab_shift,
# param_change, pk_conc) is the last title line, or where a title row whose
# whole text is {PAGE_HEADING} is (rtfreporter#924): tflspec writes the row
# as a row list rtfreporter reads, and a report that writes no mark is as
# it was (tflspec#674).

hd_skip <- function() {
  skip_if_not_installed("rtfreporter")
  skip_if_not("PAGE_HEADING" %in% gsub("[{}]", "", rtfreporter::rtf_text_tokens()$token),
              "rtfreporter without the {PAGE_HEADING} title row")
}

hd_shell <- function(mark = list(left = "{PAGE_HEADING}")) list(
  `1` = list(center = "Table 14.3.4.2"),
  `2` = list(center = "Shift Table of Laboratory Values"),
  `3` = mark,
  `4` = list(center = "(Safety Analysis Set)"))

test_that("the mark's row is written as the row list rtfreporter reads, its alignment kept", {
  code <- function(mark, ...) {
    o <- tfl_output("T-1", "table", extends = "none",
                    page = list(titles = hd_shell(mark)))
    paste(tfl_report_code(o), collapse = "\n")
  }
  expect_match(code(list(left = "{PAGE_HEADING}")),
               'list(text = "{PAGE_HEADING}", align = "left")', fixed = TRUE)
  expect_match(code(list(center = "{PAGE_HEADING}")),
               'list(text = "{PAGE_HEADING}", align = "center")', fixed = TRUE)
  expect_match(code(list(right = "{PAGE_HEADING}")),
               'list(text = "{PAGE_HEADING}", align = "right")', fixed = TRUE)
  # the other rows are as they were
  expect_match(code(list(left = "{PAGE_HEADING}")),
               'c(c = "(Safety Analysis Set)")', fixed = TRUE)
  # a row of its own height: the same list, the height in it
  o <- tfl_output("T-1", "table", extends = "none", page = list(titles = hd_shell(
    list(left = "{PAGE_HEADING}", row_height_twips = 600L))))
  expect_match(paste(tfl_report_code(o), collapse = "\n"),
               'list(text = "{PAGE_HEADING}", align = "left", row_height_twips = 600)',
               fixed = TRUE)
  # a title without the mark: the code it was
  o <- tfl_output("T-1", "table", extends = "none", page = list(titles = hd_shell(
    list(center = "Parameter: x"))))
  expect_match(paste(tfl_report_code(o), collapse = "\n"),
               'c(c = "Parameter: x")', fixed = TRUE)
  # the mark among other text is not rewritten: rtfreporter says it
  expect_match(code(list(left = "{PAGE_HEADING}", right = "x")),
               'c(l = "{PAGE_HEADING}", r = "x")', fixed = TRUE)
})

test_that("tfl_report(): the heading at the mark's row; last without it; no heading, no row", {
  hd_skip()
  named <- list(`Heading Alpha` = data.frame(Item = "a1", Val = "1"),
                `Heading Beta` = data.frame(Item = "b1", Val = "2"))
  titles <- function(mark) if (is.null(mark)) {
    data.frame(output_id = NA_character_, line = c("1", "2", "3"), left = NA_character_,
               center = c("Table 1", "Shift Table", "(Set)"), right = NA_character_)
  } else {
    data.frame(output_id = NA_character_, line = c("1", "2", "3", "4"),
               left = c(NA, NA, mark, NA), center = c("Table 1", "Shift Table", NA, "(Set)"),
               right = NA_character_)
  }
  texts <- function(doc, i) vapply(doc$contents[[i]]$title, function(r)
    if (is.list(r)) r$text else unname(r[[1L]]), "")
  build <- function(mark, content) {
    sp <- tfl_table_spec(study = c(output_path = "out"),
                         report = data.frame(output_id = NA, auto_title = TRUE),
                         titles = titles(mark))
    tfl_report(sp, content = content)
  }
  doc <- build("{PAGE_HEADING}", named)
  expect_identical(texts(doc, 1L), c("Table 1", "Shift Table", "Heading Alpha", "(Set)"))
  expect_identical(texts(doc, 2L), c("Table 1", "Shift Table", "Heading Beta", "(Set)"))
  expect_identical(doc$contents[[1L]]$title[[3L]]$align, "left")
  expect_true(isTRUE(doc$contents[[1L]]$title[[3L]]$heading))
  # no mark: the heading is the last line, as it was
  doc0 <- build(NULL, named)
  expect_identical(texts(doc0, 1L), c("Table 1", "Shift Table", "(Set)", "Heading Alpha"))
  # a table with no heading: the mark's row is dropped, never printed
  one <- build("{PAGE_HEADING}", list(data.frame(Item = "a1", Val = "1")))
  expect_identical(texts(one, 1L), c("Table 1", "Shift Table", "(Set)"))
})

# ---- the four templates, built to their RTF in a fictional study -----------

hd_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adlb", "advs")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  haven::write_xpt(tflspec:::.ai_chr(pk_adpc(tflspec::adsl)), file.path(d, "adam", "adpc.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "HD-1")
  d
}

hd_args <- list(
  shift = list(population = "SAF", data_id = "adlb_shift", by_levels = c("Drug X", "Control")),
  lab_shift = list(population = "SAF", visit = "CYCLE 2 DAY 1", params = c("ALT", "AST")),
  param_change = list(population = "SAF", data = "ADLB", by = "TRT01A", params = c("ALT", "AST"),
                      where = NULL),
  pk_conc = list(population = "SAF", blq = "BLQFL == \"Y\""),
  demog = list(by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX")))

hd_write <- function(d, id, tpl, titles = NULL, args = list()) {
  tfl_write_output(tfl_output(id, "table", population = "SAF", extends = "defaults",
                              template = list(name = tpl, args = utils::modifyList(hd_args[[tpl]], args)),
                              page = if (!is.null(titles)) list(titles = titles) else list()),
                   file.path(d, "spec", "outputs", paste0(id, ".yml")))
}

# the title rows of the first page: text and alignment, in the RTF's order
hd_page1 <- function(d, id) {
  f <- suppressWarnings(suppressMessages(tfl_run_output(id, dir = d)))
  x <- readLines(f, warn = FALSE)
  rx <- "^\\\\pard\\\\q([lcr])\\\\li0\\\\ri0 \\\\b (.*)\\\\b0 \\\\par$"
  x <- x[grepl(rx, x)]
  data.frame(text = sub(rx, "\\2", x), align = sub(rx, "\\1", x), stringsAsFactors = FALSE)
}
hd_shell_texts <- c("Table 14.3.4.2", "Shift Table of Laboratory Values", "(Safety Analysis Set)")

test_that("shift and param_change: the Parameter line where the mark is, on every page, left", {
  skip_on_cran()
  hd_skip()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- hd_study(withr::local_tempdir())
  for (tpl in c("shift", "param_change")) {
    id <- paste0("T-", toupper(tpl))
    hd_write(d, id, tpl, hd_shell())
    t <- hd_page1(d, id)
    is_head <- startsWith(t$text, "Parameter: ")
    expect_gt(sum(is_head), 1L)
    # page 1: the shell's lines, the parameter's third
    expect_identical(t$text[1:4], c(hd_shell_texts[1:2], t$text[3L], hd_shell_texts[3L]))
    expect_true(is_head[3L], info = tpl)
    # every page: the same four lines in the same order, the heading's `left` kept
    n <- sum(is_head)
    expect_identical(which(is_head), seq(3L, by = 4L, length.out = n), info = tpl)
    expect_identical(t$text[!is_head], rep(hd_shell_texts, n), info = tpl)
    expect_true(all(t$align[is_head] == "l"), info = tpl)
    expect_true(all(t$align[!is_head] == "c"), info = tpl)
    # a heading in the middle, its alignment the row's
    hd_write(d, id, tpl, hd_shell(list(right = "{PAGE_HEADING}")))
    t <- hd_page1(d, id)
    expect_true(all(t$align[startsWith(t$text, "Parameter: ")] == "r"), info = tpl)
  }
})

test_that("a report with no mark keeps the Parameter line last, as it was", {
  skip_on_cran()
  hd_skip()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- hd_study(withr::local_tempdir())
  hd_write(d, "T-SH0", "shift", hd_shell(list(center = "Population line")))
  t <- hd_page1(d, "T-SH0")
  expect_identical(t$text[1:4], c(hd_shell_texts[1:2], "Population line", hd_shell_texts[3L]))
  # (the mark's place taken by a plain line: the heading is a line after the four)
  expect_true(startsWith(t$text[5L], "Parameter: "))
  expect_identical(t$align[5L], "l")
})

test_that("lab_shift and pk_conc: the mark places their heading too", {
  skip_on_cran()
  hd_skip()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- hd_study(withr::local_tempdir())
  for (tpl in c("lab_shift", "pk_conc")) {
    id <- paste0("T-", toupper(tpl))
    hd_write(d, id, tpl, hd_shell())
    t <- hd_page1(d, id)
    expect_identical(t$text[c(1:2, 4L)], hd_shell_texts, info = tpl)
    expect_false(t$text[3L] %in% hd_shell_texts, info = tpl)
    expect_identical(t$align[3L], "l", info = tpl)
  }
})

test_that("no heading to place: the mark's row is dropped (no parameter pages; demog; defaults)", {
  skip_on_cran()
  hd_skip()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- hd_study(withr::local_tempdir())
  hd_write(d, "T-SHNP", "shift", hd_shell(), args = list(page_by = FALSE))
  t <- hd_page1(d, "T-SHNP")
  expect_identical(t$text, hd_shell_texts)
  hd_write(d, "T-DM", "demog", hd_shell())
  t <- hd_page1(d, "T-DM")
  expect_identical(t$text, hd_shell_texts)
  # the mark in the study's defaults: a report with a heading has it there, one without drops it
  df <- file.path(d, "spec", "defaults.yml")
  writeLines(c(readLines(df), "      titles:", "        '1': {center: Table 14.3.4.2}",
               "        '2': {center: Shift Table of Laboratory Values}",
               "        '3': {left: \"{PAGE_HEADING}\"}",
               "        '4': {center: (Safety Analysis Set)}"), df)
  hd_write(d, "T-SHD", "shift")
  hd_write(d, "T-DMD", "demog")
  t <- hd_page1(d, "T-SHD")
  expect_identical(t$text[c(1:2, 4L)], hd_shell_texts)
  expect_true(startsWith(t$text[3L], "Parameter: "))
  expect_identical(hd_page1(d, "T-DMD")$text, hd_shell_texts)
})

test_that("the mark with other text, or twice: rtfreporter's message reaches the run", {
  skip_on_cran()
  hd_skip()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- hd_study(withr::local_tempdir())
  hd_write(d, "T-TXT", "shift", hd_shell(list(left = "{PAGE_HEADING}", right = "Page {PAGE}")))
  expect_error(tfl_run_output("T-TXT", dir = d), "title row of its own")
  hd_write(d, "T-TXT", "shift", hd_shell(list(left = "Parameter {PAGE_HEADING}")))
  expect_error(tfl_run_output("T-TXT", dir = d), "title row of its own")
  two <- hd_shell()
  two[["5"]] <- list(left = "{PAGE_HEADING}")
  hd_write(d, "T-TXT", "shift", two)
  expect_error(tfl_run_output("T-TXT", dir = d), "in 2 rows")
})

test_that("the mark goes through the sheets and back, and the workbook", {
  skip_on_cran()
  hd_skip()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- hd_study(withr::local_tempdir())
  hd_write(d, "T-SH", "shift", hd_shell())
  hd_write(d, "T-PC", "param_change", hd_shell(list(right = "{PAGE_HEADING}")))
  before <- hd_page1(d, "T-SH")
  # the sheets: the row is a titles line with the mark, and back
  sh <- tfl_sheets_from_yaml(d)
  tt <- sh$table$titles
  expect_identical(tt$left[tt$output_id == "T-SH" & tt$line == "3"], "{PAGE_HEADING}")
  expect_identical(tt$right[tt$output_id == "T-PC" & tt$line == "3"], "{PAGE_HEADING}")
  d2 <- withr::local_tempdir()
  tfl_yaml_from_sheets(sh, d2)
  back <- tfl_read_outputs(d2)
  expect_identical(back[["T-SH"]]$page$titles, hd_shell())
  expect_identical(back[["T-PC"]]$page$titles, hd_shell(list(right = "{PAGE_HEADING}")))
  file.copy(file.path(d, "adam"), d2, recursive = TRUE)
  file.copy(file.path(d, "study.yml"), d2, overwrite = TRUE)
  expect_identical(hd_page1(d2, "T-SH"), before)
  # the flat workbook: imported onto reports that have no mark, it puts it there
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  x <- tempfile(fileext = ".xlsx")
  on.exit(unlink(x), add = TRUE)
  tfl_write_spec_xlsx(d, x)
  plain <- hd_shell()[c("1", "2", "4")]
  names(plain) <- c("1", "2", "3")
  hd_write(d, "T-SH", "shift", plain)
  hd_write(d, "T-PC", "param_change", plain)
  res <- tfl_import_spec_xlsx(d, x)
  expect_true(all(c("T-SH / 3", "T-PC / 3") %in% res$changes$key))
  expect_identical(tfl_read_outputs(d)[["T-SH"]]$page$titles, hd_shell())
  expect_identical(hd_page1(d, "T-SH"), before)
  expect_identical(nrow(tfl_check_spec(d)), 0L)
})
