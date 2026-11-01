# A report's file name from the study's pattern: {output_id} as ever, the
# parts of its number, the shell's own id; [ ... ] dropped when a field in
# it is empty

test_that("the pattern's fields: the number's parts, the shell's code, [ ] dropped when empty", {
  e <- list(shell_code = "151_t_dem_sub_01")
  p <- "[m{part_no}_]{shell_code}.rtf"
  expect_identical(.file_name_fill(p, "t.15.1.5.1.a", e), "m1_151_t_dem_sub_01.rtf")
  expect_identical(.file_name_fill(p, "t.15.1.5.1.b", e), "m2_151_t_dem_sub_01.rtf")
  expect_identical(.file_name_fill(p, "t.15.1.5.1", e), "151_t_dem_sub_01.rtf")
  expect_identical(.file_name_fill("{type}_{num:2}_{num}.rtf", "t.15.1.5.1.a"), "t_151_15.1.5.1.rtf")
  expect_identical(.file_name_fill("{output_id}.rtf", "T-14-1-1"), "T-14-1-1.rtf")
  # no shell code: its optional part goes, a bare field is empty
  expect_identical(.file_name_fill("{num:2}[_{shell_code}].rtf", "t.15.1.5.1"), "151.rtf")
  # an unknown field stays as written
  expect_identical(.file_name_fill("{nope}.rtf", "t.1.1"), "{nope}.rtf")
  # a value's odd characters are made file-safe
  expect_identical(.file_name_fill("{shell_code}.rtf", "t.1.1", list(shell_code = "t dm/1")), "t_dm_1.rtf")
})

test_that("the shell's code: the first cell of its top rows in the form; the study's own form", {
  cells <- data.frame(sheet = "s", row = c(1L, 1L, 2L, 9L), col = c(1L, 2L, 1L, 1L),
                      ref = "", text = c("Table 15.1.5.1", "151_t_dem_sub_01", "t_other_01", "t_late_09"),
                      stringsAsFactors = FALSE)
  expect_identical(.shell_code_of(cells), "151_t_dem_sub_01")
  expect_identical(.shell_code_of(cells, "^t_[a-z]+_[0-9]+$"), "t_other_01")
  expect_true(is.na(.shell_code_of(cells[1L, ])))
})

test_that("a report's RTF path: the study's pattern with its TOC entry's code; a part takes its number's", {
  st <- list(study = list(settings = list(file = "[m{part_no}_]{shell_code}.rtf",
                                          output_path = "output/tfl")),
             toc = list(outputs = list(list(id = "t.15.1.5.1", shell_code = "151_t_dem_sub_01"))))
  class(st) <- "tfl_study_spec"
  o <- tfl_output("t.15.1.5.1.a", "table")
  expect_identical(.output_rtf_path(o, st), file.path("output/tfl", "m1_151_t_dem_sub_01.rtf"))
  o$id <- "t.15.1.5.1"
  expect_identical(.output_rtf_path(o, st), file.path("output/tfl", "151_t_dem_sub_01.rtf"))
})

test_that("tfl_ai_prepare(): the code at the top of each shell goes to the TOC entry (shell_code)", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  f <- tempfile(fileext = ".xlsx")
  d <- tempfile()
  on.exit(unlink(c(f, d), recursive = TRUE), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(`Output No.` = paste("Table", c("14.1.1", "14.1.2")),
                                            Title = c("Demographics", "Disposition"),
                                            check.names = FALSE), startRow = 2)
  body <- list(t(c("", "Placebo", "Drug X")), t(c("n", "xx", "xx")), t(c("Mean (SD)", "xx.x", "xx.x")))
  codes <- c("141_t_dem_01", NA)
  for (k in 1:2) {
    s <- sprintf("t.14.1.%d", k)
    openxlsx::addWorksheet(wb, s)
    lines <- c(if (!is.na(codes[k])) list(codes[k]),
               list(sprintf("Table 14.1.%d  Title", k), "Safety"), body)
    for (j in seq_along(lines)) openxlsx::writeData(wb, s, lines[[j]], startRow = j, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  invisible(tfl_ai_prepare(d, f, adam = "adam", quiet = TRUE, templates = FALSE))
  toc <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  sc <- vapply(toc, function(e) e$shell_code %||% NA_character_, "")
  expect_identical(unname(sc), c("141_t_dem_01", NA))
})
