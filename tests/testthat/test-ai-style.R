# A company's past TFLs surveyed for their look: the numbers R reads out of
# the RTF files (never their text), the agent's prompt, its survey read back

test_that("tfl_rtf_style_scan(): a table's rows -- widths, borders, size, alignment -- and no text", {
  skip_if_not_installed("rtfreporter")
  d <- tempfile("style-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  df <- data.frame(Characteristic = c("Age", "Sex"), Placebo = c("50", "M"), Drug = c("51", "F"))
  doc <- rtfreporter::rtf_document() |>
    rtfreporter::rtf_tables(list(rtfreporter::rtftable(df, border = "tfl", col_rel_width = c(3, 1, 1))),
                            titles = "Table 1 Secret title")
  suppressMessages(rtfreporter::generate_rtfreport(doc, file.path(d, "t1.rtf"), overwrite = TRUE))
  s <- tfl_rtf_style_scan(d)
  expect_true(nrow(s) >= 3L)
  expect_true(all(s$file == "t1.rtf"))
  three <- s[s$n_cells == 3L, , drop = FALSE]
  expect_true(all(three$rel_widths == "3 | 1 | 1"))
  # the header row: a line above and below every cell
  expect_true(any(three$border_top == "111" & three$border_bottom == "111"))
  expect_true(all(!is.na(s$font_size_pt)))
  # (the font table's names, no control word left in them)
  expect_false(grepl("fonttbl|\\\\", s$fonts[1L]))
  expect_true(nzchar(s$fonts[1L]))
  # nothing of the text
  expect_false(any(grepl("Secret|Placebo|Age", unlist(s), fixed = FALSE)))
})

test_that("tfl_ai_style_prompt(): the prompt and the numbers beside the survey's file; read back", {
  skip_if_not_installed("rtfreporter")
  d <- tempfile("style-")
  dir.create(file.path(d, "rtf"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  df <- data.frame(A = "x", B = "1")
  doc <- rtfreporter::rtf_document() |> rtfreporter::rtf_tables(list(rtfreporter::rtftable(df)))
  suppressMessages(rtfreporter::generate_rtfreport(doc, file.path(d, "rtf", "a.rtf"), overwrite = TRUE))
  out <- file.path(d, "standards", "ai_style.yml")
  p <- tfl_ai_style_prompt(file.path(d, "rtf"), out = out)
  f <- attr(p, "files")
  expect_true(all(file.exists(f[c("prompt", "scan")])))
  expect_identical(unname(f["result"]), out)
  expect_match(p, "1 files, 1 tables found", fixed = TRUE)
  expect_match(p, normalizePath(out, winslash = "/", mustWork = FALSE), fixed = TRUE)
  # `dir`: the survey's file in it
  p2 <- tfl_ai_style_prompt(file.path(d, "rtf"), dir = file.path(d, "home"))
  expect_identical(unname(attr(p2, "files")["result"]), file.path(d, "home", "ai_style.yml"))
  expect_error(tfl_ai_style_prompt(file.path(d, "nowhere"), out = out), "a folder of RTF files")
  # the agent's survey, read back
  writeLines(c("tfl_style_survey: {version: 1, files: 12, tables: 40}",
               "borders:",
               "  - {form: \"above, under the column header, at the end\", count: 34}",
               "  - {form: \"above, under the column header\", count: 6}",
               "widths:",
               "  - {kind: demographics, form: \"stub 3 : values 1\", count: 10}",
               "fonts:",
               "  - {form: \"Courier New 9 pt\", count: 12}",
               "notes: \"two files could not be read\""), out)
  x <- tfl_ai_style_read(out)
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 4L)
  expect_identical(x$count, c(34L, 6L, 10L, 12L))
  txt <- attr(x, "text")
  expect_identical(txt[1L], "Survey of 12 file(s), 40 table(s)")
  expect_true("borders: above, under the column header, at the end (34)" %in% txt)
  expect_true("widths: demographics, stub 3 : values 1 (10)" %in% txt)
  expect_output(print(x), "two files could not be read", fixed = TRUE)
  expect_error(tfl_ai_style_read(file.path(d, "none.yml")), "no survey")
})
