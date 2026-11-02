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

# fictional SAS ODS RTF (fixtures/sas-rtf, make-fixtures.R): DMY-901 / DMY-902
sas_dir <- function() test_path("fixtures", "sas-rtf")

test_that("SAS ODS RTF: a table over two pages is one table, its repeated header counted once (#547)", {
  s <- tfl_rtf_style_scan(sas_dir())
  ae <- s[s$file == "DMY-901/t_ae_soc.rtf", , drop = FALSE]
  expect_identical(unique(ae$table), 1L)
  expect_identical(sort(unique(ae$page)), 1:2)
  expect_identical(sum(ae$header), 2L)
  expect_identical(nrow(ae), 8L)
  expect_identical(ae$row, seq_len(8L))
  # the files by their study's folder
  expect_setequal(unique(s$file), c("DMY-901/t_ae_soc.rtf", "DMY-901/t_dm.rtf",
                                    "DMY-902/l_ae.rtf", "DMY-902/t_lb_chg.rtf"))
  expect_false(any(grepl("Diarrhoea|Fatigue|Demographic", unlist(s))))
})

test_that("tfl_rtf_style_tables(): a table's kind, header, widths, rules, page, titles (#547)", {
  t <- tfl_rtf_style_tables(sas_dir())
  rownames(t) <- t$file
  expect_identical(t[c("DMY-901/t_dm.rtf", "DMY-901/t_ae_soc.rtf", "DMY-902/t_lb_chg.rtf",
                       "DMY-902/l_ae.rtf"), "kind"],
                   c("demog", "ae_soc_pt", "param_change", "listing"))
  ae <- t["DMY-901/t_ae_soc.rtf", ]
  expect_identical(c(ae$pages, ae$header_rows, ae$body_rows, ae$n_cols), c(2L, 2L, 6L, 3L))
  expect_identical(ae$stub_ratio, 2)
  expect_identical(ae$rules, "above_header | under_header | end")
  # a spanner's underline over its columns only
  expect_identical(t["DMY-902/t_lb_chg.rtf", "rules"],
                   "above_header | under_spanner | under_header | end")
  expect_identical(t["DMY-901/t_dm.rtf", "stub_ratio"], 1.5)
  expect_identical(t$orientation, ifelse(t$file == "DMY-902/t_lb_chg.rtf", "portrait", "landscape"))
  expect_identical(unique(t$margins_in), "1 | 1 | 1 | 1")
  expect_identical(unique(t$font), "Times New Roman")
  expect_identical(unique(t$font_size_pt), 9)
  # titles in the page header, centred, without its running line (page n of m);
  # footnotes in the page footer, left
  expect_identical(unique(t$title_place), "header")
  expect_identical(unique(t$title_align), "c")
  expect_identical(ae$title, paste("Table 14.3.1.2",
    "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
    "(Safety Population)", sep = " / "))
  expect_identical(unique(t$footnote_place), "footer")
  expect_identical(unique(t$footnote_align), "l")
  expect_identical(ae$footnotes, 2L)
  expect_identical(c(t["DMY-901/t_dm.rtf", "value_align"], t["DMY-902/l_ae.rtf", "value_align"]),
                   c("c", "l"))
})

test_that("titles written as paragraphs before the table, footnotes after it", {
  d <- tempfile("style-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c("{\\rtf1\\ansi{\\fonttbl{\\f0 Courier New;}}\\paperw12240\\paperh15840",
               "\\pard\\qc{\\b Table 14.2.1}\\par\\pard\\qc Shift Table of ALT\\par",
               "\\trowd\\trhdr\\clbrdrb\\brdrs\\cellx3000\\clbrdrb\\brdrs\\cellx4500",
               "\\pard\\intbl\\ql \\cell\\pard\\intbl\\qc Normal\\cell\\row",
               "\\trowd\\cellx3000\\cellx4500\\pard\\intbl\\ql Low\\cell\\pard\\intbl\\qc 3\\cell\\row",
               "\\pard\\ql Note: x.\\par}"), file.path(d, "t.rtf"))
  t <- tfl_rtf_style_tables(d)
  expect_identical(t$title_place, "before")
  expect_identical(t$title, "Table 14.2.1 / Shift Table of ALT")
  expect_identical(t$kind, "shift")
  expect_identical(t$footnote_place, "after")
  expect_identical(t$footnotes, 1L)
  expect_identical(t$rules, "under_header")
  expect_identical(t$orientation, "portrait")
  expect_error(tfl_rtf_style_tables(file.path(d, "none")), "a folder of RTF files")
})
