# A company's past TFLs surveyed for their look: the numbers R reads out of
# the RTF files (never their text), the agent's prompt, what it learned read
# back and set against the company standard (#547)

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

test_that("tfl_ai_style_prompt(): the numbers, the tables and the files to open by kind (#547)", {
  d <- withr::local_tempdir()
  home <- file.path(d, "ai")
  # no rtf_dir: past_tfl/ of the folder
  dir.create(file.path(home, "past_tfl"), recursive = TRUE)
  file.copy(list.files(sas_dir(), full.names = TRUE), file.path(home, "past_tfl"), recursive = TRUE)
  p <- tfl_ai_style_prompt(dir = home)
  f <- attr(p, "files")
  expect_identical(names(f), c("prompt", "scan", "tables", "standard", "result"))
  expect_true(all(file.exists(f[c("prompt", "scan", "tables")])))
  expect_true(is.na(f[["standard"]]))
  expect_identical(unname(f["result"]), file.path(home, "company_hints.yml"))
  expect_identical(unname(f[c("prompt", "scan", "tables")]),
                   file.path(home, c("AGENTS-learn.md", "learn/scan.csv", "learn/tables.csv")))
  tabs <- utils::read.csv(f[["tables"]], stringsAsFactors = FALSE)
  expect_identical(nrow(tabs), 4L)
  expect_match(p, "4 files, 4 tables found", fixed = TRUE)
  expect_match(p, "# Learn this company's hints from its past TFLs", fixed = TRUE)
  # a kind a line, its files
  expect_match(p, "- `demog` (1 table(s)): `DMY-901/t_dm.rtf`", fixed = TRUE)
  expect_match(p, "- `listing` (1 table(s)): `DMY-902/l_ae.rtf`", fixed = TRUE)
  # every convention asked for; the form of the answer
  for (k in tflspec:::.ai_style_keys$key) expect_match(p, sprintf("- `%s`: ", k), fixed = TRUE)
  expect_match(p, "tfl_company_hints: {version: 1, files: 4, tables: 4, written: <YYYY-MM-DD>}", fixed = TRUE)
  expect_match(p, "conflicts:", fixed = TRUE)
  expect_match(p, normalizePath(f[["result"]], winslash = "/", mustWork = FALSE), fixed = TRUE)
  # no standard
  expect_match(p, "## The company standard wins", fixed = TRUE)
  expect_match(p, "No company standard was given", fixed = TRUE)
  expect_false(file.exists(file.path(home, "learn", "standard.csv")))
  # a standard: written, and the prompt says it wins
  std <- data.frame(kind = c("all", "demog"), key = c("font", "n_form"),
                    value = c("Courier New 9 pt", "N = xx"))
  p2 <- tfl_ai_style_prompt(dir = home, standard = std)
  expect_true(file.exists(file.path(home, "learn", "standard.csv")))
  expect_identical(utils::read.csv(file.path(home, "learn", "standard.csv"))$key, c("font", "n_form"))
  expect_identical(unname(attr(p2, "files")["standard"]), file.path(home, "learn", "standard.csv"))
  expect_match(p2, "## The company standard wins", fixed = TRUE)
  expect_false(grepl("No company standard was given", p2, fixed = TRUE))
  expect_match(p2, "write no hint for that point", fixed = TRUE)
  expect_error(tfl_ai_style_prompt(dir = home, standard = list()), "`kind`, `key` and `value`")
  # the option sets the folder; an explicit rtf_dir; a missing folder
  withr::local_options(tflspec.ai_home = file.path(d, "opt"))
  p3 <- tfl_ai_style_prompt(sas_dir())
  expect_identical(unname(attr(p3, "files")["result"]), file.path(d, "opt", "company_hints.yml"))
  expect_error(tfl_ai_style_prompt(file.path(d, "nowhere")), "a folder of RTF files")
  expect_error(tfl_ai_style_prompt(), "a folder of RTF files")
})

learned_lines <- c(
  "tfl_company_hints: {version: 1, files: 4, tables: 4, written: 2026-11-03}",
  "kinds:",
  "  all:",
  "    font: {value: \"Times New Roman 9 pt\", seen: 4, of: 4}",
  "  demog:",
  "    evidence: [DMY-901/t_dm.rtf, DMY-902/t_dm.rtf]",
  "    n_form: {value: \"(N=xx)\", seen: 2, of: 2}",
  "    zero: {value: \"0\", seen: 1, of: 3}",
  "    continuous: {value: [n, Mean (SD), Median, \"Min, Max\"], seen: 2, of: 2}",
  "    house_rule: {value: \"bold totals\", seen: 2, of: 2}",
  "    notes: \"a blank row before each SOC\"",
  "  ae_soc_pt:",
  "    first_row: \"Any TEAE\"",
  "conflicts:",
  "  - {kind: demog, key: n_form, standard: \"N = xx\", files: \"(N=xx)\", seen: 2, of: 2}",
  "notes: \"one listing could not be read\"")

test_that("tfl_ai_style_read(): a convention a row; a form few files show, by hand (#547)", {
  d <- tempfile("style-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- file.path(d, "learned.yml")
  writeLines(learned_lines, out)
  x <- tfl_ai_style_read(out)
  expect_s3_class(x, "data.frame")
  expect_identical(x$kind, c("all", "demog", "demog", "demog", "demog", "ae_soc_pt"))
  expect_identical(x$key, c("font", "n_form", "zero", "continuous", "house_rule", "first_row"))
  expect_identical(x$value[4L], "n | Mean (SD) | Median | Min, Max")
  expect_identical(x$seen, c(4L, 2L, 1L, 2L, 2L, NA))
  expect_identical(x$evidence[2L], "DMY-901/t_dm.rtf | DMY-902/t_dm.rtf")
  # fewer than half the files, a name the prompt does not ask for, no count: by hand
  expect_identical(x$adopt, c("proposed", "proposed", "by hand", "proposed", "by hand", "by hand"))
  txt <- attr(x, "text")
  expect_identical(txt[1L], "Learned from 4 file(s), 4 table(s)")
  expect_true("demog: n_form = (N=xx) (2 of 2)" %in% txt)
  expect_true("demog: zero = 0 (1 of 3) -- by hand" %in% txt)
  expect_output(print(x), "one listing could not be read", fixed = TRUE)
  # notes by kind ("" the top level's), not a convention; the conflicts
  nt <- attr(x, "notes")
  expect_identical(names(nt), c("", "demog"))
  expect_identical(unname(nt), c("one listing could not be read", "a blank row before each SOC"))
  cf <- attr(x, "conflicts")
  expect_identical(names(cf), c("kind", "key", "standard", "files", "seen", "of"))
  expect_identical(nrow(cf), 1L)
  expect_identical(unlist(cf[1L, ], use.names = FALSE), c("demog", "n_form", "N = xx", "(N=xx)", "2", "2"))
  expect_true("1 conflict(s) with the company standard" %in% txt)
  expect_error(tfl_ai_style_read(file.path(d, "none.yml")), "nothing learned")
  writeLines("borders: []", out)
  expect_error(tfl_ai_style_read(out), "tfl_company_hints")
  writeLines("tfl_style_learned: {version: 2}
kinds: {}", out)
  expect_error(tfl_ai_style_read(out), "tfl_company_hints")
  writeLines("tfl_company_hints: {version: 1}", out)
  expect_error(tfl_ai_style_read(out), "has no `kinds:`")
  # no notes, no conflicts: empty
  writeLines(c("tfl_company_hints: {version: 1}", "kinds:", "  all:", "    font: Arial"), out)
  y <- tfl_ai_style_read(out)
  expect_identical(nrow(attr(y, "conflicts")), 0L)
  expect_identical(attr(y, "notes"), stats::setNames("", ""))
  # the default file: company_hints.yml in the AI folder
  withr::local_options(tflspec.ai_home = d)
  file.copy(out, file.path(d, "company_hints.yml"))
  expect_identical(tfl_ai_style_read()$key, "font")
})

test_that("tfl_ai_style_diff(): learned against the standard; `all` stands for every kind (#547)", {
  d <- tempfile("style-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- file.path(d, "learned.yml")
  writeLines(learned_lines, out)
  std <- data.frame(kind = c("all", "all", "demog", "demog"),
                    key = c("font", "n_form", "zero", "categorical"),
                    value = c("Times  New Roman 9 pt ", "N = xx", "0", "xx (xx.x%)"))
  x <- tfl_ai_style_diff(out, std)
  expect_identical(names(x), c("kind", "key", "standard", "learned", "seen", "of", "adopt", "status"))
  row <- function(k, key) x[x$kind == k & x$key == key, , drop = FALSE]
  # spaces do not count
  expect_identical(row("all", "font")$status, "same")
  # the company's `all` value, for demog that has none of its own
  expect_identical(unlist(row("demog", "n_form")[c("standard", "learned", "status")], use.names = FALSE),
                   c("N = xx", "(N=xx)", "differs"))
  expect_identical(row("demog", "zero")$status, "same")
  expect_identical(row("demog", "continuous")$status, "new")
  expect_identical(unlist(row("demog", "categorical")[c("learned", "status")], use.names = FALSE),
                   c(NA, "not learned"))
  # by kind: all, demog, ae_soc_pt
  expect_identical(unique(x$kind), c("all", "demog", "ae_soc_pt"))
  # no standard: all new; the read result as well as its file
  y <- tfl_ai_style_diff(tfl_ai_style_read(out))
  expect_identical(unique(y$status), "new")
  expect_error(tfl_ai_style_diff(out, data.frame(a = 1)), "`kind`, `key` and `value`")
  expect_error(tfl_ai_style_diff(list()), "tfl_ai_style_read")
})

test_that("a running header laid out in rows is not a table; the kind is guessed from the lines joined by ' / ' (E7)", {
  skip_if_not_installed("rtfreporter")
  d <- tempfile("style-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  df <- data.frame(Baseline = c("Normal", "High"), Normal = c("5", "1"), High = c("2", "3"))
  mk <- function(title, f) {
    doc <- rtfreporter::rtf_document() |>
      rtfreporter::rtf_section(header = rtfreporter::rtf_header(c(
        list(c("Example Pharma", "Page 1 of 1"), "Protocol: DMY"), as.list(title)))) |>
      rtfreporter::rtf_tables(list(rtfreporter::rtftable(df, border = "tfl")))
    suppressMessages(rtfreporter::generate_rtfreport(doc, file.path(d, f), overwrite = TRUE))
  }
  mk(c("Table 14.3.5", "Shift Table of ALT, Baseline to Worst Post-baseline"), "t_shift.rtf")
  mk(c("Listing 16.2.7", "Listing of Adverse Events"), "l_ae.rtf")
  x <- paste(readLines(file.path(d, "t_shift.rtf"), warn = FALSE), collapse = "")
  expect_true(grepl("header", x, fixed = TRUE))
  t <- tfl_rtf_style_tables(d)
  expect_identical(nrow(t), 2L)
  rownames(t) <- t$file
  expect_identical(t$kind[order(t$file)], c("listing", "shift"))
  expect_false(grepl("Example PharmaPage|DMYTable", t["t_shift.rtf", "title"]))
  expect_match(t["t_shift.rtf", "title"], "Table 14.3.5 / Shift Table of ALT", fixed = TRUE)
  expect_identical(t["t_shift.rtf", "body_rows"], 2L)
  # titles written with no separator are still told apart
  expect_identical(tflspec:::.rtf_kind_guess(c("Table 14.3.5", "Shift Table of ALT")), "shift")
})

# one table, `pre` before it and `post` after it, in a file of its own (#547)
style_rtf <- function(d, f, head, pre = character(), post = character(), cell = "\\pard\\intbl\\ql ",
                      border = "\\clbrdrb\\brdrs") {
  writeLines(c(paste0("{\\rtf1\\ansi", head, "\\paperw12240\\paperh15840"), pre,
               paste0("\\trowd\\trhdr", border, "\\cellx3000", border, "\\cellx4500"),
               paste0(cell, "\\cell", cell, "Drug\\cell\\row"),
               paste0("\\trowd\\cellx3000\\cellx4500", cell, "Age\\cell", cell, "54\\cell\\row"),
               post, "}"), file.path(d, f))
}

test_that("the font is the one the table's cells use, not the font table's first (#547)", {
  d <- withr::local_tempdir()
  tbl <- "{\\fonttbl{\\f0 Courier New;}{\\f2 Arial;}}"
  style_rtf(d, "used.rtf", tbl, cell = "\\pard\\intbl\\f2\\fs18\\ql ")
  # no \fN in the rows: the default font, \deff, else the first
  style_rtf(d, "deff.rtf", paste0("\\deff2", tbl))
  style_rtf(d, "first.rtf", tbl)
  t <- tfl_rtf_style_tables(d)
  rownames(t) <- t$file
  expect_identical(t["used.rtf", "font"], "Arial")
  expect_identical(t["deff.rtf", "font"], "Arial")
  expect_identical(t["first.rtf", "font"], "Courier New")
})

test_that("a footer holding only the page number leaves the footnotes after the table (#547)", {
  d <- withr::local_tempdir()
  tbl <- "{\\fonttbl{\\f0 Arial;}}"
  page <- "{\\footer\\pard\\qr Page {\\field{\\*\\fldinst PAGE}} of {\\field{\\*\\fldinst NUMPAGES}}\\par}"
  style_rtf(d, "page.rtf", tbl, pre = page, post = c("\\pard\\ql Note: x.\\par", "\\pard\\ql Source: y.\\par"))
  # a footer with real text still wins
  text <- "{\\footer\\pard\\ql Note: z.\\par\\pard\\qr Page {\\field{\\*\\fldinst PAGE}}\\par}"
  style_rtf(d, "text.rtf", tbl, pre = text, post = "\\pard\\ql Note: x.\\par")
  t <- tfl_rtf_style_tables(d)
  rownames(t) <- t$file
  expect_identical(t["page.rtf", "footnote_place"], "after")
  expect_identical(t["page.rtf", "footnotes"], 2L)
  expect_identical(t["text.rtf", "footnote_place"], "footer")
  expect_identical(t["text.rtf", "footnotes"], 1L)
})

test_that("a border that is none is not a border (#547)", {
  d <- withr::local_tempdir()
  tbl <- "{\\fonttbl{\\f0 Arial;}}"
  style_rtf(d, "none.rtf", tbl, border = "\\clbrdrt\\brdrnone\\clbrdrb\\brdrnone")
  style_rtf(d, "nil.rtf", tbl, border = "\\clbrdrt\\brdrnil\\clbrdrb\\brdrnil")
  style_rtf(d, "real.rtf", tbl, border = "\\clbrdrt\\brdrs\\clbrdrb\\brdrs")
  s <- tfl_rtf_style_scan(d)
  expect_true(all(s$border_top[s$file %in% c("none.rtf", "nil.rtf")] == "00"))
  expect_true(all(s$border_bottom[s$file %in% c("none.rtf", "nil.rtf")] == "00"))
  expect_true(all(s$border_top[s$file == "real.rtf"][1L] == "11"))
  expect_true(all(s$border_bottom[s$file == "real.rtf"][1L] == "11"))
  t <- tfl_rtf_style_tables(d)
  rownames(t) <- t$file
  expect_identical(t["none.rtf", "rules"], "")
  expect_match(t["real.rtf", "rules"], "above_header", fixed = TRUE)
})

test_that("a folder with no tables: a warning naming it, the prompt still written; the prompt's kinds leave out `all` (#547)", {
  d <- withr::local_tempdir()
  fig <- file.path(d, "figs")
  dir.create(fig)
  writeLines("{\\rtf1\\ansi{\\fonttbl{\\f0 Arial;}}\\pard Figure 14.1 Kaplan-Meier\\par}", file.path(fig, "f1.rtf"))
  expect_warning(p <- tfl_ai_style_prompt(dir = file.path(d, "home"), rtf_dir = fig),
                 "1 RTF file.*figures are not read")
  expect_true(file.exists(attr(p, "files")[["prompt"]]))
  expect_match(p, "0 tables found", fixed = TRUE)
  expect_warning(tfl_ai_style_prompt(dir = file.path(d, "home2"), rtf_dir = withr::local_tempdir()),
                 "0 RTF file")
  expect_match(p, "R's guess from the title: `demog`", fixed = TRUE)
  expect_no_match(p, "guess from the title: `all`", fixed = TRUE)
})
