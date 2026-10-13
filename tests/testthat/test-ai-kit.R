# The AI kit (tflspec_ai_manual(dir)) and the review of what an AI
# returns (tfl_ai_review()).

kit_dir <- function() {
  d <- file.path(tempdir(), "ai-kit-test")
  if (!dir.exists(d) || length(list.files(d)) < 8L) {
    unlink(d, recursive = TRUE)
    suppressMessages(tflspec_ai_manual(d))
  }
  d
}
kit_file <- function(name) file.path(kit_dir(), name)

kit_toc <- function() {
  tfl_read_toc(kit_file("05_example_toc_shell.xlsx"), sheet = "TOC", skip = 1L,
               map = list(output_id = "Shell Number", title = c("Title1", "Title2"),
                          population = "Population",
                          footnote = c("Footnote1", "Footnote2")))
}

test_that("the kit is eight flat files, small enough to attach", {
  skip_if_not_installed("readxl")
  d <- file.path(tempdir(), "ai-kit-files")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_message(kit <- tflspec_ai_manual(d), "Attach all files in")
  expect_identical(basename(kit), unname(tflspec:::.ai_kit_files))
  expect_identical(list.files(d), sort(unname(tflspec:::.ai_kit_files)))
  expect_false(any(dir.exists(file.path(d, list.files(d)))))
  expect_lt(sum(file.size(kit)), 1024^2)
  # split: one blank workbook a spec
  unlink(d, recursive = TRUE)
  kit2 <- suppressMessages(tflspec_ai_manual(d, split = TRUE))
  expect_length(kit2, 11L)
  expect_true(all(c("03a_blank_ard_spec.xlsx", "03d_blank_listing_spec.xlsx") %in% basename(kit2)))
  expect_identical(sort(readxl::excel_sheets(file.path(d, "03d_blank_listing_spec.xlsx"))),
                   sort(c("listings", "listing_cols", "about", "_columns")))
})

test_that("the blank specs are every sheet of the specs, empty, and read back", {
  skip_if_not_installed("readxl")
  f <- kit_file("03_blank_specs.xlsx")
  sh <- readxl::excel_sheets(f)
  ks <- tflspec:::.spec_kind_sheets()
  expect_true(all(unlist(ks) %in% sh))
  expect_true("_columns" %in% sh)
  expect_s3_class(tfl_read_ard_spec(f, check = FALSE), "tfl_ard_spec")
  expect_s3_class(tfl_read_table_spec(f), "tfl_table_spec")
  expect_s3_class(tfl_read_report_spec(f), "tfl_table_spec")
  expect_s3_class(tfl_read_listing_spec(f, check = FALSE), "tfl_listing_spec")
  # _columns says every column of the sheets, from the catalog
  cols <- as.data.frame(readxl::read_excel(f, "_columns", col_types = "text"))
  expect_named(cols, c("spec", "sheet", "column", "description", "example"))
  for (s in setdiff(unlist(ks), "study")) {
    hdr <- names(readxl::read_excel(f, s, n_max = 0))
    expect_true(all(setdiff(hdr, c("output_id", "note")) %in% cols$column[cols$sheet == s]),
                info = s)
  }
})

test_that("the blank figure design lists every piece and still reads", {
  f <- kit_file("04_blank_figure.yml")
  txt <- readLines(f)
  parts <- tfl_fig_parts()
  for (pc in unique(parts$piece[parts$section != "plot"])) {
    expect_true(any(grepl(paste0(": ", pc, " -- "), txt, fixed = TRUE)), info = pc)
  }
  expect_no_error(yaml::read_yaml(f))
})

test_that("the example specs read, review clean and match the example TOC", {
  skip_if_not_installed("readxl")
  f <- kit_file("06_example_specs.xlsx")
  a <- tfl_read_ard_spec(f)
  t <- tfl_read_table_spec(f)
  r <- tfl_read_report_spec(f)
  l <- tfl_read_listing_spec(f)
  rv <- tfl_review_spec(spec = t, ard = a, listings = l)
  expect_identical(nrow(rv), 0L)
  toc <- kit_toc()
  expect_setequal(toc$report$output_id, r$report$output_id)
  expect_identical(tflspec:::.ai_lines(toc$titles, "T-2"), tflspec:::.ai_lines(r$titles, "T-2"))
  expect_identical(tflspec:::.ai_lines(toc$footnotes, "T-2"), tflspec:::.ai_lines(r$footnotes, "T-2"))
  d <- tfl_read_fig_design(kit_file("07_example_figure.yml"))
  expect_identical(nrow(tfl_check_fig_design(d)), 0L)
})

test_that("the example runs on the pilot data: the ARD, the tables, the listing, the figure", {
  skip_on_cran()
  skip_if_not_installed("pharmaverseadam")
  skip_if_not_installed("readxl")
  skip_if_not_installed("rtfreporter")
  f <- kit_file("06_example_specs.xlsx")
  st <- file.path(tempdir(), "ai-kit-study")
  dir.create(file.path(st, "data", "adam"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st, recursive = TRUE), add = TRUE)
  for (d in c("adsl", "adae", "advs")) {
    saveRDS(getExportedValue("pharmaverseadam", d), file.path(st, "data", "adam", paste0(d, ".rds")))
  }
  a <- tfl_read_ard_spec(f)
  t <- tfl_read_table_spec(f)
  # one program a report, as a study runs them
  ard <- do.call(dplyr::bind_rows, lapply(c("T-1", "T-2"), function(id)
    tfl_build_ard(a, dir = st, output_id = id, save = FALSE, codelists = t)))
  # the code list's labels and order are the ARD's
  sex <- ard[ard$output_id == "T-1" & ard$variable == "SEX", ]
  expect_identical(levels(unlist(sex$variable_level)), c("Male", "Female"))
  for (id in c("T-1", "T-2")) {
    sp <- tfl_read_report_spec(f, output_id = id)
    plan <- tfl_table_plan(rtfreporter::normalize_ard(ard[ard$output_id == id, ]), sp)
    out <- file.path(st, paste0(id, ".rtf"))
    expect_no_warning(suppressMessages(
      rtfreporter::generate_rtfreport(tfl_report(sp, content = plan), out, overwrite = TRUE)))
    txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
    expect_match(txt, "(N=86)", fixed = TRUE)
    expect_false(grepl("NA", gsub("[A-Za-z]NA|NA[A-Za-z]", "", txt), fixed = TRUE), info = id)
  }
  ls <- tfl_read_listing_spec(f, output_id = "L-1")
  pages <- tfl_listing(readRDS(file.path(st, "data", "adam", "adae.rds")), ls, output_id = "L-1")
  out <- file.path(st, "L-1.rtf")
  suppressMessages(rtfreporter::generate_rtfreport(
    tfl_report(tfl_read_report_spec(f, output_id = "L-1"), content = pages), out, overwrite = TRUE))
  expect_true(file.exists(out))
  skip_if_not_installed("ggplot2")
  fd <- tfl_read_fig_design(kit_file("07_example_figure.yml"))
  e <- new.env()
  e$advs <- readRDS(file.path(st, "data", "adam", "advs.rds"))
  suppressMessages(eval(parse(text = tfl_fig_design_code(fd, save = FALSE)), envir = e))
  expect_s3_class(e$fig, "ggplot")
})

test_that("tfl_ai_review(): the example has nothing for the AI, its decisions for the person", {
  skip_if_not_installed("readxl")
  r <- tfl_ai_review(kit_file("06_example_specs.xlsx"), toc = kit_toc(), write = FALSE)
  expect_s3_class(r, "tfl_ai_review")
  expect_identical(sum(r$who == "ai"), 0L)
  expect_setequal(unique(r$rule), c("K09", "K10", "K11", "K13"))
  a <- attr(r, "assumptions")
  expect_identical(sum(r$rule == "K09"), nrow(a))
  expect_identical(dim(summary(r)), c(2L, 3L))
  # the figure: its assumptions under `assumptions:`
  rf <- tfl_ai_review(kit_file("07_example_figure.yml"), write = FALSE)
  expect_identical(sum(rf$who == "ai"), 0L)
  expect_identical(sum(rf$rule == "K09"), 2L)
})

test_that("tfl_ai_review(): a flawed workbook gives the AI its fixes and writes the two prompts", {
  skip_if_not_installed("readxl")
  d <- file.path(tempdir(), "ai-review-flawed")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "XYZ_specs.xlsx")
  wb <- openxlsx::loadWorkbook(kit_file("06_example_specs.xlsx"))
  openxlsx::renameWorksheet(wb, "listing_cols", "Listing columns")
  ch <- openxlsx::readWorkbook(wb, "col_header")
  openxlsx::writeData(wb, "col_header", "(N=86)", startCol = which(names(ch) == "text"),
                      startRow = which(ch$text == "(N={n})")[1L] + 1L)
  ti <- openxlsx::readWorkbook(wb, "titles")
  openxlsx::writeData(wb, "titles", "Demographics", startCol = which(names(ti) == "center"),
                      startRow = which(ti$output_id == "T-1" & ti$line == 2) + 1L)
  an <- openxlsx::readWorkbook(wb, "analyses")
  openxlsx::writeData(wb, "analyses", "T-9", startCol = which(names(an) == "output_id"),
                      startRow = which(an$analysis_id == "AE") + 1L)
  openxlsx::removeWorksheet(wb, "_assumptions")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  r <- NULL
  out <- utils::capture.output(r <- tfl_ai_review(f, toc = kit_toc()))
  ai <- r[r$who == "ai", ]
  expect_true(all(c("K01", "K03", "K05", "K06", "K08") %in% ai$rule))
  expect_true("Listing columns" %in% ai$sheet[ai$rule == "K01"])
  expect_true("T-9" %in% ai$output_id[ai$rule == "K03"])
  expect_match(ai$message[ai$rule == "K05"], "the TOC has \"Demographic Characteristics\"", fixed = TRUE)
  p <- attr(r, "paths")
  expect_true(all(file.exists(p)))
  rep_txt <- readLines(p[["repair"]])
  expect_match(rep_txt[1L], "Corrections for XYZ_specs.xlsx", fixed = TRUE)
  expect_true(any(grepl("(N=86)", rep_txt, fixed = TRUE)))
  q <- readLines(p[["questions"]])
  expect_identical(sum(grepl("^   Answer: $", q)), sum(r$who == "human"))
  expect_true(any(grepl("For the AI", out)))
  expect_error(tfl_ai_review(f, toc = kit_toc()), "overwrite")
})
