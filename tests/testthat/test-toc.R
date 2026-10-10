# tfl_read_toc(): a company's TOC through a map of its columns

toc_csv <- function(lines) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(lines, f, useBytes = TRUE)
  f
}

test_that("a TOC in one layout: kinds, titles, the population, footnotes, a heading passed over", {
  f <- toc_csv(c(
    "No.,Kind,Title,Population,Footnotes,Program",
    ",14.1 Demographics,,,,",
    "T-14-1-1,Table,Demographic Characteristics,Safety Population,N: subjects | %: of N,t_dm",
    "F-14-2-1,Fig,Mean Systolic Blood Pressure | Mean (SE) by visit,Safety Population,,f_sbp",
    "L-16-2-7,,Listing of Serious Adverse Events,Safety Population,,l_sae"))
  sp <- tfl_read_toc(f, map = c(output_id = "No.", type = "Kind", title = "Title",
                                population = "Population", footnote = "Footnotes",
                                program = "Program"))
  expect_s3_class(sp, "tfl_table_spec")
  expect_identical(sp$report$output_id, c("T-14-1-1", "F-14-2-1", "L-16-2-7"))
  expect_identical(sp$report$type, c("table", "figure", "listing"))
  expect_identical(sp$report$program, c("t_dm", "f_sbp", "l_sae"))
  # " | " splits a cell into lines; the population is the last title line
  t2 <- sp$titles[sp$titles$output_id == "F-14-2-1", ]
  expect_identical(t2$center, c("Mean Systolic Blood Pressure", "Mean (SE) by visit",
                                "Safety Population"))
  expect_identical(t2$line, c("1", "2", "3"))
  expect_identical(sp$footnotes$left[sp$footnotes$output_id == "T-14-1-1"],
                   c("N: subjects", "%: of N"))
  # the listing's kind came from its id; the heading row was passed over
  expect_identical(attr(sp, "guessed"), "L-16-2-7")
  expect_identical(nrow(attr(sp, "skipped")), 1L)
})

test_that("another layout: several title and footnote columns, odd names, an xlsx with rows above", {
  skip_if_not_installed("writexl")
  d <- data.frame(
    `Output Number` = c("Table 14.3.1", "Figure 14.3.2", "Listing 16.2.1"),
    `Title Line 1` = c("Overview of Adverse Events", "Kaplan-Meier Plot", "Deaths"),
    `Title Line 2` = c("Safety Analysis Set", NA, "All Randomised"),
    `Footnote 1` = c("TEAE: treatment-emergent", NA, NA),
    `Footnote 2` = c("MedDRA 26.0", NA, NA),
    check.names = FALSE, stringsAsFactors = FALSE)
  f <- withr::local_tempfile(fileext = ".xlsx")
  # a banner row above the header row
  sheet <- rbind(c("STUDY XYZ", rep(NA, 4)), names(d), as.matrix(d))
  writexl::write_xlsx(list(TOC = as.data.frame(sheet, stringsAsFactors = FALSE)),
                      f, col_names = FALSE)
  sp <- tfl_read_toc(f, sheet = "TOC", skip = 1L,
                     map = list(output_id = "output number",
                                title = c("Title Line 1", "Title Line 2"),
                                footnote = c("Footnote 1", "Footnote 2")))
  expect_identical(sp$report$type, c("table", "figure", "listing"))
  expect_identical(sp$titles$center[sp$titles$output_id == "Table 14.3.1"],
                   c("Overview of Adverse Events", "Safety Analysis Set"))
  expect_identical(sp$footnotes$left, c("TEAE: treatment-emergent", "MedDRA 26.0"))
  expect_identical(attr(sp, "guessed"), sp$report$output_id)
})

test_that("a third layout: one cell with line breaks, kinds from the title", {
  f <- toc_csv(c("ID,Titles,Notes",
                 "\"14.1.1\",\"Table 14.1.1\nSummary of Demographics\",\"Source: ADSL\"",
                 "\"14.2.1\",\"Figure 14.2.1\nForest Plot\","))
  sp <- tfl_read_toc(f, map = c(output_id = "ID", title = "Titles", footnote = "Notes"))
  expect_identical(sp$report$type, c("table", "figure"))
  expect_identical(sp$titles$center[sp$titles$output_id == "14.1.1"],
                   c("Table 14.1.1", "Summary of Demographics"))
})

test_that("what does not fit says so", {
  f <- toc_csv(c("No.,Title", "T1,A", "T1,B"))
  expect_error(tfl_read_toc(f, map = c(output_id = "Number")),
               "no column 'Number'.*Closest: 'No.'")
  expect_error(tfl_read_toc(f, map = c(output_id = "No.", title = "Title")),
               "given twice: 'T1'")
  g <- toc_csv(c("No.,Title,Pop", ",A title,Safety"))
  expect_error(tfl_read_toc(g, map = c(output_id = "No.", title = "Title",
                                       population = "Pop")),
               "row\\(s\\) 2 have no output id")
  expect_error(tfl_read_toc(f, map = c(title = "Title")), "which column is the `output_id`")
  expect_error(tfl_read_toc(f, map = c(output_id = "No.", heading = "Title")),
               "no field 'heading'")
})

test_that("the TOC's specs are written and read back", {
  f <- toc_csv(c("No.,Kind,Title,Footnotes",
                 "T-14-1-1,Table,Demographics,N: subjects",
                 "F-14-2-1,Figure,SBP,"))
  sp <- tfl_read_toc(f, map = c(output_id = "No.", type = "Kind", title = "Title",
                                footnote = "Footnotes"))
  x <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_specs(x, report = sp)
  back <- tfl_read_report_spec(x, output_id = "F-14-2-1")
  expect_identical(back$report$type[back$report$output_id %in% "F-14-2-1"], "figure")
  expect_identical(back$titles$center, "SBP")
})

test_that("the synthetic TOC of ydisctools' SAP pipeline reads", {
  f <- "C:/Yrepo/Rrepo/ydisctools/inst/sap-pipeline/01_toc/TOC_STUDY01.xlsx"
  skip_if_not(file.exists(f), "ydisctools' sample TOC not here")
  sp <- tfl_read_toc(f, sheet = "TOC",
                     map = c(output_id = "output_id", title = "title",
                             note = "display_type"))
  expect_identical(nrow(sp$report), 10L)
  expect_true(all(sp$report$type == "table"))
  expect_true(all(sp$report$output_id %in% sp$titles$output_id))
})

test_that("each report's section: the heading row above it, or its own column", {
  f <- toc_csv(c(
    "No.,Title",
    ",14.1 Demographics",
    "T-14-1-1,Demographic Characteristics",
    "T-14-1-2,Disposition",
    ",14.3 Adverse Events",
    "T-14-3-1,Overview of TEAEs"))
  sp <- tfl_read_toc(f, map = c(output_id = "No.", title = "Title"))
  expect_identical(attr(sp, "sections"),
                   c(`T-14-1-1` = "14.1 Demographics", `T-14-1-2` = "14.1 Demographics",
                     `T-14-3-1` = "14.3 Adverse Events"))
  # no heading above: NA
  f2 <- toc_csv(c("No.,Title", "T-1,One", ",2 Later", "T-2,Two"))
  expect_identical(unname(attr(tfl_read_toc(f2, map = c(output_id = "No.", title = "Title")),
                               "sections")), c(NA, "2 Later"))
  # a section column: its value, over a heading row's
  f3 <- toc_csv(c("No.,Title,Section",
                  ",Heading row,",
                  "T-1,One,14.1 Demographics",
                  "T-2,Two,"))
  sp3 <- tfl_read_toc(f3, map = c(output_id = "No.", title = "Title", section = "Section"))
  expect_identical(unname(attr(sp3, "sections")), c("14.1 Demographics", "Heading row"))
  # not part of the spec
  expect_false("section" %in% names(sp3$report))
})

test_that("a TOC's label, first title and analysis set, for the report's own tokens", {
  f <- toc_csv(c("No.,Label,Title,Population",
                 "T-14-1-1,Table 14.1.1,Demographics | Age and sex,Safety Analysis Set",
                 "T-14-1-2,,Disposition,"))
  sp <- tfl_read_toc(f, map = c(output_id = "No.", label = "Label", title = "Title",
                                population = "Population"))
  expect_identical(unname(attr(sp, "labels")), c("Table 14.1.1", NA))
  expect_identical(unname(attr(sp, "first_titles")), c("Demographics", "Disposition"))
  expect_identical(unname(attr(sp, "populations")), c("Safety Analysis Set", NA))
  expect_identical(names(attr(sp, "labels")), c("T-14-1-1", "T-14-1-2"))
})

test_that("each report's datasets: its datasets column, as one value", {
  f <- toc_csv(c("No.,Title,Data",
                 "T-1,One,\"ADSL, ADAE\"",
                 "T-2,Two,ADSL / ADVS",
                 "T-3,Three,"))
  sp <- tfl_read_toc(f, map = c(output_id = "No.", title = "Title", datasets = "Data"))
  expect_identical(attr(sp, "datasets"),
                   c(`T-1` = "ADSL | ADAE", `T-2` = "ADSL | ADVS", `T-3` = NA))
  expect_false("datasets" %in% names(sp$report))
  # none mapped: NA for each
  sp2 <- tfl_read_toc(f, map = c(output_id = "No.", title = "Title"))
  expect_true(all(is.na(attr(sp2, "datasets"))))
  expect_identical(.toc_datasets(c("ADSL;ADAE", "ADSL ADSL", NA)), c("ADSL | ADAE", "ADSL", NA))
  expect_error(tfl_read_toc(f, map = list(output_id = "No.", datasets = c("Data", "Title"))),
               "one column")
})

test_that("a data frame in place of a file reads as the file of the same rows", {
  f <- toc_csv(c(
    "No.,Kind,Title,Population",
    ",14.1 Demographics,,",
    "T-14-1-1,Table,Demographic Characteristics,Safety Population",
    ",,,",
    "F-14-2-1,, Mean SBP | Mean (SE) ,Safety Population"))
  map <- c(output_id = "No.", type = "Kind", title = "Title",
           population = "Population")
  from_file <- tfl_read_toc(f, map = map)
  d <- utils::read.csv(f, colClasses = "character", check.names = FALSE)
  # sheet and skip do not apply to a data frame
  from_df <- tfl_read_toc(d, map = map, sheet = "ignored", skip = 3L)
  expect_identical(from_df$report, from_file$report)
  expect_identical(from_df$titles, from_file$titles)
  expect_identical(attr(from_df, "sections"), attr(from_file, "sections"))
  expect_identical(attr(from_df, "guessed"), attr(from_file, "guessed"))
  # a tibble too, and the closest-name error still names its columns
  if (requireNamespace("tibble", quietly = TRUE)) {
    expect_identical(tfl_read_toc(tibble::as_tibble(d), map = map)$report,
                     from_file$report)
  }
  expect_error(tfl_read_toc(d, map = c(output_id = "Number")), "Closest")
  # a row with no id but other cells is named by its data frame row
  g <- data.frame(No. = c("T1", NA), Title = c("A", "B"), Pop = c(NA, "SAF"),
                  check.names = FALSE)
  expect_error(tfl_read_toc(g, map = c(output_id = "No.", title = "Title",
                                       population = "Pop")),
               "row\\(s\\) 2 have no output id")
})
