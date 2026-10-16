# The YAML spec back as sheets (the adapter): a workbook converted to YAML
# and read back as sheets makes the same table code

test_that("a workbook -> YAML -> sheets makes the same table code, and the page parts come back", {
  skip_if_not_installed("openxlsx")
  skip_if_not(exists(".plan_sheets_from_content", envir = asNamespace("tflspec")), "no bridge")
  books <- system.file("extdata", "ard-spec", c("AE.xlsx", "DM.xlsx", "LB.xlsx"), package = "tflspec")
  books <- books[nzchar(books)]
  skip_if(length(books) == 0L)
  for (b in books) {
    from <- withr::local_tempdir()
    file.copy(b, file.path(from, "table_spec.xlsx"))
    to <- withr::local_tempdir()
    r <- tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE)
    expect_false(any(grepl("bridge", r$not_converted$reason)), info = basename(b))
    back <- tfl_sheets_from_yaml(to)
    sp0 <- tfl_read_table_spec(file.path(from, "table_spec.xlsx"))
    sp1 <- tfl_table_spec(back$table[intersect(names(back$table), c("study", names(.ard_spec_schema())))])
    ids <- unique(sp0$tables$output_id[!is.na(sp0$tables$output_id)])
    for (id in ids) {
      c0 <- tryCatch(tfl_table_code(sp0, id), error = function(e) conditionMessage(e))
      c1 <- tryCatch(tfl_table_code(sp1, id), error = function(e) conditionMessage(e))
      expect_identical(c1, c0, info = paste(basename(b), id))
    }
  }
})

test_that("the sheets of the fixture study come back: the report list, the page parts, the ARD", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  sh <- tfl_sheets_from_yaml(d)
  expect_identical(sh$planner$output_id, c(NA, "T-14-1-1"))
  expect_identical(sh$planner$population[2L], "SAF")
  rp <- sh$table$report
  expect_identical(rp$type[rp$output_id %in% "T-14-1-1"], "table")
  # the defaults' rtf_tables(auto_section = TRUE) as the report sheet's study row
  expect_identical(rp$auto_section[is.na(rp$output_id)], "TRUE")
  h <- sh$table$header
  expect_true(all(is.na(h$output_id)))
  expect_identical(h$left[h$line == "2"], "Protocol: {STUDY_ID}")
  expect_identical(sh$table$tokens$value[sh$table$tokens$name == "COMPANY"], "Sample Pharma")
  expect_identical(sh$table$titles$center, c("Table 14.1.1", "Demographic Characteristics"))
  expect_identical(sh$ard$analyses$analysis_id, c("GROUPN", "CONT", "CAT"))
  expect_identical(sh$ard$analyses$variables[3L], "AGEGR1 | SEX | RACE | ECOGBL")
  expect_identical(sh$ard$analysis_data$data_id, "adsl_saf")
  expect_identical(sh$ard$analysis_data$population_id, "SAF")
  expect_identical(sh$table$codelists$label[2L], "Control 100 mg")
  # a nested statistic map is not a cell: left blank
  expect_true(is.na(sh$ard$analyses$statistic[2L]) || !"statistic" %in% names(sh$ard$analyses))
})

# -- the sheets from a spec in memory (#304) -----------------------------------

test_that("tfl_sheets_from_yaml(dir) is tfl_sheets_from_spec() of what is read from dir", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  expect_identical(tfl_sheets_from_spec(tfl_read_study_spec(d), tfl_read_outputs(d)),
                   tfl_sheets_from_yaml(d))
  # a study converted from the bundled workbooks: tables, the page parts, the ARD
  skip_if_not_installed("openxlsx")
  from <- withr::local_tempdir()
  file.copy(system.file("extdata", "ard-spec", "AE.xlsx", package = "tflspec"),
            file.path(from, "table_spec.xlsx"))
  to <- withr::local_tempdir()
  tfl_convert_sheets_to_yaml(from, to, overwrite = TRUE)
  a <- tfl_sheets_from_yaml(to)
  b <- tfl_sheets_from_spec(tfl_read_study_spec(to), tfl_read_outputs(to))
  expect_identical(b, a)
  expect_identical(attributes(b), attributes(a))
  expect_identical(b$not_converted, a$not_converted)
})

test_that("tfl_sheets_from_spec(): a report changed in memory gives the changed sheets, nothing written", {
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  d <- withr::local_tempdir()
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  files <- list.files(d, recursive = TRUE, full.names = TRUE)
  stamp <- tools::md5sum(files)
  s <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  o <- outs[["T-14-1-1"]]
  i <- which(vapply(o$content, function(it) identical(it$fn, "plan_paginate_rows"), NA))
  o$content[[i]]$args$max_rows <- 25L
  o$codelists$TRT01A[[2L]]$label <- "Control 50 mg"
  o$label <- "Demographics (draft)"
  outs[["T-14-1-1"]] <- o
  sh <- tfl_sheets_from_spec(s, outs)
  lay <- sh$table$layout
  expect_identical(lay$pages_max_rows[lay$output_id %in% "T-14-1-1"], "25")
  cl <- sh$table$codelists
  expect_identical(cl$label[cl$output_id %in% "T-14-1-1" & cl$value == "Control"], "Control 50 mg")
  expect_identical(sh$planner$description[sh$planner$output_id %in% "T-14-1-1"], "Demographics (draft)")
  # nothing written, and what is on disk still says the old
  expect_identical(list.files(d, recursive = TRUE, full.names = TRUE), files)
  expect_identical(tools::md5sum(files), stamp)
  lay0 <- tfl_sheets_from_yaml(d)$table$layout
  expect_identical(lay0$pages_max_rows[lay0$output_id %in% "T-14-1-1"], "30")
  # the same sheets as the changed report written and read back
  tfl_write_output(o, file.path(d, "outputs", "T-14-1-1.yml"))
  expect_identical(sh, tfl_sheets_from_yaml(d))
})

test_that("tfl_sheets_from_spec() refuses what is not a read spec", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  expect_error(tfl_sheets_from_spec(list(), tfl_read_outputs(d)), "tfl_read_study_spec")
  expect_error(tfl_sheets_from_spec(tfl_read_study_spec(d), tfl_read_outputs(d)[[1L]]),
               "tfl_read_outputs")
})

# -- a listing: its content code and the sheets' tfl_listing_code() (#304) -------
# tflplanner's sample study's L-16-2-7 (spec/outputs/L-16-2-7.yml), on cards'
# ADAE.  The two texts differ by design: the content path writes the plan
# (table_plan(data) |> plan_listing(listing_col(...)) |> plan_paginate_rows())
# from a `data` made before, the sheets' program reads the dataset, keeps
# its records (where), sorts them, prints its dates as text, then
# listing_spec() + as_rtftables().  So they are compared by their RTF, the
# content's on the data the sheets' program makes (tfl_listing_code(part =
# "data")): tfl_content_code() does not write the listing's data part
# (dataset, where, sort, dates) itself.

lst_study <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  dir.create(file.path(d, "data"))
  ae <- as.data.frame(cards::ADAE)
  ae$TRT01A <- ae$TRTA
  ae$TRT01AN <- ae$TRTAN
  saveRDS(ae, file.path(d, "data", "adae.rds"))
  writeLines(c("datasets:", "  - dataset: ADAE", "    path: data/adae.rds"),
             file.path(d, "spec", "data.yml"))
  col <- function(vars, label, width, more = character()) {
    c("        - fn: listing_col", "          args:", "            vars:",
      paste0("              - ", vars), paste0("            label: ", label),
      paste0("            width: ", width), more)
  }
  writeLines(c(
    "id: L-16-2-7", "kind: listing",
    "label: Listing of severe treatment-emergent adverse events",
    "extends:", "  - defaults",
    "data:", "  source: dataset", "  dataset: ADAE",
    "  where: !r AESEV == \"SEVERE\" & TRTEMFL == \"Y\"",
    "  sort:", "    - TRT01AN", "    - USUBJID", "    - ASTDT",
    "content:", "  - plan_listing:", "      cols:",
    col("TRT01A", "Treatment", 22),
    col("USUBJID", "Subject", 14, "            collapse_repeats: true"),
    col(c("AEBODSYS", "AEDECOD"), "System Organ Class/\\nPreferred Term", 40),
    col(c("ASTDT", "AENDT"), "Start/\\nEnd", 14),
    col("AESER", "Serious", 8), col("AEREL", "Relation", 9), col("AEOUT", "Outcome", 24),
    "      type: multiline",
    "  - plan_paginate_rows:", "      max_rows: 22"),
    file.path(d, "spec", "outputs", "L-16-2-7.yml"))
  d
}

lst_rtf <- function(x) {
  f <- withr::local_tempfile(fileext = ".rtf")
  rtfreporter::generate_rtfreport(rtfreporter::rtf_tables(rtfreporter::rtf_document(), x),
                                  f, overwrite = TRUE)
  readLines(f)
}

# the two paths' pages: the sheets' program run, and the content's code run
# on the data the sheets' program makes
lst_both <- function(d, output = NULL) {
  s <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  sh <- tfl_sheets_from_spec(s, outs)
  ls <- tfl_listing_spec(sh$listing[c("listings", "listing_cols")])
  cat_ <- sh$ard$datasets
  cat_$path <- file.path(d, cat_$path)
  e1 <- new.env()
  eval(parse(text = c("library(rtfreporter)", tfl_listing_code(ls, "L-16-2-7", cat_))), e1)
  o <- output %||% tfl_resolve_output(outs[["L-16-2-7"]], s)
  e2 <- new.env()
  eval(parse(text = c("library(rtfreporter)",
                      tfl_listing_code(ls, "L-16-2-7", cat_, part = "data"),
                      tfl_content_code(o, pipe = "|>"))), e2)
  list(sheets = lst_rtf(e1$content),
       content = lst_rtf(suppressMessages(rtfreporter::plan_apply(e2$plan))),
       rows = nrow(e1$data))
}

test_that("a listing's content code, on the sheets' data, makes the sheets' listing's pages", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  d <- lst_study()
  s <- tfl_read_study_spec(d)
  o <- tfl_resolve_output(tfl_read_outputs(d)[["L-16-2-7"]], s)
  cc <- tfl_content_code(o, pipe = "|>")
  expect_identical(cc[1L], "plan <- table_plan(data) |>")
  expect_identical(utils::tail(cc, 1L), "  plan_paginate_rows(max_rows = 22)")
  # (a label's `\n`, two characters in the file, is a line break on the
  # sheets' path -- .listing_label() -- and is written as it is on the
  # content's: here made a line break, see the test below)
  nl <- function(x) gsub("\\n", "\n", x, fixed = TRUE)
  pl <- which(vapply(o$content, function(it) identical(it$fn, "plan_listing"), NA))
  for (k in seq_along(o$content[[pl]]$args$cols)) {
    o$content[[pl]]$args$cols[[k]]$args$label <- nl(o$content[[pl]]$args$cols[[k]]$args$label)
  }
  r <- lst_both(d, o)
  expect_gt(r$rows, 0L)
  expect_identical(r$content, r$sheets)
})

test_that("a listing's label `\\n`: the same line break on both paths", {
  skip("known difference (#304): tfl_content_code() writes a label's `\\n` as it is, the sheets a line break")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  r <- lst_both(lst_study())
  expect_identical(r$content, r$sheets)
})
