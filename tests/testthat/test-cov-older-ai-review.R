# tfl_ai_review() (R/ai_review.R): the branches no other test reaches -- a
# spec workbook reviewed on its own (not through a study folder), the
# figure design and report .yml paths' errors, the metadata checks' edge
# rows, the assumptions in each form, and the print and summary methods.
# Fictional data only.

# The kit's example answer, as one spec workbook: built once per session,
# each test then edits a copy of its sheets.
cov_airv_base <- function() {
  d <- file.path(tempdir(), "cov-older-ai-review")
  f <- file.path(d, "base.xlsx")
  if (!file.exists(f)) {
    unlink(d, recursive = TRUE)
    withr::defer(unlink(d, recursive = TRUE), envir = testthat::teardown_env())
    kit <- suppressMessages(tflspec_ai_manual(file.path(d, "kit")))
    st <- file.path(d, "study")
    tfl_ai_unpack(kit[basename(kit) == "06_example_spec.md"], st)
    b <- suppressMessages(tfl_sheets_from_yaml(st, templates = "expand"))
    tfl_write_specs(f, ard = tfl_ard_spec(b$ard), table = tfl_table_spec(b$table),
                    report = tfl_table_spec(b$table), listing = tfl_listing_spec(b$listing))
  }
  f
}

cov_airv_sheets <- function() tflspec:::.ai_read_sheets(cov_airv_base())

# the sheets written to a workbook of their own, removed when the caller ends
cov_airv_book <- function(sheets, env = parent.frame()) {
  f <- tempfile("cov-airv", fileext = ".xlsx")
  do.call(on.exit, list(substitute(unlink(f)), add = TRUE), envir = env)
  tflspec:::.ai_write_sheets(sheets, f)
  f
}

cov_airv_toc <- function() {
  list(path = file.path(tempdir(), "cov-older-ai-review", "kit", "05_example_toc_shell.xlsx"),
       sheet = "TOC", skip = 1L,
       map = list(output_id = "Shell Number", title = c("Title1", "Title2"),
                  population = "Population", footnote = c("Footnote1", "Footnote2")))
}

cov_airv_yml <- function(lines, env = parent.frame()) {
  f <- tempfile("cov-airv", fileext = ".yml")
  do.call(on.exit, list(substitute(unlink(f)), add = TRUE), envir = env)
  writeLines(lines, f)
  f
}

# ---- tfl_ai_review()'s arguments ----------------------------------------------

test_that("a file that does not exist stops with what `file` takes", {
  expect_error(tfl_ai_review(file.path(tempdir(), "no-such-spec.xlsx"), write = FALSE),
               "one spec file that exists")
  expect_error(tfl_ai_review(c("a.xlsx", "b.xlsx"), write = FALSE), "one spec file")
  expect_error(tfl_ai_review(NA_character_, write = FALSE), "one spec file")
})

test_that("a file that is neither a workbook nor a .yml stops", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines("a,b", f)
  expect_error(tfl_ai_review(f, write = FALSE), "a workbook [(][.]xlsx[)]")
})

test_that("metadata without a levels column is read with no levels", {
  md <- data.frame(dataset = " adsl ", variable = " AGE ", stringsAsFactors = FALSE)
  x <- tflspec:::.ai_review_md(md)
  expect_identical(x$dataset, "ADSL")
  expect_identical(x$variable, "AGE")
  expect_identical(x$levels, NA_character_)
  expect_null(tflspec:::.ai_review_md(NULL))
})

test_that("`toc` that is neither a TOC nor a list with a path stops", {
  expect_error(tflspec:::.ai_toc("toc.xlsx"), "tfl_read_toc[(][)] result")
  expect_error(tflspec:::.ai_toc(list(sheet = "TOC")), "tfl_read_toc[(][)] result")
})

test_that(".ai_lines() of no titles is no lines", {
  expect_identical(tflspec:::.ai_lines(NULL, "T-1"), character())
  expect_identical(tflspec:::.ai_lines(data.frame(output_id = character()), "T-1"), character())
})

# ---- a workbook on its own -----------------------------------------------------

test_that("a workbook as it was written: its kinds, and K08 for no _assumptions sheet", {
  skip_on_cran()
  r <- tfl_ai_review(cov_airv_base(), write = FALSE)
  expect_s3_class(r, "tfl_ai_review")
  expect_setequal(attr(r, "kinds"), c("ard", "table", "report", "listing"))
  expect_null(attr(r, "assumptions"))
  expect_true(any(r$rule == "K08" & grepl("no sheet _assumptions", r$message, fixed = TRUE)))
})

test_that("a sheet that is not the spec's is K01, a column that is not is K02", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  sh$Notes <- data.frame(text = "see the SAP", stringsAsFactors = FALSE)
  sh$tables$comment <- "fictional"
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  k01 <- r[r$rule == "K01", ]
  expect_identical(k01$sheet, "Notes")
  expect_identical(k01$who, "ai")
  k02 <- r[r$rule == "K02", ]
  expect_identical(k02$sheet, "tables")
  expect_identical(k02$field, "comment")
})

test_that("an _assumptions sheet: each row K09 for the person, its question appended", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  sh[["_assumptions"]] <- data.frame(
    output_id = c("T-1", NA), sheet = c("analyses", "populations"), item = c("1", "2"),
    assumption = c("Age is summarised as continuous", "Safety set is SAFFL = Y"),
    question = c("Also by age group?", NA), stringsAsFactors = FALSE)
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  k09 <- r[r$rule == "K09", ]
  expect_identical(nrow(k09), 2L)
  expect_true(all(k09$who == "human"))
  expect_true(any(grepl("Also by age group?", k09$message, fixed = TRUE)))
  expect_false(any(r$rule == "K08"))
  a <- attr(r, "assumptions")
  expect_identical(a$output_id, c("T-1", NA))
  expect_identical(a$question, c("Also by age group?", ""))
})

test_that("an _assumptions sheet with only its header is K08 'is empty'", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  cols <- c("output_id", "sheet", "item", "assumption", "question")
  sh[["_assumptions"]] <- stats::setNames(as.data.frame(matrix(character(), 0L, 5L),
                                                        stringsAsFactors = FALSE), cols)
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  expect_true(any(r$rule == "K08" & grepl("is empty", r$message, fixed = TRUE)))
})

test_that("an _assumptions sheet with no rows and not every column is K08 'is empty'", {
  a <- tflspec:::.ai_review_assumptions(list(`_assumptions` = data.frame(
    assumption = character(), stringsAsFactors = FALSE)))
  expect_identical(nrow(a), 0L)
  sh <- cov_airv_sheets()
  sh[["_assumptions"]] <- data.frame(assumption = character(), stringsAsFactors = FALSE)
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  expect_true(any(r$rule == "K08" & grepl("is empty", r$message, fixed = TRUE)))
})

test_that("an _assumptions sheet missing columns gets them empty", {
  a <- tflspec:::.ai_review_assumptions(list(`_Assumptions` = data.frame(
    assumption = "No imputation", stringsAsFactors = FALSE)))
  expect_identical(names(a), c("output_id", "sheet", "item", "assumption", "question"))
  expect_identical(a$output_id, NA_character_)
  expect_identical(a$sheet, "")
})

test_that("a reader's error is S01 for the AI and does not stop the other kinds", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  sh$listing_cols <- NULL
  sh$listings$output_id <- NULL
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  s01 <- r[r$rule == "S01", ]
  expect_true(any(grepl("^The listing spec: ", s01$message)))
  expect_true(any(r$rule == "K10"))
})

test_that("the TOC as a list of tfl_read_toc()'s arguments: K04 an output the report lacks, K05 a changed title", {
  skip_on_cran()
  base <- cov_airv_base()
  toc <- cov_airv_toc()
  sh <- cov_airv_sheets()
  drop <- sh$report$output_id[1L]
  sh$report <- sh$report[sh$report$output_id != drop, , drop = FALSE]
  keep <- sh$report$output_id[1L]
  i <- which(sh$titles$output_id == keep)[1L]
  col <- intersect(c("text", "center", "left"), names(sh$titles))[1L]
  sh$titles[[col]][i] <- "A title the TOC does not have"
  r <- tfl_ai_review(cov_airv_book(sh), toc = toc, write = FALSE)
  expect_true(any(r$rule == "K04" & r$output_id == drop))
  k05 <- r[r$rule == "K05" & r$output_id == keep, ]
  expect_true(nrow(k05) >= 1L)
  expect_true(any(grepl("A title the TOC does not have", k05$message, fixed = TRUE)))
  expect_true(file.exists(base))
})

test_that("a report with more title lines than the TOC: each extra line is K05 against nothing", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  keep <- sh$report$output_id[1L]
  i <- which(sh$titles$output_id == keep)[1L]
  col <- intersect(c("text", "center", "left"), names(sh$titles))[1L]
  extra <- sh$titles[i, , drop = FALSE]
  extra$line <- "9"
  extra[[col]] <- "A line only the report has"
  sh$titles <- rbind(sh$titles, extra)
  r <- tfl_ai_review(cov_airv_book(sh), toc = cov_airv_toc(), write = FALSE)
  k05 <- r[r$rule == "K05" & r$output_id == keep, ]
  expect_true(any(grepl("A line only the report has", k05$message, fixed = TRUE)))
})

test_that("a report with fewer title lines than the TOC: each missing line is K05", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  keep <- sh$report$output_id[1L]
  sh$titles <- sh$titles[sh$titles$output_id != keep, , drop = FALSE]
  r <- tfl_ai_review(cov_airv_book(sh), toc = cov_airv_toc(), write = FALSE)
  expect_true(any(r$rule == "K05" & r$output_id == keep))
})

test_that("a number where a token belongs is K06", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  col <- intersect(c("text", "center", "left"), names(sh$col_header))[1L]
  sh$col_header[[col]][1L] <- "Placebo (N = 86)"
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  k06 <- r[r$rule == "K06", ]
  expect_identical(nrow(k06), 1L)
  expect_identical(k06$sheet, "col_header")
  expect_match(k06$message, "(N = 86)", fixed = TRUE)
})

test_that("an analysis no table uses is K07, unless the report says it is a figure's", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  an <- sh$analyses
  extra <- an[c(1L, 1L), , drop = FALSE]
  extra$output_id <- c("T-99", "F-99")
  extra$analysis_id <- c("A-T99", "A-F99")
  sh$analyses <- rbind(an, extra)
  rp <- sh$report[1L, , drop = FALSE]
  rp$output_id <- "F-99"
  rp$type <- "figure"
  sh$report <- rbind(sh$report, rp)
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  expect_true(any(r$rule == "K07" & r$output_id == "T-99"))
  expect_false(any(r$rule == "K07" & r$output_id == "F-99"))
})

test_that("a workbook of the ARD and the tables, no report: an analysis no table uses is K07", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  rep_sheets <- c("report", "page", "header", "footer", "titles", "footnotes", "tokens")
  sh <- sh[!names(sh) %in% rep_sheets]
  extra <- sh$analyses[1L, , drop = FALSE]
  extra$output_id <- "T-99"
  extra$analysis_id <- "A-T99"
  sh$analyses <- rbind(sh$analyses, extra)
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  expect_false("report" %in% attr(r, "kinds"))
  expect_true(any(r$rule == "K07" & r$output_id == "T-99"))
})

test_that("K10 names a written denominator, K11 says no condition, K12 asks about a test", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  i <- which(sh$analyses$method %in% c("categorical", "dichotomous", "subjects"))[1L]
  sh$analyses$denominator[i] <- "population"
  j <- which(sh$analyses$method == "continuous")[1L]
  sh$analyses$method[j] <- "ttest"
  sh$populations$where[1L] <- NA
  r <- tfl_ai_review(cov_airv_book(sh), write = FALSE)
  expect_false(any(r$rule == "S01"))
  expect_true(any(r$rule == "K10" & grepl("population", r$message, fixed = TRUE)))
  expect_true(any(r$rule == "K11" & grepl("no condition", r$message, fixed = TRUE)))
  expect_true(any(r$rule == "K12" & grepl("ttest", r$message, fixed = TRUE)))
})

test_that("with the data given, the review reads its facts", {
  skip_on_cran()
  adam <- list(adsl = tflspec::adsl, adae = tflspec::adae, advs = tflspec::advs)
  r <- tfl_ai_review(cov_airv_base(), adam = adam, write = FALSE)
  expect_s3_class(r, "tfl_ai_review")
  expect_setequal(attr(r, "kinds"), c("ard", "table", "report", "listing"))
})

# ---- the metadata checks (K14 / K15) ---------------------------------------------

cov_airv_md_check <- function(sheets, md) {
  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  tflspec:::.ai_review_metadata(sheets, tflspec:::.ai_review_md(md), add, tflspec:::.ai_cat())
  if (length(rows)) do.call(rbind, rows) else NULL
}

cov_airv_md <- function() {
  data.frame(dataset = c("ADSL", "ADSL", "ADSL"), variable = c("AGE", "SEX", "TRT01A"),
             levels = c(NA, "F|M", NA), stringsAsFactors = FALSE)
}

test_that("a dataset the metadata does not have is K14 with the nearest names", {
  r <- cov_airv_md_check(list(datasets = data.frame(output_id = "T-1", dataset = "adsx",
                                                    stringsAsFactors = FALSE)),
                         cov_airv_md())
  expect_identical(r$rule, "K14")
  expect_identical(r$field, "dataset")
  expect_match(r$message, "ADSX", fixed = TRUE)
  expect_match(r$hint, "ADSL", fixed = TRUE)
})

test_that("a dataset sheet with no dataset column is passed over", {
  r <- cov_airv_md_check(list(Populations = data.frame(population_id = "SAF",
                                                       stringsAsFactors = FALSE)),
                         cov_airv_md())
  expect_null(r)
})

test_that("an analysis in an unknown dataset checks its variables against every dataset's", {
  an <- data.frame(output_id = "T-1", analysis_id = "A1", dataset = "ADXX",
                   variables = "AGE|AGEX", stringsAsFactors = FALSE)
  r <- cov_airv_md_check(list(analyses = an), cov_airv_md())
  expect_identical(sort(r$field), c("dataset", "variables"))
  expect_match(r$message[r$field == "variables"], "AGEX", fixed = TRUE)
})

test_that("a variables sheet's unknown variable is K14", {
  vs <- data.frame(output_id = "T-1", variable = c("AGE", "WEIGHTX", NA), stringsAsFactors = FALSE)
  r <- cov_airv_md_check(list(variables = vs), cov_airv_md())
  expect_identical(nrow(r), 1L)
  expect_identical(r$sheet, "variables")
  expect_match(r$message, "WEIGHTX", fixed = TRUE)
})

test_that("a code list of a variable with no levels is not checked; one not among the levels is K15", {
  cl <- data.frame(output_id = "T-1", variable = c("AGE", "SEX", "SEX"),
                   value = c("65+", "F", "Unknown"), stringsAsFactors = FALSE)
  r <- cov_airv_md_check(list(codelists = cl), cov_airv_md())
  expect_identical(r$rule, "K15")
  expect_match(r$message, "Unknown", fixed = TRUE)
  expect_match(r$hint, "F | M", fixed = TRUE)
})

test_that("a workbook with metadata: tfl_ai_review() reads the kit's CSV and checks the names", {
  skip_on_cran()
  sh <- cov_airv_sheets()
  sh$datasets$dataset[1L] <- "ADZZ"
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  md <- tfl_adam_metadata(datasets = list(ADSL = tflspec::adsl, ADAE = tflspec::adae,
                                          ADVS = tflspec::advs))
  utils::write.csv(md, csv, row.names = FALSE, na = "")
  r <- tfl_ai_review(cov_airv_book(sh), metadata = csv, write = FALSE)
  expect_true(any(r$rule == "K14" & grepl("ADZZ", r$message, fixed = TRUE)))
})

# ---- a figure design .yml ----------------------------------------------------------

test_that("a .yml that is not YAML is S01 on the design", {
  f <- cov_airv_yml(c("type: km", "args: [param: OS"))
  r <- tfl_ai_review(f, write = FALSE)
  expect_identical(attr(r, "kinds"), "figure")
  expect_true(any(r$rule == "S01" & r$sheet == "design"))
  expect_null(attr(r, "assumptions"))
})

test_that("a figure design the reader refuses is S01, and no assumptions: K08", {
  f <- cov_airv_yml("ggplot2_version: '9.9'")
  r <- tfl_ai_review(f, write = FALSE)
  expect_true(any(r$rule == "S01" & r$sheet == "design"))
  expect_true(any(r$rule == "K08" & grepl("no assumptions:", r$message, fixed = TRUE)))
})

test_that("a figure design's assumptions are K09 each, for the person", {
  f <- cov_airv_yml(c("type: km", "style: simple", "args:", "  param: OS",
                      "assumptions:", "  - OS is the time to death",
                      "  - Censored at the last contact"))
  r <- tfl_ai_review(f, write = FALSE)
  a <- attr(r, "assumptions")
  expect_identical(a$assumption, c("OS is the time to death", "Censored at the last contact"))
  expect_identical(a$sheet, c("design", "design"))
  k09 <- r[r$rule == "K09", ]
  expect_identical(nrow(k09), 2L)
  expect_true(all(k09$who == "human"))
})

test_that("a figure design's problems are S01 each, with the part and field", {
  f <- cov_airv_yml(c("layers:", "  - {layer: nope, type: zzz}", "assumptions: [none]"))
  r <- tfl_ai_review(f, write = FALSE)
  s01 <- r[r$rule == "S01", ]
  expect_identical(s01$row, "layers[1] nope")
  expect_identical(s01$field, "layer")
  expect_identical(s01$who, "ai")
})

# ---- a report .yml of the YAML spec ---------------------------------------------------

test_that("a report .yml the reader refuses is S01 on the report, with no assumptions", {
  f <- cov_airv_yml(c("id: T-1", "kind: banana", "content: 3"))
  r <- tfl_ai_review(f, write = FALSE)
  expect_identical(attr(r, "kinds"), "report")
  expect_true(any(r$rule == "S01" & r$sheet == "report"))
})

test_that("what the AI marked `ai: confirm` is K19 for the person", {
  f <- cov_airv_yml(c("id: T-1", "kind: table", "extends: none",
                      "ai:", "  confirm:",
                      "    - {where: content, item: plan_cells, why: the shell shows no denominator}",
                      "    - a bare reason",
                      "content:", "  - table_plan: {cols: ARM, rows: {group: variable}}"))
  r <- tfl_ai_review(f, write = FALSE)
  k19 <- r[r$rule == "K19", ]
  expect_identical(nrow(k19), 2L)
  expect_true(all(k19$output_id == "T-1"))
  expect_true(any(grepl("content$plan_cells", k19$message, fixed = TRUE)))
  expect_true(any(grepl("a bare reason", k19$message, fixed = TRUE)))
})

test_that("a report whose content is R code has no items to check: nothing found", {
  f <- cov_airv_yml(c("id: T-1", "kind: table", "content: !r \"list()\""))
  r <- tfl_ai_review(f, write = FALSE)
  expect_identical(nrow(r), 0L)
  expect_identical(attr(r, "kinds"), "report")
  expect_true(any(grepl("nothing for the AI to correct", attr(r, "repair"), fixed = TRUE)))
  expect_true(any(grepl("nothing to decide", attr(r, "questions"), fixed = TRUE)))
})

test_that("a figure of named plots: each plot's items are checked", {
  x <- tfl_fig_template("km_risk_table", param = "OS", group = "TRT01P", form = "items")
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tfl_write_output(tfl_output("F-1", "figure", data = x$data, content = x$content), f)
  r <- tfl_ai_review(f, write = FALSE)
  expect_identical(attr(r, "kinds"), "report")
  expect_true(all(r$output_id == "F-1"))
  expect_true(any(r$rule == "K18" & r$sheet == "content"))
})

test_that("a .yml reviewed with write = TRUE: the two prompts next to it", {
  d <- withr_tempdir()
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content: !r \"list()\""), f)
  out <- utils::capture.output(r <- tfl_ai_review(f))
  expect_identical(unname(basename(attr(r, "paths"))), c("T-1_repair.md", "T-1_questions.md"))
  expect_true(all(file.exists(file.path(d, c("T-1_repair.md", "T-1_questions.md")))))
  expect_identical(out[1L], "tfl_ai_review: T-1.yml (report)")
})

# ---- a study folder ------------------------------------------------------------------

test_that("a folder whose definition does not read is S01 on the spec, and no assumptions: K08", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  writeLines("study: [", file.path(d, "study.yml"))
  r <- tfl_ai_review(d, write = FALSE)
  expect_identical(attr(r, "kinds"), "study")
  expect_true(any(r$rule == "S01" & r$sheet == "spec"))
  expect_true(any(r$rule == "K08" & grepl("_assumptions.yml", r$message, fixed = TRUE)))
})

test_that("a folder written: the two prompts are in it, named after it", {
  skip_on_cran()
  d <- file.path(withr_tempdir(), "XYZ-COV")
  dir.create(file.path(d, "spec"), recursive = TRUE)
  writeLines("study: [", file.path(d, "study.yml"))
  out <- utils::capture.output(r <- tfl_ai_review(d))
  p <- attr(r, "paths")
  expect_identical(basename(p[["repair"]]), "XYZ-COV_repair.md")
  expect_identical(normalizePath(dirname(p[["questions"]])), normalizePath(d))
  expect_true(all(file.exists(p)))
  expect_true(any(grepl("-- XYZ-COV_repair.md", out, fixed = TRUE)))
})

# the sample study of the YAML spec (no spec/data.yml: its ARD has no data
# to read), copied to a folder of its own
cov_airv_sample <- function(env = parent.frame()) {
  d <- tempfile("cov-airv-study")
  do.call(on.exit, list(substitute(unlink(d, recursive = TRUE)), add = TRUE), envir = env)
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  d
}

test_that("a folder whose definition does not make a valid workbook is S01 on the spec", {
  skip_on_cran()
  d <- cov_airv_sample()
  r <- tfl_ai_review(d, write = FALSE)
  s01 <- r[r$rule == "S01", ]
  expect_identical(s01$sheet, "spec")
  expect_match(s01$message, "ARD definition is not valid", fixed = TRUE)
  expect_true("ard" %in% attr(r, "kinds"))
})

test_that("a folder's figure of a design (not items) is checked by the figure designer", {
  skip_on_cran()
  d <- cov_airv_sample()
  writeLines(c("id: F-9", "kind: figure", "label: A fictional figure",
               "layers:", "  - {layer: nope, type: zzz}"),
             file.path(d, "outputs", "F-9.yml"))
  r <- tfl_ai_review(d, write = FALSE)
  expect_true("figure" %in% attr(r, "kinds"))
  s01 <- r[r$rule == "S01" & r$output_id %in% "F-9", ]
  expect_identical(s01$sheet, "spec/outputs/F-9.yml")
  expect_identical(s01$field, "layer")
})

# ---- the folder's places and assumptions ---------------------------------------------

test_that("a sheet's row is placed in study.yml or spec/defaults.yml when no output owns it", {
  expect_identical(tflspec:::.ai_yaml_place("study", NA_character_), "study.yml")
  expect_identical(tflspec:::.ai_yaml_place("", ""), "study.yml")
  expect_identical(tflspec:::.ai_yaml_place("tokens", NA_character_),
                   "spec/defaults.yml: page.tokens")
  expect_identical(tflspec:::.ai_yaml_place("populations", ""), "spec/data.yml: populations")
  expect_identical(tflspec:::.ai_yaml_place("titles", "T-1"),
                   "spec/outputs/T-1.yml: page.titles")
})

test_that("spec/_assumptions.yml that is empty or a scalar is no assumptions", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  f <- file.path(d, "spec", "_assumptions.yml")
  writeLines("", f)
  expect_identical(nrow(tflspec:::.ai_folder_assumptions(d)), 0L)
  writeLines("just a sentence", f)
  expect_identical(nrow(tflspec:::.ai_folder_assumptions(d)), 0L)
})

test_that("spec/_assumptions.yml under an `assumptions:` key, a bare sentence an item", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  writeLines(c("assumptions:", "  - No imputation of missing ages",
               "  - {output_id: T-1, where: analyses, item: A1, assumption: Age in years}"),
             file.path(d, "spec", "_assumptions.yml"))
  a <- tflspec:::.ai_folder_assumptions(d)
  expect_identical(a$assumption, c("No imputation of missing ages", "Age in years"))
  expect_identical(a$output_id, c(NA, "T-1"))
})

# ---- the prompts, print and summary ----------------------------------------------------

test_that("the repair prompt lists the columns of the sheets it names", {
  r <- data.frame(rule = "K02", level = "error", output_id = NA_character_,
                  sheet = "tables", row = "", field = "comment",
                  message = "A column | not the spec's", hint = "Remove it",
                  who = "ai", stringsAsFactors = FALSE)
  txt <- tflspec:::.ai_repair_text(r, "book.xlsx")
  expect_true("## The columns of the sheets above" %in% txt)
  expect_true(any(startsWith(txt, "| tables | ")))
  expect_true(any(grepl("A column \\| not", txt, fixed = TRUE)))
})

test_that("print() names the prompt files and cuts a long list at 20; summary() counts by who and level", {
  n <- 23L
  x <- data.frame(rule = "K09", level = "hand", output_id = sprintf("T-%d", seq_len(n)),
                  sheet = "", row = "", field = "", message = "An assumption", hint = "",
                  who = c("ai", rep("human", n - 1L)), stringsAsFactors = FALSE)
  x <- structure(x, class = c("tfl_ai_review", "data.frame"), file = "book.xlsx",
                 kinds = c("ard", "table"),
                 paths = c(repair = "/x/book_repair.md", questions = "/x/book_questions.md"))
  out <- utils::capture.output(print(x))
  expect_identical(out[1L], "tfl_ai_review: book.xlsx (ard, table)")
  expect_true(any(grepl("(1 item) -- book_repair.md", out, fixed = TRUE)))
  expect_true(any(grepl("(22 items) -- book_questions.md", out, fixed = TRUE)))
  expect_true(any(grepl("... and 2 more", out, fixed = TRUE)))
  s <- summary(x)
  expect_identical(names(dimnames(s)), c("who", "level"))
  expect_identical(s[["human", "hand"]], 22L)
  expect_identical(s[["ai", "hand"]], 1L)
})

test_that("print() of a review not written: its items, and no file named", {
  f <- cov_airv_yml(c("layers:", "  - {layer: nope, type: zzz}",
                      "assumptions: [OS is fictional]"))
  r <- tfl_ai_review(f, write = FALSE)
  expect_identical(attr(r, "paths"), character())
  out <- utils::capture.output(print(r))
  expect_true(any(grepl("^For the AI: paste back into the chat [(][0-9]+ items?[)]$", out)))
  expect_true(any(grepl("^For you: answer, then paste back [(]1 item[)]$", out)))
  expect_false(any(grepl("_repair.md", out, fixed = TRUE)))
})
