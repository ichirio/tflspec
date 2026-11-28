# Branches of a report's code list that refers to the study's (A7, from
# 2026-11-24) that no other test reaches: the reference's own checks
# (R/codelist_ref.R), the sheets and the workbook (R/convert.R,
# R/spec_xlsx_import.R), the ARD programs given rows with a `list` cell
# (R/ard_spec.R), the study's lists (R/spec_ard.R, R/study_refs.R) and a
# code list written as a map (R/spec_yaml.R).  Fictional data only.

# a study made on the package's fictional adsl, with SEX and AGEGR1 lists,
# and the fixture's T-14-1-1 with the code lists given
c24c_study <- function(codelists, env = parent.frame()) {
  skip_if_not_installed("haven")
  root <- tempfile("cov1124-clr")
  dir.create(root)
  do.call(on.exit, list(bquote(unlink(.(root), recursive = TRUE)), add = TRUE), envir = env)
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "CLR-9"))
  cat(c("codelists:", "  SEX: [{value: F, label: Female}, {value: M, label: Male}]",
        "  AGEGR1: [{value: \"<65\", label: \"<65 years\"}, {value: \">=65\", label: \">=65 years\"}]", ""),
      sep = "\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  dir.create(file.path(d, "spec", "outputs"), showWarnings = FALSE)
  file.copy(system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec"),
            file.path(d, "spec", "outputs"))
  f <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  y <- readLines(f)
  i <- grep("^codelists:", y)
  j <- grep("^content:", y)
  writeLines(c(y[seq_len(i - 1L)], "codelists:", paste0("  ", codelists), y[j:length(y)]), f)
  d
}
c24c_trt <- c("TRT01A:", "  - {value: Drug X, label: Drug X 200 mg}",
              "  - {value: Control, label: Control 100 mg}")

# the workbook of the study, and a writer of its codelists sheet
c24c_book <- function(d) {
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  x <- openxlsx::read.xlsx(f, "codelists")
  x[] <- lapply(x, as.character)
  list(file = f, sheet = x, put = function(x) {
    wb <- openxlsx::loadWorkbook(f)
    openxlsx::removeWorksheet(wb, "codelists")
    openxlsx::addWorksheet(wb, "codelists")
    openxlsx::writeData(wb, "codelists", x, keepNA = FALSE)
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  })
}

# ---- the reference as read (R/codelist_ref.R) -----------------------------------

test_that("a reference whose `list` is not one name stops", {
  for (bad in list(1, c("SEX", "AGE"), NA_character_, "  ")) {
    expect_error(.cl_ref_norm(list(list = bad), "SEX", "T-9"),
                 "T-9: codelists: SEX: `list` is the name of one of the study's lists.", fixed = TRUE)
  }
})

test_that("a reference whose `codes` are empty, NA or not single codes stops", {
  for (bad in list(list(), list("F", NA), list("F", list("M", "U")))) {
    expect_error(.cl_ref_norm(list(list = "SEX", codes = bad), "SEX"),
                 "codelists: SEX: `codes` is a list of the list's codes.", fixed = TRUE)
  }
})

test_that("a reference's rows on a sheet without a label column are compared by their codes", {
  want <- list(list(value = "F", label = "F"), list(value = "M", label = "M"))
  expect_false(.cl_ref_rows_edited(data.frame(value = c("F", "M")), want))
  expect_true(.cl_ref_rows_edited(data.frame(value = c("M", "F")), want))
})

test_that("code list rows without an output_id column are made from the study's list", {
  rows <- data.frame(variable = "SEX", value = NA_character_, label = NA_character_, list = "SEX")
  st <- list(SEX = c(F = "Female", M = "Male"))
  out <- .cl_rows_from_study(rows, st, "x")
  expect_identical(out$value, c("F", "M"))
  expect_identical(out$label, c("Female", "Male"))
})

test_that("the study_codelists sheet without order or label columns keeps the rows' order and codes alone", {
  d <- data.frame(name = c("SEX", "SEX"), value = c("M", "F"))
  expect_identical(.conv_study_codelists(d), list(SEX = list("M", "F")))
})

# ---- a code list written as a map (R/spec_yaml.R) -------------------------------

test_that("a map of codes to labels whose label is not one text is named with a placeholder", {
  expect_error(.cl_map_stop(list(F = list("Female", "Woman")), "SEX", "T-9", FALSE),
               "T-9: codelists: SEX: a map of codes to labels ({F: <label>, ...})", fixed = TRUE)
})

# ---- the sheets (R/convert.R) ---------------------------------------------------

test_that("sheets without study_codelists over a defaults.yml whose lists cannot be read: said in a note", {
  d <- c24c_study(c(c24c_trt, "SEX: {list: SEX}"))
  sh <- tfl_sheets_from_yaml(d)
  sh$study_codelists <- NULL
  to <- tempfile("cov1124-to")
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  dir.create(file.path(to, "spec"), recursive = TRUE)
  writeLines(c("codelists:", "  SEX: {list: GENDER}"), file.path(to, "spec", "defaults.yml"))
  r <- suppressWarnings(suppressMessages(tryCatch(tfl_yaml_from_sheets(sh, to, overwrite = TRUE),
                                                  error = function(e) e)))
  nc <- yaml::read_yaml(file.path(to, "spec", "_not_converted.yml"))$not_converted
  why <- vapply(nc, function(e) as.character(e$reason %||% ""), "")
  expect_true(any(grepl("the study's lists cannot be read, so the reports' references to them are not made into rows",
                        why, fixed = TRUE)))
  expect_true(any(grepl("a reference ({list: ...}) is a report's", why, fixed = TRUE)))
})

test_that("a data map that is not a map takes no analysis set", {
  expect_false(.set_on_data("figure", "dataset"))
  expect_false(.set_on_data("table", tfl_r("adsl")))
})

# ---- the workbook (R/spec_xlsx_import.R) ----------------------------------------

test_that("a `list` cell put on a list written in place makes it a reference, its rows not read back", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  d <- c24c_study(c(c24c_trt, "SEX: [{value: F, label: Female}, {value: M, label: Male}]"))
  b <- c24c_book(d)
  x <- b$sheet
  x$list <- x$list %||% NA_character_
  x$list[x$variable == "SEX"] <- "SEX"
  x$label[x$variable == "SEX" & x$value == "F"] <- "Woman"
  b$put(x)
  expect_message(r <- tfl_import_spec_xlsx(d, b$file),
                 "codelists T-14-1-1 / SEX: the rows of a reference to the study's list SEX", fixed = TRUE)
  expect_true("codelists: T-14-1-1 / SEX" %in% r$ignored)
  expect_identical(tfl_read_outputs(d)[["T-14-1-1"]]$codelists$SEX, list(list = "SEX"))
})

test_that("a `list` cell emptied takes the reference off: the rows are written in place", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  d <- c24c_study(c(c24c_trt, "SEX: {list: SEX}"))
  b <- c24c_book(d)
  x <- b$sheet
  x$list[x$variable == "SEX"] <- NA_character_
  b$put(x)
  r <- suppressMessages(tfl_import_spec_xlsx(d, b$file))
  expect_true(any(r$changes$column %in% "list" & r$changes$from %in% "SEX" & is.na(r$changes$to)))
  sex <- tfl_read_outputs(d)[["T-14-1-1"]]$codelists$SEX
  expect_identical(vapply(sex, function(e) e$value, ""), c("F", "M"))
  expect_identical(vapply(sex, function(e) e$label, ""), c("Female", "Male"))
})

test_that("the import's reference check with no changed report file checks nothing", {
  expect_null(.imp_refs_check(tempdir(), stats::setNames(list(), character())))
  expect_null(.imp_refs_check(tempdir(), list("spec/defaults.yml" = list(y = list(), y0 = list()))))
})

test_that("the import's reference check of a new report file: its new error stops it", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  y <- list(id = "T-9", kind = "table", codelists = list(SEX = list(list = "GENDER")))
  files <- list("outputs/T-9.yml" = list(y = y, y0 = NULL, path = "outputs/T-9.yml"))
  expect_error(.imp_refs_check(d, files), "T-9, SEX: the code list of SEX refers to the study's list GENDER",
               fixed = TRUE)
})

# ---- the ARD programs and the study's lists -------------------------------------

test_that("tfl_ard_code() given rows with a `list` cell the study has not got stops", {
  d <- c24c_study(c(c24c_trt, "SEX: {list: SEX}"))
  st <- tfl_read_study_spec(d)
  cl <- data.frame(output_id = "T-14-1-1", variable = "SEX", value = NA_character_,
                   label = NA_character_, list = "GENDER")
  expect_error(tfl_ard_code(st, output_id = "T-14-1-1", save = FALSE, codelists = cl),
               "tfl_ard_code(): report T-14-1-1, the code list of SEX refers to the study's list GENDER",
               fixed = TRUE)
})

test_that("tfl_build_ard() given rows with a `list` cell the study has not got stops", {
  d <- c24c_study(c(c24c_trt, "SEX: {list: SEX}"))
  st <- tfl_read_study_spec(d)
  cl <- data.frame(output_id = "T-14-1-1", variable = "SEX", value = NA_character_,
                   label = NA_character_, list = "SEX: F | U")
  expect_error(tfl_build_ard(st, output_id = "T-14-1-1", save = FALSE, codelists = cl),
               "tfl_build_ard(): report T-14-1-1, the code list of SEX picks \"U\" from the study's list SEX",
               fixed = TRUE)
})

test_that("tfl_report_codelists(): a report's reference with no rows given must be the study's", {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  none <- data.frame(output_id = character())
  ok <- tfl_output("T-9", "table", codelists = list(SEX = list(list = "SEX")))
  expect_silent(tfl_report_codelists(st, list(ok), rows = none))
  bad <- tfl_output("T-9", "table", codelists = list(SEX = list(list = "GENDER")))
  expect_error(tfl_report_codelists(st, list(bad), rows = none),
               "tfl_report_codelists(): report T-9, the code list of SEX refers to the study's list GENDER",
               fixed = TRUE)
})

test_that("tfl_study_refs() refuses a report already resolved", {
  o <- tfl_output("T-9", "table", codelists = list(SEX = list(list = "SEX")))
  attr(o, "resolved") <- TRUE
  expect_error(tfl_study_refs(o), "tfl_study_refs(): T-9 is resolved; give the report as read", fixed = TRUE)
})
