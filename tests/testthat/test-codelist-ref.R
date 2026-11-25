# A report's code list as a reference to the study's (A7 step 1): `{list: X}`
# (all of X's codes, in its order) or `{list: X, codes: [...]}` (those codes,
# in the order written).  The programs are written as from the same list
# written in place; the sheets keep the reference in the `list` column.
# A study made on the package's fictional adsl, its defaults.yml given the
# fixture study's SEX and AGEGR1 lists, and the fixture's T-14-1-1.

# the study, the report's `codelists:` as given, and an analysis that is not
# cards' (its ARD put in the lists' order, relevel_ard())
clr_study <- function(codelists, env = parent.frame()) {
  skip_if_not_installed("haven")
  root <- withr::local_tempdir(.local_envir = env)
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "CLR-1"))
  cat(c("codelists:", "  SEX: [{value: F, label: Female}, {value: M, label: Male}]",
        "  AGEGR1: [{value: \"<65\", label: \"<65 years\"}, {value: \">=65\", label: \">=65 years\"}]", ""),
      sep = "\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  dir.create(file.path(d, "spec", "outputs"), showWarnings = FALSE)
  file.copy(system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec"),
            file.path(d, "spec", "outputs"))
  clr_put(d, codelists)
  d
}
clr_put <- function(d, codelists) {
  f <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  y <- readLines(f)
  i <- grep("^codelists:", y)
  j <- grep("^content:", y)
  own <- c("    - id: OWN", "      method: custom", "      data: adsl_sub", "      by: [TRT01A]",
           "      variables: [SEX]",
           "      code: \"cards::ard_tabulate(data, by = TRT01A, variables = SEX)\"")
  writeLines(c(y[seq_len(i - 1L)], own, "codelists:", paste0("  ", codelists), y[j:length(y)]), f)
}
clr_trt <- c("TRT01A:", "  - {value: Drug X, label: Drug X 200 mg}",
             "  - {value: Control, label: Control 100 mg}")
clr_inplace <- c(clr_trt, "SEX: [{value: F, label: Female}, {value: M, label: Male}]",
                 "AGEGR1: [{value: \">=65\", label: \">=65 years\"}, {value: \"<65\", label: \"<65 years\"}]")
clr_refs <- c(clr_trt, "SEX: {list: SEX}", "AGEGR1: {list: AGEGR1, codes: [\">=65\", \"<65\"]}")

# what the programs are written from and with
clr_made <- function(d) {
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], st)
  a <- tflspec:::.ard_spec_of_study(st, "T-14-1-1")
  list(codelists = ro$codelists,
       ard = tfl_ard_code(st, "T-14-1-1", save = FALSE),
       content = tfl_content_code(ro),
       rows = tfl_report_codelists(st)[c("output_id", "variable", "value", "label", "order", "under")],
       hash = tfl_ard_spec_hash(a$spec, "T-14-1-1", dir = d, codelists = a$codelists))
}

test_that("a reference is read as one, not as a value; a list in place is as it was", {
  d <- clr_study(clr_refs)
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_identical(o$codelists$SEX, list(list = "SEX"))
  expect_identical(o$codelists$AGEGR1, list(list = "AGEGR1", codes = list(">=65", "<65")))
  expect_identical(o$codelists$TRT01A,
                   list(list(value = "Drug X", label = "Drug X 200 mg"),
                        list(value = "Control", label = "Control 100 mg")))
  # (written back, read again: the same)
  f <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  tfl_write_output(o, f)
  expect_identical(tfl_read_output(f)$codelists, o$codelists)
  clr_put(d, c(clr_trt, "SEX: {list: SEX, code: [F]}"))
  expect_error(tfl_read_outputs(d), "T-14-1-1: codelists: SEX: `code` is not a key", fixed = TRUE)
})

test_that("a reference writes the programs a list written in place writes, byte for byte", {
  d <- clr_study(clr_inplace)
  inplace <- clr_made(d)
  clr_put(d, clr_refs)
  refs <- clr_made(d)
  expect_identical(refs$codelists, inplace$codelists)
  expect_identical(refs$ard, inplace$ard)
  expect_identical(refs$content, inplace$content)
  expect_identical(refs$rows, inplace$rows)
  expect_identical(refs$hash, inplace$hash)
  # (what is compared: the lists' lines, set_levels(), relevel_ard(), the labels)
  ard <- paste(refs$ard, collapse = "\n")
  expect_match(ard, "cl_sex <- c(F = \"Female\", M = \"Male\")", fixed = TRUE)
  expect_match(ard, "cl_agegr1 <- c(`>=65` = \">=65 years\", `<65` = \"<65 years\")", fixed = TRUE)
  expect_match(ard, "set_levels(", fixed = TRUE)
  expect_match(ard, "relevel_ard(SEX = cl_sex)", fixed = TRUE)
  expect_match(paste(refs$content, collapse = "\n"), "Female", fixed = TRUE)
})

test_that("a list or a code the study has not got stops, naming the report, the variable and the list", {
  d <- clr_study(c(clr_trt, "SEX: {list: GENDER}"))
  st <- tfl_read_study_spec(d)
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_error(tfl_resolve_output(o, st),
               "report T-14-1-1, the code list of SEX refers to the study's list GENDER, which the study has not got",
               fixed = TRUE)
  expect_error(tfl_sheets_from_spec(st, list(`T-14-1-1` = o)), "the code list of SEX refers to the study's list GENDER",
               fixed = TRUE)
  clr_put(d, c(clr_trt, "SEX: {list: SEX, codes: [F, U]}"))
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_error(tfl_resolve_output(o, st),
               "report T-14-1-1, the code list of SEX picks \"U\" from the study's list SEX, which has not got it",
               fixed = TRUE)
  # (a study's list is codes, not a reference)
  expect_error(tflspec:::.study_codelists_read(list(RACE = list(list = "SEX"))), "RACE: a study's list is its codes")
})

test_that("the sheets keep a reference in the `list` column; YAML -> sheets -> YAML gives both forms back", {
  d <- clr_study(clr_refs)
  sh <- tfl_sheets_from_yaml(d)
  cl <- sh$table$codelists
  expect_true("list" %in% names(cl))
  sex <- cl[cl$variable == "SEX", ]
  expect_identical(sex$value, c("F", "M"))
  expect_identical(sex$label, c("Female", "Male"))
  expect_identical(sex$list, c("SEX", "SEX"))
  ag <- cl[cl$variable == "AGEGR1", ]
  expect_identical(ag$value, c(">=65", "<65"))
  expect_identical(unique(ag$list), "AGEGR1: >=65 | <65")
  expect_true(all(is.na(cl$list[cl$variable == "TRT01A"])))
  to <- withr::local_tempdir()
  suppressWarnings(suppressMessages(tfl_yaml_from_sheets(sh, to)))
  f <- list.files(to, "^T-14-1-1[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_identical(tfl_read_output(f[1L])$codelists, tfl_read_outputs(d)[["T-14-1-1"]]$codelists)
  # (a list in place stays in place)
  d2 <- clr_study(clr_inplace)
  to2 <- withr::local_tempdir()
  suppressWarnings(suppressMessages(tfl_yaml_from_sheets(tfl_sheets_from_yaml(d2), to2)))
  f2 <- list.files(to2, "^T-14-1-1[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_identical(tfl_read_output(f2[1L])$codelists, tfl_read_outputs(d2)[["T-14-1-1"]]$codelists)
})

test_that("the flat-view workbook keeps a reference: written, read back unchanged, changed in its cell", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  d <- clr_study(clr_refs)
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  x <- openxlsx::read.xlsx(f, "codelists")
  expect_identical(x$list[x$variable == "SEX"], c("SEX", "SEX"))
  expect_identical(x$label[x$variable == "SEX"], c("Female", "Male"))
  r <- tfl_import_spec_xlsx(d, f, write = FALSE)
  expect_false(any(r$changes$sheet %in% "codelists"))
  # the cell changed in "Excel": the reference changes, the other stays
  wb <- openxlsx::loadWorkbook(f)
  x[] <- lapply(x, as.character)
  x$list[x$variable == "SEX"] <- "SEX: M"
  openxlsx::removeWorksheet(wb, "codelists")
  openxlsx::addWorksheet(wb, "codelists")
  openxlsx::writeData(wb, "codelists", x, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  r <- tfl_import_spec_xlsx(d, f)
  expect_true(any(r$changes$column %in% "list" & r$changes$to %in% "SEX: M"))
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_identical(o$codelists$SEX, list(list = "SEX", codes = list("M")))
  expect_identical(o$codelists$AGEGR1, list(list = "AGEGR1", codes = list(">=65", "<65")))
})
