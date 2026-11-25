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
  # (the analysis once: a second call changes the lists only)
  if (any(grepl("- id: OWN", y, fixed = TRUE))) own <- character()
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
  # (AGEGR1: the codes picked, in the order written)
  expect_match(ard, "cl_agegr1 <- c\\([^\n]*>=65 years[^\n]*<65 years")
  expect_match(ard, "set_levels(", fixed = TRUE)
  # (the analysis that is not cards': its ARD in the lists' order, by and variables)
  expect_match(ard, "relevel_ard\\([^)]*SEX = cl_sex")
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

test_that("the study's lists cross the sheets (study_codelists): the study written back resolves its references", {
  d <- clr_study(clr_refs)
  sh <- tfl_sheets_from_yaml(d)
  scl <- sh$study_codelists
  expect_identical(names(scl), c("name", "value", "label", "order"))
  expect_identical(scl$value[scl$name == "AGEGR1"], c("<65", ">=65"))
  expect_identical(scl$label[scl$name == "SEX"], c("Female", "Male"))
  expect_false("codelists" %in% sh$not_converted$part)
  # the lines a code list writes in the ARD program
  cl_lines <- function(dir) {
    code <- tfl_ard_code(tfl_read_study_spec(dir), "T-14-1-1", save = FALSE)
    grep("^cl_[a-z0-9_]+ <- |set_levels\\(|relevel_ard\\(", code, value = TRUE)
  }
  before <- cl_lines(d)
  expect_match(paste(before, collapse = "\n"), "cl_sex <- c(F = \"Female\", M = \"Male\")", fixed = TRUE)
  st0 <- tfl_read_study_spec(d)
  ro0 <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], st0)
  to <- withr::local_tempdir()
  for (overwrite in c(FALSE, TRUE)) {
    suppressWarnings(suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = overwrite)))
    st1 <- tfl_read_study_spec(to)
    expect_identical(st1$codelists, st0$codelists, info = paste("overwrite", overwrite))
    o1 <- tfl_read_outputs(to)[["T-14-1-1"]]
    expect_identical(o1$codelists$SEX, list(list = "SEX"))
    expect_identical(tfl_resolve_output(o1, st1)$codelists, ro0$codelists, info = paste("overwrite", overwrite))
    expect_identical(cl_lines(to), before, info = paste("overwrite", overwrite))
  }
})

test_that("the rows of one variable agree on their `list` cell; a cell that cannot be read back stops", {
  d <- clr_study(clr_refs)
  sh <- tfl_sheets_from_yaml(d)
  k <- which(sh$table$codelists$variable == "SEX")
  two <- sh
  two$table$codelists$list[k[2L]] <- "SEX: M"
  expect_error(suppressMessages(tfl_yaml_from_sheets(two, withr::local_tempdir())),
               "report T-14-1-1, variable SEX: its rows' `list` cells differ (\"SEX\", \"SEX: M\")", fixed = TRUE)
  blank <- sh
  blank$table$codelists$list[k[2L]] <- NA
  expect_error(suppressMessages(tfl_yaml_from_sheets(blank, withr::local_tempdir())),
               "its rows' `list` cells differ (\"SEX\", \"(blank)\")", fixed = TRUE)
  # a code with "|", a list name with ":": not written as a cell
  expect_error(tflspec:::.cl_ref_cell(list(list = "SEX", codes = list("A|B", "C")), "T-1, SEX"),
               "T-1, SEX: the reference to the study's list SEX cannot be the codelists sheet's `list` cell \"SEX: A|B | C\": a code with \"|\" (\"A|B\")",
               fixed = TRUE)
  expect_error(tflspec:::.cl_ref_cell(list(list = "A:B")), "a list name with \":\" is read back as the name before it",
               fixed = TRUE)
  expect_identical(tflspec:::.cl_ref_cell(list(list = "SEX", codes = list("M", "F"))), "SEX: M | F")
  expect_identical(tflspec:::.cl_ref_from_cell("SEX: M | F"), list(list = "SEX", codes = list("M", "F")))
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
  expect_false(any(grepl("^codelists", r$ignored)))
  x[] <- lapply(x, as.character)
  put <- function(x) {
    wb <- openxlsx::loadWorkbook(f)
    openxlsx::removeWorksheet(wb, "codelists")
    openxlsx::addWorksheet(wb, "codelists")
    openxlsx::writeData(wb, "codelists", x, keepNA = FALSE)
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  }
  # a reference's row edited: not read back, said and listed in `ignored`
  y <- x
  y$label[y$variable == "SEX" & y$value == "F"] <- "Woman"
  put(y)
  expect_message(r <- tfl_import_spec_xlsx(d, f, write = FALSE),
                 "codelists T-14-1-1 / SEX: the rows of a reference to the study's list SEX", fixed = TRUE)
  expect_true("codelists: T-14-1-1 / SEX" %in% r$ignored)
  expect_false(any(r$changes$sheet %in% "codelists"))
  # the rows' cells disagree: the import stops
  y <- x
  y$list[which(y$variable == "SEX")[2L]] <- "SEX: M"
  put(y)
  expect_error(tfl_import_spec_xlsx(d, f, write = FALSE), "its rows' `list` cells differ", fixed = TRUE)
  # the cell changed in "Excel": the reference changes, the other stays
  x$list[x$variable == "SEX"] <- "SEX: M"
  put(x)
  r <- tfl_import_spec_xlsx(d, f)
  expect_true(any(r$changes$column %in% "list" & r$changes$to %in% "SEX: M"))
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_identical(o$codelists$SEX, list(list = "SEX", codes = list("M")))
  expect_identical(o$codelists$AGEGR1, list(list = "AGEGR1", codes = list(">=65", "<65")))
})

test_that("a template that adds codes to a reference: the code SPEC writes the list in place, and says so", {
  skip_on_cran()
  d <- clr_study(clr_trt)
  st <- tfl_read_study_spec(d)
  demog <- function(levels) {
    tfl_output("T-9", "table", common = list(population = "SAF"), codelists = list(SEX = list(list = "SEX")),
               template = list(name = "demog", args = list(
                 by = "TRT01A", continuous = "AGE", categorical = "SEX",
                 categorical_levels = list(SEX = levels))))
  }
  expect_message(cs <- tfl_template_to_spec(demog(c("F", "M", "U")), st),
                 "T-9: the template adds codes to SEX, a reference to the study's list SEX", fixed = TRUE)
  expect_identical(vapply(cs$codelists$SEX, `[[`, "", "value"), c("F", "M", "U"))
  # (the template's codes all in the list: the reference stays, nothing said)
  msgs <- character()
  cs2 <- withCallingHandlers(tfl_template_to_spec(demog(c("M", "F")), st), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_identical(cs2$codelists$SEX, list(list = "SEX"))
  expect_false(any(grepl("adds codes", msgs, fixed = TRUE)))
})
