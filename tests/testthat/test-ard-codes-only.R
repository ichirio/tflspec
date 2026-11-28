# The ARD side takes a report's code lists as codes only (canon 11-28,
# CANON-3): the ARD program and its fingerprint have the codes, their order
# and `under`, never a label; the labels are the report program's
# plan_labels().  A report's list that no step uses is a warning.

co_cl <- function(label = c("Under 65", "65 to 80", "Over 80", "Men", "Women")) {
  data.frame(output_id = "T",
             variable = c("AGEGR1", "AGEGR1", "AGEGR1", "SEX", "SEX"),
             value = c("<65", "65-80", ">80", "M", "F"),
             label = label, order = c("1", "2", "3", "1", "2"),
             stringsAsFactors = FALSE)
}

co_spec <- function(where = NA) {
  exact_spec(list(method = "categorical", dataset = "ADSL", population_id = "SAF",
                  by = "ARM", variables = "AGEGR1 | SEX", where = where))
}

test_that("a label-only change leaves the ARD program and its fingerprint as they were", {
  sp <- co_spec()
  cl <- co_cl()
  relabel <- co_cl(c("Below 65", "65-80 years", "Above 80", "Male", "Female"))
  nolabel <- co_cl(rep(NA_character_, 5L))
  code <- tfl_ard_code(sp, save = FALSE, codelists = cl)
  expect_identical(tfl_ard_code(sp, save = FALSE, codelists = relabel), code)
  # (labels or none: the same program)
  expect_identical(tfl_ard_code(sp, save = FALSE, codelists = nolabel), code)
  expect_identical(tfl_ard_code(sp, part = "body", codelists = relabel),
                   tfl_ard_code(sp, part = "body", codelists = cl))
  # (the program saved, with its fingerprint in it)
  expect_identical(tfl_ard_code(sp, save = TRUE, codelists = relabel),
                   tfl_ard_code(sp, save = TRUE, codelists = cl))
  h <- tfl_ard_spec_hash(sp, "T", codelists = cl)
  expect_identical(tfl_ard_spec_hash(sp, "T", codelists = relabel), h)
  expect_identical(tfl_ard_spec_hash(sp, "T", codelists = nolabel), h)
  # the lists' lines: codes only
  expect_true("cl_sex <- c(\"M\", \"F\")" %in% code)
  expect_true("cl_agegr1 <- c(\"<65\", \"65-80\", \">80\")" %in% code)
  expect_false(any(grepl("Men|Women|Under 65|Over 80", code)))
})

test_that("a change of codes or of their order changes the ARD program and its fingerprint", {
  sp <- co_spec()
  cl <- co_cl()
  code <- tfl_ard_code(sp, save = FALSE, codelists = cl)
  h <- tfl_ard_spec_hash(sp, "T", codelists = cl)
  # (another code)
  recode <- cl
  recode$value[5L] <- "U"
  expect_false(identical(tfl_ard_code(sp, save = FALSE, codelists = recode), code))
  expect_false(identical(tfl_ard_spec_hash(sp, "T", codelists = recode), h))
  # (the same codes in another order)
  reorder <- cl
  reorder$order[4:5] <- c("2", "1")
  expect_true("cl_sex <- c(\"F\", \"M\")" %in% tfl_ard_code(sp, save = FALSE, codelists = reorder))
  expect_false(identical(tfl_ard_spec_hash(sp, "T", codelists = reorder), h))
  # (a code taken off)
  fewer <- cl[-3L, ]
  expect_false(identical(tfl_ard_spec_hash(sp, "T", codelists = fewer), h))
  # (an `under`: the fingerprint has it)
  under <- cl
  under$under <- c(NA, NA, NA, NA, NA)
  expect_identical(tfl_ard_spec_hash(sp, "T", codelists = under), h)
  under$under[2L] <- "<65"
  expect_false(identical(tfl_ard_spec_hash(sp, "T", codelists = under), h))
})

test_that("the codes-only list is narrowed by its values, and the ARD has those levels", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- co_spec("AGEGR1 %in% c(\"<65\", \"65-80\")")
  cl <- co_cl()
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "AGEGR1 = intersect(cl_agegr1, c(\"<65\", \"65-80\"))", fixed = TRUE)
  spx <- co_spec("SEX != \"F\"")
  expect_match(paste(tfl_ard_code(spx, part = "body", codelists = cl), collapse = "\n"),
               "SEX = setdiff(cl_sex, c(\"F\"))", fixed = TRUE)
  # the program as written runs, and keeps the levels the filter leaves
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  ag <- unique(vapply(ard$variable_level[ard$variable == "AGEGR1"], as.character, ""))
  expect_identical(ag, c("<65", "65-80"))
  # (the codes, never a label, in the ARD)
  expect_false(any(c("Under 65", "Men", "Women") %in% unlist(ard$variable_level)))
  # the narrowed list is the same with labels or none: one program
  nolabel <- co_cl(rep(NA_character_, 5L))
  expect_identical(tfl_ard_code(sp, part = "body", codelists = nolabel),
                   tfl_ard_code(sp, part = "body", codelists = cl))
})

test_that("the codes-only form of a report's lists", {
  f <- tflspec:::.codelist_codes_only
  expect_identical(f(list(SEX = c(F = "Female", M = "Male"), AGEGR1 = c("<65", ">=65"))),
                   list(SEX = c("F", "M"), AGEGR1 = c("<65", ">=65")))
  expect_null(f(NULL))
  expect_identical(f(list()), list())
})

co_output <- function(codelists) {
  tfl_output("T-9", "table",
             ard = list(analyses = list(list(id = "A", method = "cards::ard_tabulate",
                                             by = list("TRT01A"), variables = list("SEX")))),
             codelists = codelists,
             content = list(tfl_item("table_plan", list(cols = "TRT01A",
                                                        rows = list(group = "variable")))))
}

test_that("a report's code list that no step uses warns once, naming the variable; it does not stop", {
  cl <- list(TRT01A = list(list(value = "A", label = "Drug A")),
             SEX = list(list(value = "F", label = "Female")),
             ETHNIC = list(list(value = "HL", label = "Hispanic or Latino")),
             RACE = list(list(value = "W", label = "White")))
  o <- co_output(cl)
  expect_identical(tflspec:::.cl_unused(o), c("ETHNIC", "RACE"))
  rm(list = ls(tflspec:::.cl_unused_memo), envir = tflspec:::.cl_unused_memo)
  expect_warning(tflspec:::.cl_unused_warn(list(o)),
                 "T-9: the code list of ETHNIC, RACE is used by no step of the report", fixed = TRUE)
  # (once: the same report and lists again are silent)
  expect_no_warning(tflspec:::.cl_unused_warn(list(o)))
  # a list the content names (a key or a value) is used; so is one a data
  # step's R names, or a figure's
  o2 <- o
  o2$content <- c(o2$content, list(tfl_item("plan_labels", list(ETHNIC = "Ethnicity"))))
  o2$data <- list(list(id = "adsl_sub", from = "ADSL", where = "RACE == \"W\""))
  expect_identical(tflspec:::.cl_unused(o2), character())
  o3 <- co_output(cl["SEX"])
  expect_identical(tflspec:::.cl_unused(o3), character())
  # (the page and the label are not steps)
  o4 <- o
  o4$page <- list(footnotes = list("1" = list(left = "ETHNIC and RACE as collected")))
  o4$label <- "RACE"
  expect_identical(tflspec:::.cl_unused(o4), c("ETHNIC", "RACE"))
  # a figure's steps
  o5 <- tfl_output("F-9", "figure", codelists = cl["TRT01A"])
  o5$plot <- list(aes = list(colour = "TRT01A"))
  expect_identical(tflspec:::.cl_unused(o5), character())
  expect_identical(tflspec:::.cl_unused(tfl_output("F-8", "figure", codelists = cl["TRT01A"])),
                   "TRT01A")
})

test_that("a list a report's template or the study's layers use is not warned of", {
  cl <- list(TRT01A = list(list(value = "A", label = "Drug A")),
             ETHNIC = list(list(value = "HL", label = "Hispanic or Latino")))
  o <- tfl_output("T-7", "table", codelists = cl)
  expect_identical(tflspec:::.cl_unused(o), c("TRT01A", "ETHNIC"))
  # (the study's defaults name ETHNIC: a layer the report extends)
  st <- structure(list(defaults = list(content = list(list(fn = "plan_labels",
                                                           args = list(ETHNIC = "Ethnicity"))))),
                  class = "tfl_study_spec")
  expect_identical(tflspec:::.cl_unused(o, st), c("TRT01A"))
  o$extends <- "none"
  expect_identical(tflspec:::.cl_unused(o, st), c("TRT01A", "ETHNIC"))
  # (a template's own definition: ae_soc_pt's arms are TRT01A, by default)
  ot <- o
  ot$template <- list(name = "ae_soc_pt")
  expect_identical(tflspec:::.cl_unused(ot), "ETHNIC")
  ot$template <- "no_such_template"
  expect_identical(tflspec:::.cl_unused(ot), c("TRT01A", "ETHNIC"))
})

test_that("tfl_report_codelists() warns of a report's unused list, and gives its rows all the same", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  # (the sample's lists are all used: no warning)
  rm(list = ls(tflspec:::.cl_unused_memo), envir = tflspec:::.cl_unused_memo)
  expect_no_warning(base <- tfl_report_codelists(st, outputs = outs))
  o <- outs[["T-14-1-1"]]
  o$codelists$ETHNIC <- list(list(value = "HL", label = "Hispanic or Latino"))
  outs[["T-14-1-1"]] <- o
  expect_warning(rows <- tfl_report_codelists(st, outputs = outs),
                 "T-14-1-1: the code list of ETHNIC is used by no step", fixed = TRUE)
  expect_true("ETHNIC" %in% rows$variable)
  expect_no_warning(tfl_report_codelists(st, outputs = outs))
})
