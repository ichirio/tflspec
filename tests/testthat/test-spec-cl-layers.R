# A report's code lists are its own (canon 2026-11-17): no layer gives one
# (a pattern's `codelists:` stops; the study's in defaults.yml are
# study$codelists, named where used).  The report's own lists reach its ARD
# program as they reach its RTF (#647).  Package fictional data (adsl,
# adlb); the expectations are counted from the lists written.

cll_study <- function(defaults = NULL, pattern = NULL, file = NULL, reports = list(), shift = FALSE,
                      env = parent.frame()) {
  skip_if_not_installed("haven")
  root <- withr::local_tempdir(.local_envir = env)
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)), file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "CL-1"))
  spec <- file.path(d, "spec")
  if (length(defaults)) cat(c("codelists:", paste0("  ", defaults), ""), sep = "\n",
                            file = file.path(spec, "defaults.yml"), append = TRUE)
  if (length(pattern)) {
    dir.create(file.path(spec, "patterns"), showWarnings = FALSE)
    writeLines(c("codelists:", paste0("  ", pattern)), file.path(spec, "patterns", "pp.yml"))
  }
  if (length(file)) writeLines(file, file.path(spec, "codelists.yml"))
  for (id in names(reports)) cll_sex_report(d, id, reports[[id]]$extends, reports[[id]]$codelists,
                                            reports[[id]]$by %||% "SEX")
  d
}

# a table of one categorical analysis (no template), by ARM
cll_sex_report <- function(d, id, extends = "[defaults]", codelists = NULL, variables = "SEX") {
  writeLines(c(paste("id:", id), "kind: table", "common: {population: SAF}",
               paste0("extends: ", extends), codelists,
               "analysis:", "  analyses:", "    - id: A1", "      method: categorical",
               "      population: SAF", "      by: [ARM]", paste0("      variables: [", variables, "]"),
               "content:", "  - table_plan: {cols: ARM}"),
             file.path(d, "spec", "outputs", paste0(id, ".yml")))
}

# the ARD holds the codes
cll_codes <- function(ard, id) {
  x <- ard[ard$output_id == id & ard$variable == "SEX", ]
  unique(unlist(lapply(x$variable_level, as.character)))
}
# the labels of the list that wins for a report: the program's head, `cl_sex <- c(M = "DefM", ...)`
cll_labels <- function(d, id) {
  code <- tfl_ard_code(tfl_read_study_spec(d), id, save = FALSE)
  line <- grep("^cl_sex <- ", code, value = TRUE)
  if (length(line)) unname(eval(parse(text = line[1L])))
}
cll_build <- function(d, ...) {
  suppressMessages(tfl_build_ard(tfl_read_study_spec(d), dir = d, save = FALSE, ...))
}
cll_sex <- c("SEX: [{value: M, label: DefM}, {value: F, label: DefF}]")

test_that("a shift report's own code lists label its rows and columns (#647)", {
  skip_on_cran()
  skip_if_not_installed("rtfreporter")
  lv <- list(list(value = "LOW", label = "Low"), list(value = "NORMAL", label = "Normal"),
             list(value = "HIGH", label = "High"))
  d <- cll_study()
  f <- file.path(d, "spec", "outputs", "T-SH.yml")
  tfl_write_output(tfl_output("T-SH", "table", common = list(population = "SAF"),
                              extends = "defaults", codelists = list(ANRIND = lv, BNRIND = lv),
    template = list(name = "shift", args = list(population = "SAF", where = 'PARAMCD == "ALT"',
                                                page_by = FALSE, levels = c("Low", "Normal", "High")))), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-SH", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(tfl_content_run(ro, ard), "table")))
  # (the three levels written, then the Total row; no Missing row: a missing
  # baseline is out by default, missing_label = NULL)
  expect_identical(as.character(t$BNRIND), c("Low", "Normal", "High", "Total"))
  expect_identical(unique(sub("^.*____", "", grep("____", names(t), value = TRUE))),
                   c("Low", "Normal", "High"))
})

test_that("a pattern's codelists: stops the study's read: no layer gives a list", {
  skip_on_cran()
  d <- cll_study()
  dir.create(file.path(d, "spec", "patterns"))
  writeLines(c("codelists:", paste0("  ", cll_sex)), file.path(d, "spec", "patterns", "pp.yml"))
  expect_error(tfl_read_study_spec(d), "pp.yml `codelists:`: a pattern holds no code list", fixed = TRUE)
})

test_that("the study's code lists (defaults.yml) reach no report: they are study$codelists", {
  skip_on_cran()
  d <- cll_study(defaults = cll_sex, reports = list("T-A" = list(extends = "[defaults]")))
  st <- tfl_read_study_spec(d)
  expect_identical(st$codelists$SEX, c(M = "DefM", F = "DefF"))
  expect_null(cll_labels(d, "T-A"))
  expect_identical(nrow(tfl_report_codelists(st)), 0L)
  # (no list: the data's own order)
  expect_identical(cll_codes(cll_build(d), "T-A"), c("F", "M"))
})

test_that("a report's own list: the program carries it, puts it on, and runs (#647)", {
  skip_on_cran()
  own <- c("codelists:", "  SEX: [{value: M, label: OwnM}, {value: F, label: OwnF}]")
  d <- cll_study(reports = list("T-D" = list(extends = "[defaults]", codelists = own)))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, "T-D", save = FALSE)
  expect_length(grep("^cl_sex <- ", code), 1L)
  expect_true(any(grepl('c(M = "OwnM", F = "OwnF")', code, fixed = TRUE)))
  expect_true(any(grepl("set_levels(", code, fixed = TRUE)))
  cl <- tfl_report_codelists(st)
  expect_identical(cl$label, c("OwnM", "OwnF"))
  expect_identical(cll_labels(d, "T-D"), c("OwnM", "OwnF"))
  ard <- cll_build(d, output_id = "T-D")
  expect_identical(cll_codes(ard, "T-D"), c("M", "F"))
  # a level the data lacks is counted as the list has it: two levels, three arms
  x <- ard[ard$variable == "SEX" & ard$stat_name == "n", ]
  expect_identical(nrow(x), 2L * length(unique(stats::na.omit(tflspec::adsl$ARM))))
})

test_that("a resolved table keeps its own lists, in their order", {
  skip_on_cran()
  own <- c("codelists:", "  ARM: [{value: A, label: Arm A}]", "  AGEGR1: [{value: '<65'}]",
           "  SEX: [{value: M, label: DefM}, {value: F, label: DefF}]")
  d <- cll_study(reports = list("T-A" = list(extends = "[defaults]", codelists = own)))
  st <- tfl_read_study_spec(d)
  r <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", "T-A.yml")), st)
  expect_identical(names(r$codelists), c("ARM", "AGEGR1", "SEX"))
  expect_true(any(grepl("DefM", tfl_content_code(r), fixed = TRUE)))
})

test_that("a changed list of the report changes its ARD fingerprint (#647)", {
  skip_on_cran()
  own <- function(m) c("codelists:", sprintf("  SEX: [{value: M, label: %s}, {value: F, label: DefF}]", m))
  a <- cll_study(reports = list("T-A" = list(extends = "[defaults]", codelists = own("DefM"))))
  b <- cll_study(reports = list("T-A" = list(extends = "[defaults]", codelists = own("Mmm"))))
  hash <- function(d) {
    code <- tfl_ard_code(tfl_read_study_spec(d), "T-A", save = TRUE, part = "body")
    grep("definition = ", code, value = TRUE)
  }
  expect_length(hash(a), 1L)
  expect_false(identical(hash(a), hash(b)))
})

test_that("a report's list that does not cover the data's values stops that report's ARD (#647)", {
  skip_on_cran()
  d <- cll_study(reports = list("T-A" = list(extends = "[defaults]",
                                             codelists = c("codelists:", "  SEX: [{value: M, label: DefM}]")),
                                "T-E" = list(extends = "none")))
  st <- tfl_read_study_spec(d)
  # (the value the list lacks is counted from the data)
  expect_true("F" %in% tflspec::adsl$SEX)
  expect_error(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-A", save = FALSE)),
               'SEX has values its code list does not: "F"')
  expect_gt(nrow(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-E", save = FALSE))), 0L)
})

test_that("a spec/codelists.yml left in the study is ignored (#647, #659)", {
  skip_on_cran()
  d <- cll_study(file = c("SEX: [{value: M, label: FileM}, {value: F, label: FileF}]",
                          "ANRIND: [{value: LOW, label: Low}]"),
                 reports = list("T-A" = list(extends = "[defaults]")))
  expect_message(st <- tfl_read_study_spec(d), "spec/codelists.yml is not read", fixed = TRUE)
  expect_identical(nrow(tfl_sheets_from_yaml(d)$table$codelists), 0L)
  expect_identical(nrow(tfl_report_codelists(st)), 0L)
  expect_null(cll_labels(d, "T-A"))
  expect_identical(cll_codes(cll_build(d), "T-A"), c("F", "M"))
})

test_that("tfl_report_codelists(): the sheets' rows, a report's own (#647)", {
  skip_on_cran()
  d <- cll_study(reports = list("T-A" = list(extends = "[defaults]")))
  st <- tfl_read_study_spec(d)
  cl <- tfl_report_codelists(st)
  expect_identical(cl, tfl_sheets_from_yaml(d)$table$codelists)
  expect_identical(nrow(cl), 0L)
  expect_identical(names(cl), c("output_id", "variable", "value", "label", "order", "under", "list"))
  cll_sex_report(d, "T-A", "[defaults]", c("codelists:", paste0("  ", cll_sex)))
  st <- tfl_read_study_spec(d)
  cl <- tfl_report_codelists(st)
  expect_identical(cl$order, c("1", "2"))
  expect_identical(cl, tfl_sheets_from_yaml(d)$table$codelists)
  expect_identical(cl, tfl_report_codelists(st, tfl_read_outputs(d)))
  expect_error(tfl_report_codelists(list()), "tfl_read_study_spec")
})

test_that("tfl_report_codelists(rows =): the caller's own rows, the sheets not made again (#647)", {
  skip_on_cran()
  own <- c("codelists:", "  RACE: [{value: WHITE, label: OwnWhite}]")
  d <- cll_study(reports = list("T-A" = list(extends = "[defaults]", codelists = own),
                                "T-B" = list(extends = "[defaults]")))
  st <- tfl_read_study_spec(d)
  sheets <- tfl_sheets_from_yaml(d)$table$codelists
  base <- tfl_report_codelists(st)
  expect_identical(tfl_report_codelists(st, rows = sheets), base)
  testthat::local_mocked_bindings(tfl_sheets_from_spec = function(...) stop("sheets made"),
                                  .package = "tflspec")
  expect_identical(tfl_report_codelists(st, rows = sheets), base)
  # a changed row is what comes back
  chg <- sheets
  chg$label[chg$variable == "RACE"] <- "Changed"
  cl <- tfl_report_codelists(st, rows = chg)
  expect_identical(cl$label[cl$variable == "RACE"], "Changed")
  # a column of the caller's is kept
  chg$note <- "mine"
  cl <- tfl_report_codelists(st, rows = chg)
  expect_identical(names(cl), c(names(sheets), "note"))
  expect_identical(cl$note, "mine")
  # a report only in the rows stays, after the others
  extra <- chg[1L, ]
  extra$output_id <- "T-Z"
  cl <- tfl_report_codelists(st, rows = rbind(chg, extra))
  expect_identical(cl$output_id[nrow(cl)], "T-Z")
  expect_identical(sum(cl$output_id == "T-Z"), 1L)
  # a row of no report is dropped
  na <- chg[1L, ]
  na$output_id <- NA_character_
  expect_false(anyNA(tfl_report_codelists(st, rows = rbind(chg, na))$output_id))
  # none given: none
  none <- tfl_report_codelists(st, rows = sheets[0L, c("output_id", "variable")])
  expect_identical(names(none), c("output_id", "variable", "value", "label", "order", "under", "list"))
  expect_identical(nrow(none), 0L)
  expect_identical(nrow(tfl_report_codelists(st, list(), rows = sheets[0L, ])), 0L)
})
