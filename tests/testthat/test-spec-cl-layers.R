# A report's code lists from the layers it extends (study defaults, patterns)
# reach its ARD program, as they reach its RTF (#647).  Package fictional
# data (adsl, adlb); the expectations are counted from the lists written.

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
  writeLines(c(paste("id:", id), "kind: table", "population: SAF",
               paste0("extends: ", extends), codelists,
               "ard:", "  analyses:", "    - id: A1", "      method: categorical",
               "      population: SAF", "      by: [ARM]", paste0("      variables: [", variables, "]"),
               "data:", "  source: ard", "  object: ard",
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

test_that("a shift report's code lists in the study defaults label its rows and columns (#647)", {
  skip_on_cran()
  skip_if_not_installed("rtfreporter")
  lists <- paste0(c("ANRIND", "BNRIND"),
                  ": [{value: LOW, label: Low}, {value: NORMAL, label: Normal}, {value: HIGH, label: High}]")
  d <- cll_study(defaults = lists)
  f <- file.path(d, "spec", "outputs", "T-SH.yml")
  tfl_write_output(tfl_output("T-SH", "table", population = "SAF", extends = "defaults",
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

test_that("a pattern's list wins over the defaults' for the same variable (#647)", {
  skip_on_cran()
  d <- cll_study(defaults = cll_sex, pattern = "SEX: [{value: M, label: PatM}, {value: F, label: PatF}]",
                 reports = list("T-A" = list(extends = "[defaults]"),
                                "T-B" = list(extends = "[patterns/pp, defaults]"),
                                "T-C" = list(extends = "[patterns/pp]")))
  ard <- cll_build(d)
  expect_identical(cll_labels(d, "T-A"), c("DefM", "DefF"))
  expect_identical(cll_labels(d, "T-B"), c("PatM", "PatF"))
  expect_identical(cll_labels(d, "T-C"), c("PatM", "PatF"))
  # (the ARD holds the codes, in the list's order)
  expect_identical(cll_codes(ard, "T-B"), c("M", "F"))
})

test_that("a pattern and the defaults give a list each: the variables of both (#647)", {
  skip_on_cran()
  d <- cll_study(defaults = cll_sex, pattern = "RACE: [{value: WHITE, label: White}]",
                 reports = list("T-B" = list(extends = "[patterns/pp, defaults]")))
  cl <- tfl_report_codelists(tfl_read_study_spec(d))
  expect_identical(sort(unique(cl$variable)), c("RACE", "SEX"))
  expect_identical(cl$label[cl$variable == "SEX"], c("DefM", "DefF"))
})

test_that("the report's own list wins over the layer's, per variable (#647)", {
  skip_on_cran()
  own <- c("codelists:", "  SEX: [{value: M, label: OwnM}, {value: F, label: OwnF}]")
  d <- cll_study(defaults = c(cll_sex, "RACE: [{value: WHITE, label: White}]"),
                 reports = list("T-D" = list(extends = "[defaults]", codelists = own)))
  cl <- tfl_report_codelists(tfl_read_study_spec(d))
  expect_identical(cl$label[cl$variable == "SEX"], c("OwnM", "OwnF"))
  # (the layer's list of another variable stays)
  expect_identical(cl$label[cl$variable == "RACE"], "White")
  expect_identical(cll_labels(d, "T-D"), c("OwnM", "OwnF"))
  expect_identical(cll_codes(cll_build(d), "T-D"), c("M", "F"))
})

test_that("a report that extends nothing gets nothing from the layers: its program is as without the list (#647)", {
  skip_on_cran()
  rep <- list("T-E" = list(extends = "none"))
  with <- cll_study(defaults = cll_sex, reports = rep)
  without <- cll_study(reports = rep)
  code <- tfl_ard_code(tfl_read_study_spec(with), "T-E", save = FALSE)
  expect_identical(code, tfl_ard_code(tfl_read_study_spec(without), "T-E", save = FALSE))
  expect_false(any(grepl("cl_sex|DefM", code)))
  expect_identical(nrow(tfl_report_codelists(tfl_read_study_spec(with))), 0L)
  expect_identical(cll_codes(cll_build(with), "T-E"), c("F", "M"))
})

test_that("a layer's list of a variable the report does not analyse is ignored (#647)", {
  skip_on_cran()
  rep <- list("T-A" = list(extends = "[defaults]"))
  with <- cll_study(defaults = c(cll_sex, "RACE: [{value: WHITE, label: White}]"), reports = rep)
  base <- cll_study(defaults = cll_sex, reports = rep)
  code <- tfl_ard_code(tfl_read_study_spec(with), "T-A", save = FALSE)
  expect_false(any(grepl("cl_race|White", code)))
  expect_identical(code, tfl_ard_code(tfl_read_study_spec(base), "T-A", save = FALSE))
  # (the rows are there for the report: the program picks the variables it reads)
  expect_true("RACE" %in% tfl_report_codelists(tfl_read_study_spec(with))$variable)
})

test_that("a table with no template takes the defaults' list: the program carries it and runs (#647)", {
  skip_on_cran()
  d <- cll_study(defaults = cll_sex, reports = list("T-A" = list(extends = "[defaults]")))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, "T-A", save = FALSE)
  expect_length(grep('^cl_sex <- ', code), 1L)
  expect_true(any(grepl('c(M = "DefM", F = "DefF")', code, fixed = TRUE)))
  expect_true(any(grepl("set_levels(", code, fixed = TRUE)))
  ard <- cll_build(d, output_id = "T-A")
  expect_identical(cll_labels(d, "T-A"), c("DefM", "DefF"))
  expect_identical(cll_codes(ard, "T-A"), c("M", "F"))
  # a level the data lacks is counted as the lists have it: two levels, three arms
  x <- ard[ard$variable == "SEX" & ard$stat_name == "n", ]
  expect_identical(nrow(x), 2L * length(unique(stats::na.omit(tflspec::adsl$ARM))))
})

test_that("a changed list of a layer changes the report's ARD fingerprint (#647)", {
  skip_on_cran()
  a <- cll_study(defaults = cll_sex, reports = list("T-A" = list(extends = "[defaults]")))
  b <- cll_study(defaults = "SEX: [{value: M, label: Mmm}, {value: F, label: DefF}]",
                 reports = list("T-A" = list(extends = "[defaults]")))
  hash <- function(d) {
    code <- tfl_ard_code(tfl_read_study_spec(d), "T-A", save = TRUE, part = "body")
    grep("definition = ", code, value = TRUE)
  }
  expect_length(hash(a), 1L)
  expect_false(identical(hash(a), hash(b)))
})

test_that("a layer's list that does not cover the data's values stops that report's ARD (#647)", {
  skip_on_cran()
  d <- cll_study(defaults = "SEX: [{value: M, label: DefM}]",
                 reports = list("T-A" = list(extends = "[defaults]"), "T-E" = list(extends = "none")))
  st <- tfl_read_study_spec(d)
  # (the value the list lacks is counted from the data)
  expect_true("F" %in% tflspec::adsl$SEX)
  expect_error(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-A", save = FALSE)),
               'SEX has values its code list does not: "F"')
  # a report that does not extend it is built as it was
  expect_gt(nrow(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-E", save = FALSE))), 0L)
})

test_that("the sheets hold a report's own lists only: no row for a layer's (#647)", {
  skip_on_cran()
  d <- cll_study(defaults = cll_sex, reports = list("T-A" = list(extends = "[defaults]")))
  expect_identical(nrow(tfl_sheets_from_yaml(d)$table$codelists), 0L)
  # the helper adds them
  expect_identical(tfl_report_codelists(tfl_read_study_spec(d))$output_id, c("T-A", "T-A"))
})

test_that("a spec/codelists.yml left in the study is ignored, beside a layer's list (#647, #659)", {
  skip_on_cran()
  d <- cll_study(defaults = cll_sex,
                 file = c("SEX: [{value: M, label: FileM}, {value: F, label: FileF}]",
                          "ANRIND: [{value: LOW, label: Low}]"),
                 reports = list("T-A" = list(extends = "[defaults]"), "T-E" = list(extends = "none")))
  expect_message(st <- tfl_read_study_spec(d), "spec/codelists.yml is not read", fixed = TRUE)
  # the sheets have no row for it
  expect_identical(nrow(tfl_sheets_from_yaml(d)$table$codelists), 0L)
  cl <- tfl_report_codelists(st)
  expect_false(anyNA(cl$output_id))
  expect_identical(cl$label, c("DefM", "DefF"))
  ard <- cll_build(d)
  expect_identical(cll_labels(d, "T-A"), c("DefM", "DefF"))
  expect_identical(cll_codes(ard, "T-A"), c("M", "F"))
  expect_identical(cll_labels(d, "T-E"), NULL)
  expect_identical(cll_codes(ard, "T-E"), c("F", "M"))
})

test_that("tfl_report_codelists(): the sheets' shape, none when no report has a list, `outputs` as read (#647)", {
  skip_on_cran()
  d <- cll_study(reports = list("T-A" = list(extends = "[defaults]")))
  st <- tfl_read_study_spec(d)
  cl <- tfl_report_codelists(st)
  expect_identical(cl, tfl_sheets_from_yaml(d)$table$codelists)
  expect_identical(nrow(cl), 0L)
  expect_identical(names(cl), c("output_id", "variable", "value", "label", "order", "under"))
  cat("codelists:\n  SEX: [{value: M, label: DefM}, {value: F, label: DefF}]\n",
      file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  st <- tfl_read_study_spec(d)
  cl <- tfl_report_codelists(st)
  expect_identical(cl$order, c("1", "2"))
  expect_identical(cl, tfl_report_codelists(st, tfl_read_outputs(d)))
  expect_error(tfl_report_codelists(list()), "tfl_read_study_spec")
})

test_that("tfl_report_codelists(rows =): the caller's own rows, the sheets not made again (#647)", {
  skip_on_cran()
  own <- c("codelists:", "  RACE: [{value: WHITE, label: OwnWhite}]")
  d <- cll_study(defaults = cll_sex, reports = list("T-A" = list(extends = "[defaults]", codelists = own),
                                                    "T-B" = list(extends = "[defaults]")))
  st <- tfl_read_study_spec(d)
  sheets <- tfl_sheets_from_yaml(d)$table$codelists
  base <- tfl_report_codelists(st)
  # the sheets' rows given: the same as made
  expect_identical(tfl_report_codelists(st, rows = sheets), base)
  # the sheets are not made when the rows are given
  testthat::local_mocked_bindings(tfl_sheets_from_spec = function(...) stop("sheets made"),
                                  .package = "tflspec")
  expect_identical(tfl_report_codelists(st, rows = sheets), base)
  # a changed row is what comes back
  chg <- sheets
  chg$label[chg$variable == "RACE"] <- "Changed"
  cl <- tfl_report_codelists(st, rows = chg)
  expect_identical(cl$label[cl$variable == "RACE"], "Changed")
  expect_identical(cl$label[cl$variable == "SEX"], rep(c("DefM", "DefF"), 2L))
  # a column of the caller's is kept, NA on a layer's rows
  chg$note <- "mine"
  cl <- tfl_report_codelists(st, rows = chg)
  expect_identical(names(cl), c(names(sheets), "note"))
  expect_identical(cl$note[cl$variable == "RACE"], "mine")
  expect_true(all(is.na(cl$note[cl$variable == "SEX"])))
  # a report only in the rows stays, after the others, with nothing added
  extra <- chg[1L, ]
  extra$output_id <- "T-Z"
  cl <- tfl_report_codelists(st, rows = rbind(chg, extra))
  expect_identical(cl$output_id[nrow(cl)], "T-Z")
  expect_identical(sum(cl$output_id == "T-Z"), 1L)
  # a row of no report is dropped, as in the default
  na <- chg[1L, ]
  na$output_id <- NA_character_
  expect_false(anyNA(tfl_report_codelists(st, rows = rbind(chg, na))$output_id))
  # none given: the layers' lists, in the standard columns
  none <- tfl_report_codelists(st, rows = sheets[0L, c("output_id", "variable")])
  expect_identical(names(none), c("output_id", "variable", "value", "label", "order", "under"))
  expect_identical(nrow(none), 4L)
  expect_identical(nrow(tfl_report_codelists(st, list(), rows = sheets[0L, ])), 0L)
})
