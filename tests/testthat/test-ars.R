ars_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

ars_spec <- function(analyses, purpose = "SECONDARY OUTCOME MEASURE") {
  if (!is.null(purpose) && !"purpose" %in% names(analyses)) {
    analyses$purpose <- purpose
  }
  s <- list(
    study = ars_df(list(key = "id", value = "USUBJID"),
                   list(key = "study_id", value = "PILOT01")),
    datasets = ars_df(list(dataset = "ADSL", path = "adsl.rds"),
                      list(dataset = "ADAE", path = "adae.rds")),
    populations = ars_df(list(population_id = "SAF", dataset = "ADSL",
                              where = "SAFFL == \"Y\"")),
    analyses = analyses)
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) {
      if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

dm_ae <- function(...) ars_spec(ars_df(
  list(output_id = "DM", analysis_id = "GROUPN", method = "categorical",
       population_id = "SAF", variables = "TRT01A"),
  list(output_id = "DM", analysis_id = "AGE", method = "continuous",
       population_id = "SAF", by = "TRT01A", variables = "AGE"),
  list(output_id = "DM", analysis_id = "CAT", method = "categorical",
       population_id = "SAF", by = "TRT01A", variables = "SEX | RACE"),
  list(output_id = "DM", analysis_id = "PAGE", method = "ttest",
       population_id = "SAF", by = "TRT01A", variables = "AGE"),
  list(output_id = "AE", analysis_id = "TEAE", method = "hierarchical",
       dataset = "ADAE", population_id = "SAF", where = "TRTEMFL == \"Y\"",
       by = "TRTA", variables = "AEBODSYS | AEDECOD",
       args = "over_variables = TRUE")), ...)

an_of <- function(ars, id) {
  Filter(function(a) identical(a$id, id), ars$analyses)[[1L]]
}

test_that("an R condition becomes a WhereClause, or NULL when it has none", {
  w <- .ars_where("SAFFL == \"Y\"", "ADSL")
  expect_identical(w$condition, list(dataset = "ADSL", variable = "SAFFL",
                                     comparator = "EQ", value = list("Y")))
  w <- .ars_where("AGE >= 65 & SEX %in% c(\"F\", \"M\") & RACE != \"X\"",
                  "ADSL")
  expect_identical(w$compoundExpression$logicalOperator, "AND")
  expect_length(w$compoundExpression$whereClauses, 3L)
  cmp <- vapply(w$compoundExpression$whereClauses,
                function(z) z$condition$comparator, "")
  expect_identical(cmp, c("GE", "IN", "NE"))
  expect_identical(w$compoundExpression$whereClauses[[1L]]$condition$value,
                   list("65"))
  expect_identical(.ars_where("!(AESEV %in% c(\"MILD\"))", "ADAE")$condition$
                     comparator, "NOTIN")
  w <- .ars_where("(A == \"1\" | B == \"2\") & C == \"3\"", "ADSL")
  expect_identical(w$compoundExpression$whereClauses[[1L]]$
                     compoundExpression$logicalOperator, "OR")
  expect_null(.ars_where("is.na(AESEV)", "ADAE"))
  expect_null(.ars_where("AGE > BMIBL", "ADSL"))
  expect_identical(.ars_where(NA, "ADSL"), list())
})

test_that("the specs become the CDISC model, laid out as CDISC's example", {
  tspec <- list(variables = ars_df(list(
    variable = "TRT01A", label = "Treatment",
    levels = "Placebo | Xanomeline Low Dose | Xanomeline High Dose")))
  ars <- tfl_ars(dm_ae(), tspec)
  expect_s3_class(ars, "tfl_ars")
  expect_identical(ars$id, "PILOT01")
  expect_identical(vapply(ars$outputs, `[[`, "", "id"), c("DM", "AE"))
  # the population
  expect_identical(ars$analysisSets[[1L]]$condition$variable, "SAFFL")
  # a categorical variable: the subject key, grouped by the variable; its
  # percentage divides by the output's subject count
  sex <- an_of(ars, "An_DM_CAT_SEX")
  expect_identical(sex$variable, "USUBJID")
  expect_identical(vapply(sex$orderedGroupings, `[[`, "", "groupingId"),
                   c("AG_ADSL_TRT01A", "AG_ADSL_SEX"))
  expect_identical(vapply(sex$referencedAnalysisOperations, `[[`, "",
                          "analysisId"), c("An_DM_CAT_SEX", "An_DM_GROUPN"))
  expect_identical(an_of(ars, "An_DM_GROUPN")$methodId, "Mth_total_n")
  # a continuous variable is the analysis variable
  expect_identical(an_of(ars, "An_DM_AGE")$variable, "AGE")
  # a test has no result per group
  expect_false(an_of(ars, "An_DM_PAGE")$orderedGroupings[[1L]]$resultsByGroup)
  # the levels the table gives list the groups
  trt <- Filter(function(g) g$id == "AG_ADSL_TRT01A", ars$analysisGroupings)[[1L]]
  expect_false(trt$dataDriven)
  expect_identical(trt$name, "Treatment")
  expect_identical(vapply(trt$groups, `[[`, "", "name"),
                   c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  sx <- Filter(function(g) g$id == "AG_ADSL_SEX", ars$analysisGroupings)[[1L]]
  expect_true(sx$dataDriven)
  # a hierarchy: one analysis per depth, the data subset its where
  ae <- Filter(function(a) startsWith(a$id, "An_AE_TEAE"), ars$analyses)
  expect_identical(vapply(ae, `[[`, "", "id"),
                   c("An_AE_TEAE_ANY", "An_AE_TEAE_L1", "An_AE_TEAE_L2"))
  expect_length(ae[[3L]]$orderedGroupings, 3L)
  expect_identical(ae[[1L]]$dataSubsetId, "DS_1")
  # the AE output had no subject count: one is added, first, and said
  expect_identical(ars$analyses[[which(vapply(ars$analyses, `[[`, "", "id") ==
                                         "An_AE_TEAE_ANY") - 1L]]$id,
                   "An_AE_GROUPN_TRTA")
  un <- tfl_ars_unmapped(ars)
  expect_true(any(un$item == "denominator" & un$where == "AE"))
  # the percentage operation names its numerator and denominator
  cat_m <- Filter(function(m) m$id == "Mth_categorical", ars$methods)[[1L]]
  rel <- cat_m$operations[[2L]]$referencedOperationRelationships
  expect_identical(vapply(rel, function(r) r$referencedOperationRole$
                            controlledTerm, ""), c("NUMERATOR", "DENOMINATOR"))
  expect_identical(rel[[2L]]$operationId, "Mth_total_n_1_N")
  # the list of contents: output -> its analyses
  lc <- ars$mainListOfContents$contentsList$listItems
  expect_identical(lc[[1L]]$outputId, "DM")
  expect_identical(lc[[1L]]$sublist$listItems[[1L]]$analysisId, "An_DM_GROUPN")
  # and nothing is wrong with it
  expect_identical(nrow(tfl_check_ars(ars, schema = FALSE)), 0L)
})

test_that("the ARS JSON is valid against CDISC's schema, and the same each time", {
  skip_if_not_installed("jsonvalidate")
  ars <- tfl_ars(dm_ae())
  expect_identical(nrow(tfl_check_ars(ars)), 0L)
  f1 <- withr::local_tempfile(fileext = ".json")
  f2 <- withr::local_tempfile(fileext = ".json")
  tfl_write_ars_json(ars, f1)
  tfl_write_ars_json(tfl_ars(dm_ae()), f2)
  expect_identical(readLines(f1), readLines(f2))
  expect_identical(nrow(tfl_check_ars(f1)), 0L)
  # one value is still a JSON array
  j <- jsonlite::fromJSON(f1, simplifyVector = FALSE)
  expect_type(j$analysisSets[[1L]]$condition$value, "list")
})

test_that("CDISC's own example passes the check", {
  f <- test_path("fixtures", "ars-csd-demographics.json")
  expect_identical(nrow(tfl_check_ars(f, schema = FALSE)), 0L)
  skip_if_not_installed("jsonvalidate")
  expect_identical(nrow(tfl_check_ars(f)), 0L)
})

test_that("a purpose is never guessed: blank, it is named", {
  ars <- tfl_ars(dm_ae(purpose = NULL))
  un <- tfl_ars_unmapped(ars)
  expect_true(all(c("DM / AGE", "AE / TEAE") %in% un$where[un$item == "purpose"]))
  ck <- tfl_check_ars(ars, schema = FALSE)
  expect_true(all(ck$field == "purpose"))
  expect_gt(nrow(ck), 0L)
  # the argument fills the blanks
  ars <- tfl_ars(dm_ae(purpose = NULL), purpose = "primary outcome measure")
  expect_identical(an_of(ars, "An_DM_AGE")$purpose$controlledTerm,
                   "PRIMARY OUTCOME MEASURE")
  expect_identical(an_of(ars, "An_DM_AGE")$reason$controlledTerm,
                   "SPECIFIED IN SAP")
  expect_error(tfl_ars(dm_ae(), purpose = "nonsense"))
})

test_that("what ARS cannot say is listed with the reason", {
  sp <- ars_spec(ars_df(
    list(output_id = "T1", analysis_id = "A", method = "continuous",
         population_id = "SAF", by = "TRT01A", variables = "AGE",
         where = "is.na(BMIBL)", formats = "mean=xx.x",
         args = "na.rm = TRUE"),
    list(output_id = "T1", analysis_id = "C", method = "custom",
         population_id = "SAF", variables = "AGE", code = "my_fun(data)")))
  ars <- tfl_ars(sp, list(cells = ars_df(list(output_id = "T1",
                                              template = "{mean}"))))
  un <- tfl_ars_unmapped(ars)
  expect_true(all(c("analyses$where", "formats", "args", "cells") %in%
                    un$item))
  # custom code goes with its method
  m <- Filter(function(m) m$id == "Mth_custom", ars$methods)[[1L]]
  expect_identical(m$codeTemplate$code, "my_fun(data)")
  # the check finds a broken reference
  ars$analyses[[1L]]$methodId <- "Mth_nope"
  ck <- tfl_check_ars(ars, schema = FALSE)
  expect_true(any(ck$field == "methodId"))
})

test_that("titles, footnotes and the file go to the output's display", {
  line <- function(o, l, t) list(output_id = o, line = l, center = t)
  rs <- list(
    report = ars_df(list(output_id = "DM", file = "t14_1_1.rtf")),
    titles = ars_df(line("DM", "1", "Table 14.1.1"),
                    line("DM", "2", "Demographics")),
    footnotes = ars_df(line("DM", "1", "N: subjects in the population.")))
  ars <- tfl_ars(dm_ae(), report_spec = rs)
  o <- ars$outputs[[1L]]
  expect_identical(o$name, "Table 14.1.1 Demographics")
  d <- o$displays[[1L]]$display
  expect_identical(vapply(d$displaySections, `[[`, "", "sectionType"),
                   c("Title", "Footnote"))
  expect_identical(o$fileSpecifications[[1L]]$fileType$controlledTerm, "rtf")
})

test_that("purpose and reason are columns of the ARD spec workbook", {
  skip_if_not_installed("readxl")
  sp <- dm_ae()
  sp$analyses$reason[1L] <- "SPECIFIED IN PROTOCOL"
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_ard_spec(sp, f)
  back <- tfl_read_ard_spec(f)
  expect_identical(back$analyses$purpose, sp$analyses$purpose)
  expect_identical(back$analyses$reason, sp$analyses$reason)
  expect_true(all(c("purpose", "reason") %in% tfl_spec_columns("analyses")$column))
})

test_that("an added subject count never takes an id the output has", {
  # an output whose GROUPN is a categorical without a grouping and whose
  # other analysis has no `by`: its percentage needs the count without a
  # grouping, which tfl_ars() adds
  sp <- ars_spec(ars_df(
    list(output_id = "T-1", analysis_id = "GROUPN", method = "categorical",
         population_id = "SAF", variables = "TRT01A"),
    list(output_id = "T-1", analysis_id = "KM", method = "custom",
         population_id = "SAF", code = "km(data)")))
  ars <- tfl_ars(sp)
  ids <- vapply(ars$analyses, `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0L)
  expect_true("An_T-1_GROUPN_ALL" %in% ids)
  expect_identical(nrow(tfl_check_ars(ars, schema = FALSE)), 0L)
  # and reading it back leaves the added count out
  f <- withr::local_tempfile(fileext = ".json")
  tfl_write_ars_json(ars, f)
  back <- tfl_ars_to_specs(tfl_read_ars_json(f))$ard$analyses
  expect_identical(back$analysis_id, c("GROUPN", "KM"))
})

test_that("a blank purpose is named once, not again by the schema", {
  skip_if_not_installed("jsonvalidate")
  ck <- tfl_check_ars(tfl_ars(dm_ae(purpose = NULL)))
  expect_true(all(ck$field == "purpose"))
  expect_false(any(startsWith(ck$part, "schema")))
})

test_that("strata are groupings in ARS; a denominator other than the analysis set is said", {
  sp <- exact_spec(list(method = "continuous", population_id = "SAF",
                        by = "TRT01A", strata = "SEX", variables = "AGE",
                        purpose = "PRIMARY OUTCOME MEASURE"))
  ars <- suppressWarnings(tfl_ars(sp))
  an <- ars$analyses[[length(ars$analyses)]]
  expect_identical(vapply(an$orderedGroupings, `[[`, "", "groupingId"),
                   c("AG_ADSL_TRT01A", "AG_ADSL_SEX"))
  sp <- exact_spec(list(method = "categorical", population_id = "SAF",
                        by = "TRT01A", variables = "SEX", denominator = "row",
                        args = "fmt_fun = NULL",
                        purpose = "PRIMARY OUTCOME MEASURE"))
  un <- tfl_ars_unmapped(suppressWarnings(tfl_ars(sp)))
  expect_true(any(un$item == "denominator" & grepl("`row`", un$reason)))
  expect_true(any(un$item == "args" & grepl("fmt_fun = NULL", un$reason,
                                            fixed = TRUE)))
})

test_that("a report of user code: with analyses it is an output, without them it is listed", {
  rs <- list(report = ars_df(
    list(output_id = "DM", type = "table", file = "t.rtf"),
    list(output_id = "AE", type = "user", file = "u.rtf"),
    list(output_id = "U2", type = "user", file = "u2.rtf"),
    list(output_id = "L1", type = "listing", file = "l.rtf"),
    list(output_id = "T9", type = "table", file = "t9.rtf")))
  ars <- tfl_ars(dm_ae(), report_spec = rs)
  # a user-code report that reads the ARD (AE) has its analyses, as a table
  expect_identical(vapply(ars$outputs, `[[`, "", "id"), c("DM", "AE"))
  # the others are not ARS outputs, and the list says why
  un <- tfl_ars_unmapped(ars)
  why <- function(o) un$reason[un$where == o & un$item == "output"]
  expect_match(why("U2"), "user code")
  expect_match(why("L1"), "listing")
  expect_match(why("T9"), "no analyses")
  expect_identical(nrow(tfl_check_ars(ars, schema = FALSE)), 0L)
})

test_that("a function's method carries its call, its file and its statistics", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "R"))
  writeLines(c(
    "helper <- 1",
    "ard_mine <- cards::as_cards_fn(",
    "  function(data, by, variables, ...) stop(\"not run here\"),",
    "  stat_names = c(\"statistic\", \"p.value\"))"),
    file.path(dir, "R", "ard_mine.R"))
  # (ars_spec() builds the spec before `source` is added: its warning about
  # an own function with no source is expected here)
  sp <- suppressWarnings(ars_spec(ars_df(
    list(output_id = "T1", analysis_id = "W", method = "ard_mine",
         population_id = "SAF", by = "TRT01A", variables = "AGE",
         args = "exact = FALSE"),
    list(output_id = "T1", analysis_id = "S",
         method = "cardx::ard_stats_kruskal_test",
         population_id = "SAF", by = "TRT01A", variables = "AGE"))))
  sp$study <- rbind(sp$study, data.frame(key = "source", value = "R/ard_mine.R"))
  ars <- tfl_ars(sp, dir = dir)
  m <- Filter(function(m) grepl("ard_mine", m$codeTemplate$code %||% ""),
              ars$methods)[[1L]]
  code <- m$codeTemplate$code
  # where it is defined, and the call the ARD program makes
  expect_match(code, "# ard_mine(): R/ard_mine.R", fixed = TRUE)
  expect_match(code, "ard_mine(data", fixed = TRUE)
  expect_match(code, "by = TRT01A", fixed = TRUE)
  expect_match(code, "exact = FALSE", fixed = TRUE)
  # its operations: the statistics it declares (as_cards_fn(stat_names = ))
  ops <- vapply(m$operations, function(o) o$label %||% o$name, "")
  expect_setequal(ops, c("statistic", "p.value"))
  # a package's function: its call too, instead of "f(...)"
  s <- Filter(function(m) grepl("cardx::ard_stats_kruskal_test", m$codeTemplate$code %||% ""),
              ars$methods)[[1L]]
  expect_match(s$codeTemplate$code, "cardx::ard_stats_kruskal_test(data", fixed = TRUE)
  # without the folder: the call, no file, the default operation
  ars0 <- tfl_ars(sp, dir = withr::local_tempdir())
  m0 <- Filter(function(m) grepl("ard_mine", m$codeTemplate$code %||% ""),
               ars0$methods)[[1L]]
  expect_false(grepl("R/ard_mine.R", m0$codeTemplate$code, fixed = TRUE))
})
