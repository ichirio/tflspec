xlsx_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

xlsx_spec <- function() {
  s <- list(
    study = xlsx_df(list(key = "id", value = "USUBJID")),
    datasets = xlsx_df(list(dataset = "ADSL", path = "adsl.rds"),
                       list(dataset = "ADAE", path = "adae.rds")),
    populations = xlsx_df(list(population_id = "SAF", dataset = "ADSL",
                               where = "SAFFL == \"Y\"")),
    analyses = xlsx_df(
      list(output_id = "DM", analysis_id = "AGE", method = "continuous",
           population_id = "SAF", by = "TRT01A", variables = "AGE"),
      list(output_id = "DM", analysis_id = "CAT", method = "categorical",
           population_id = "SAF", by = "TRT01A", variables = "SEX | RACE"),
      list(output_id = "AE", analysis_id = "TEAE", method = "hierarchical",
           dataset = "ADAE", population_id = "SAF",
           where = "TRTEMFL == \"Y\" & (AESEV == \"MILD\" | AESEV == \"MODERATE\")",
           by = "TRTA", variables = "AEBODSYS | AEDECOD")))
  s$analyses$purpose <- "SECONDARY OUTCOME MEASURE"
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) {
      if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

read_sheet <- function(f, s) {
  as.data.frame(readxl::read_excel(f, s, col_types = "text"),
                stringsAsFactors = FALSE)
}

test_that("the reporting event is written as CDISC's Excel template", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  rs <- list(titles = xlsx_df(list(output_id = "DM", line = "1",
                                   center = "Table 14.1.1")))
  ars <- tfl_ars(xlsx_spec(), report_spec = rs)
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_ars_xlsx(ars, f)
  # the template's 25 sheets, in its order, with its columns
  expect_identical(readxl::excel_sheets(f), names(.ars_xlsx_sheets))
  for (s in names(.ars_xlsx_sheets)) {
    expect_identical(names(read_sheet(f, s))[seq_along(.ars_xlsx_sheets[[s]])],
                     .ars_xlsx_sheets[[s]], label = s)
  }
  expect_error(tfl_write_ars_xlsx(ars, f), "overwrite")
  # a WhereClause as level / order rows, the compound with its operator
  ds <- read_sheet(f, "DataSubsets")
  expect_identical(ds$level, c("1", "2", "2", "3", "3"))
  expect_identical(ds$compoundExpression_logicalOperator,
                   c("AND", NA, "OR", NA, NA))
  expect_identical(ds$condition_value, c(NA, "Y", NA, "MILD", "MODERATE"))
  # one row per analysis; its groupings, the percentage's references
  an <- read_sheet(f, "Analyses")
  expect_identical(nrow(an), length(ars$analyses))
  sex <- an[an$id == "An_DM_CAT_SEX", ]
  expect_identical(c(sex$groupingId1, sex$groupingId2),
                   c("AG_ADSL_TRT01A", "AG_ADSL_SEX"))
  expect_identical(sex$referencedAnalysisOperations_analysisId2,
                   "An_DM_GROUPN_TRT01A")
  # the hierarchy's deepest level has three groupings
  expect_identical(an$groupingId3[an$id == "An_AE_TEAE_L2"],
                   "AG_ADAE_AEDECOD")
  # one row per operation
  m <- read_sheet(f, "AnalysisMethods")
  expect_identical(m$operation_label[m$id == "Mth_categorical"], c("n", "p"))
  # the list of contents: outputs at level 1, analyses at level 2
  lc <- read_sheet(f, "MainListOfContents")
  expect_identical(lc$listItem_outputId[lc$listItem_level == "1"],
                   c("DM", "AE"))
  # titles; a display without sections gets its name as its title
  d <- read_sheet(f, "Displays")
  expect_identical(d$displaySection_subSection_text[d$id == "DM_D1"],
                   "Table 14.1.1")
  expect_identical(d$displaySection_sectionType[d$id == "AE_D1"], "Title")
})

test_that("the siera profile's code templates and parameters are written", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  ars <- tfl_ars(xlsx_spec(), profile = "siera")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_ars_xlsx(ars, f)
  t <- read_sheet(f, "AnalysisMethodCodeTemplate")
  expect_true(all(t$context == "R (siera)"))
  expect_identical(nrow(t), length(ars$methods))
  p <- read_sheet(f, "AnalysisMethodCodeParameters")
  expect_true("DEN_analysisid" %in% p$parameter_valueSource)
  expect_identical(read_sheet(f, "OtherListsOfContents")$listItem_outputId,
                   c("DM", "AE"))
})

test_that("CDISC's example keeps its document references and categories", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  ars <- tfl_read_ars_json(test_path("fixtures", "ars-csd-demographics.json"))
  f <- withr::local_tempfile(fileext = ".xlsx")
  tfl_write_ars_xlsx(ars, f)
  dr <- read_sheet(f, "AnalysisDocumentRefs")
  expect_true(all(dr$referenceType == "Documentation"))
  expect_true("CDISCPILOT01_SAP" %in% dr$refDocumentId)
  cz <- read_sheet(f, "Categorizations")
  # a sub-categorization names its parent category
  expect_true(any(!is.na(cz$parent_category_id)))
  g <- read_sheet(f, "AnalysisGroupings")
  trt <- g[g$id == "AnlsGrouping_01_Trt", ]
  expect_identical(trt$group_condition_value,
                   c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  expect_true(all(trt$dataDriven %in% c("FALSE", "false", "0")))
})
