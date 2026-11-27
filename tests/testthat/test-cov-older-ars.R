# CDISC ARS (R/ars_model.R, R/ars_read.R, R/ars_write.R): the branches no
# other test reaches -- conditions with no WhereClause, spec columns ARS has
# no place for, reporting events of other shapes read back, and each problem
# tfl_check_ars() names.  Fictional data only.

cov_ars_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

# an ARD spec as plain sheets (every column there, blank when not given)
cov_ars_raw <- function(analyses, populations = NULL, datasets = NULL) {
  s <- list(
    study = cov_ars_df(list(key = "id", value = "USUBJID"),
                       list(key = "study_id", value = "STUDY01")),
    datasets = datasets %||%
      cov_ars_df(list(dataset = "ADSL", path = "adsl.rds"),
                 list(dataset = "ADAE", path = "adae.rds")),
    populations = populations %||%
      cov_ars_df(list(population_id = "SAF", dataset = "ADSL",
                      where = "SAFFL == \"Y\"")),
    analyses = analyses)
  if (!"purpose" %in% names(s$analyses)) {
    s$analyses$purpose <- "SECONDARY OUTCOME MEASURE"
  }
  for (n in names(.ard_spec_sheets)) {
    for (cc in .ard_spec_sheets[[n]]) {
      if (!cc %in% names(s[[n]])) s[[n]][[cc]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  s
}

cov_ars_spec <- function(...) tfl_ard_spec(cov_ars_raw(...))

# one analysis of output DM on the SAF population
cov_ars_row <- function(...) {
  cov_ars_df(c(list(output_id = "DM", analysis_id = "X", population_id = "SAF"),
               list(...)))
}

cov_ars_ids <- function(ars) vapply(ars$analyses, `[[`, "", "id")

# a reporting event written by hand, as another tool would
cov_re <- function(analyses, methods, sets = NULL, subsets = NULL,
                   groupings = NULL, items = NULL, outputs = NULL) {
  ids <- vapply(analyses, `[[`, "", "id")
  items <- items %||% list(list(
    name = "T1", level = 1L, order = 1L, outputId = "T1",
    sublist = list(listItems = lapply(seq_along(ids), function(k) list(
      name = ids[k], level = 2L, order = k, analysisId = ids[k])))))
  outputs <- outputs %||% list(list(
    id = "T1", name = "T1", version = 1L,
    displays = list(list(order = 1L, display = list(id = "T1_D1",
                                                    name = "T1")))))
  re <- list(id = "RE1", name = "RE1", version = 1L,
             mainListOfContents = list(name = "LOPA",
                                       contentsList = list(listItems = items)),
             analysisSets = sets %||% list(list(
               id = "SAF", name = "SAF", level = 1L, order = 1L,
               condition = list(dataset = "ADSL", variable = "SAFFL",
                                comparator = "EQ", value = list("Y")))),
             dataSubsets = subsets, analysisGroupings = groupings,
             methods = methods, analyses = analyses, outputs = outputs)
  structure(Filter(Negate(is.null), re), class = "tfl_ars",
            profile = "cdisc")
}

cov_method <- function(id, stats, name = id, code = NULL) {
  m <- list(id = id, name = name, operations = lapply(seq_along(stats),
    function(j) list(id = paste0(id, "_", j), name = stats[j],
                     label = stats[j], order = j)))
  if (!is.null(code)) m$codeTemplate <- list(context = "R", code = code)
  m
}

cov_analysis <- function(id, method, variable = "AGE", dataset = "ADSL",
                         groupings = NULL, ...) {
  a <- list(id = id, name = id, version = 1L,
            purpose = list(controlledTerm = "SECONDARY OUTCOME MEASURE"),
            reason = list(controlledTerm = "SPECIFIED IN SAP"),
            methodId = method, analysisSetId = "SAF", dataset = dataset,
            variable = variable, ...)
  if (length(groupings)) {
    a$orderedGroupings <- lapply(seq_along(groupings), function(k) list(
      order = k, groupingId = groupings[k], resultsByGroup = TRUE))
  }
  a
}

cov_grouping <- function(id, var, ds = "ADSL", levels = NULL) {
  g <- list(id = id, name = var, groupingDataset = ds,
            groupingVariable = var, dataDriven = !length(levels))
  if (length(levels)) {
    g$groups <- lapply(seq_along(levels), function(k) list(
      id = paste0(id, "_", k), name = levels[k], level = 1L, order = k))
  }
  g
}

# the method tflspec reads from one method of `stats` named `name`
cov_method_read <- function(stats, name) {
  .ars_method_of(cov_method("M1", stats, name = name))$method
}

cov_specs_unmapped <- function(re) {
  u <- attr(tfl_ars_to_specs(re), "unmapped")
  paste(u$where, u$item, u$reason)
}

# ---- .ars_where(): an R condition as a WhereClause --------------------------

test_that(".ars_where: text that is not R has no WhereClause", {
  expect_null(.ars_where("AGE >", "ADSL"))
})

test_that(".ars_where: a condition in parentheses is read inside them", {
  w <- .ars_where("((AGE >= 65))", "ADSL")
  expect_identical(w$condition$comparator, "GE")
})

test_that(".ars_where: a bare name or constant has no WhereClause", {
  expect_null(.ars_where("SAFFL", "ADSL"))
  expect_null(.ars_where("TRUE", "ADSL"))
})

test_that(".ars_where: an AND with a bare name in it has none", {
  expect_null(.ars_where("AGE > 1 & SAFFL", "ADSL"))
})

test_that(".ars_where: an AND with a part that is no comparison has none", {
  expect_null(.ars_where("SEX == \"F\" & is.na(AGE)", "ADSL"))
})

test_that(".ars_where: NOT IN a variable's values, not constants, has none", {
  expect_null(.ars_where("!(SEX %in% c(RACE))", "ADSL"))
})

test_that(".ars_where: NOT of a comparison is a NOT compound expression", {
  w <- .ars_where("!(AGE < 18)", "ADSL")
  expect_identical(w$compoundExpression$logicalOperator, "NOT")
  expect_identical(w$compoundExpression$whereClauses[[1L]]$condition$
                     comparator, "LT")
})

test_that(".ars_where: NOT of what has no WhereClause has none", {
  expect_null(.ars_where("!(is.na(AGE))", "ADSL"))
})

test_that(".ars_where: == with several values is IN", {
  w <- .ars_where("SEX == c(\"F\", \"M\")", "ADSL")
  expect_identical(w$condition$comparator, "IN")
  expect_identical(w$condition$value, list("F", "M"))
})

test_that(".ars_where: > with several values has none", {
  expect_null(.ars_where("AGE > c(18, 65)", "ADSL"))
})

# ---- tfl_ars(): the spec columns and shapes ---------------------------------

test_that("tfl_ars: plain sheets, not a tfl_ard_spec, are read as one", {
  ars <- tfl_ars(cov_ars_raw(cov_ars_row(method = "continuous",
                                         variables = "AGE")))
  expect_s3_class(ars, "tfl_ars")
  expect_identical(cov_ars_ids(ars), "An_DM_X")
})

test_that("tfl_ars: a population with no dataset leaves its analyses without one", {
  spec <- cov_ars_spec(cov_ars_row(method = "continuous", variables = "AGE"),
                       populations = cov_ars_df(list(
                         population_id = "SAF", where = "SAFFL == \"Y\"")))
  ars <- tfl_ars(spec)
  expect_length(ars$analyses, 0L)
  expect_identical(tfl_ars_unmapped(ars)$item, "dataset")
  expect_identical(ars$analysisSets[[1L]]$condition$dataset, NA_character_)
})

test_that("tfl_ars: dataset_names renames a spec dataset", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "continuous",
                                          variables = "AGE")),
                 dataset_names = c(ADSL = "ADSL_X"))
  expect_identical(ars$analyses[[1L]]$dataset, "ADSL_X")
})

test_that("tfl_ars: a dataset's derivation is listed as unmapped", {
  spec <- cov_ars_spec(cov_ars_row(method = "continuous", variables = "AGE"),
                       datasets = cov_ars_df(list(dataset = "ADSL",
                                                  path = "adsl.rds",
                                                  derive = "mutate(X = 1)")))
  u <- tfl_ars_unmapped(tfl_ars(spec))
  expect_identical(u$item, "datasets$derive")
  expect_identical(u$where, "ADSL")
})

test_that("tfl_ars: a population condition with no WhereClause is listed", {
  spec <- cov_ars_spec(cov_ars_row(method = "continuous", variables = "AGE"),
                       populations = cov_ars_df(list(
                         population_id = "SAF", dataset = "ADSL",
                         where = "is.na(TRTSDT)")))
  ars <- tfl_ars(spec)
  u <- tfl_ars_unmapped(ars)
  expect_identical(u$item, "populations$where")
  expect_match(u$reason, "is.na(TRTSDT)", fixed = TRUE)
  expect_null(ars$analysisSets[[1L]]$condition)
})

test_that("tfl_ars: a code list's labels name the groups, its codes their condition", {
  ts <- list(variables = cov_ars_df(list(variable = "TRT01A",
                                         levels = "A | B")),
             codelists = cov_ars_df(
               list(output_id = "DM", variable = "TRT01A", value = "A",
                    label = "Arm Å", order = "1"),
               list(output_id = "DM", variable = "TRT01A", value = "B",
                    label = "Arm B", order = "2")))
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "continuous", by = "TRT01A",
                                          variables = "AGE")), ts)
  g <- ars$analysisGroupings[[1L]]$groups
  expect_identical(g[[1L]]$name, "Arm Å")
  expect_identical(g[[1L]]$condition$value, list("A"))
})

test_that("tfl_ars: one variable with other levels in another output is a second grouping", {
  ts <- list(variables = cov_ars_df(
    list(output_id = "DM", variable = "TRT01A", levels = "A | B"),
    list(output_id = "EF", variable = "TRT01A", levels = "B | A")))
  an <- rbind(cov_ars_row(method = "continuous", by = "TRT01A",
                          variables = "AGE"),
              cov_ars_df(list(output_id = "EF", analysis_id = "X",
                              population_id = "SAF", method = "continuous",
                              by = "TRT01A", variables = "AGE")))
  ars <- tfl_ars(cov_ars_spec(an), ts)
  expect_identical(vapply(ars$analysisGroupings, `[[`, "", "id"),
                   c("AG_ADSL_TRT01A", "AG_ADSL_TRT01A_2"))
})

test_that("tfl_ars: one method with other statistics is a second method", {
  an <- rbind(cov_ars_row(method = "continuous", variables = "AGE",
                          statistics = "mean"),
              cov_ars_df(list(output_id = "DM", analysis_id = "Y",
                              population_id = "SAF", method = "continuous",
                              variables = "AGE", statistics = "sd")))
  ars <- tfl_ars(cov_ars_spec(an))
  expect_identical(vapply(ars$methods, `[[`, "", "id"),
                   c("Mth_continuous", "Mth_continuous_2"))
})

test_that("tfl_ars: an analysis with no dataset and no population is listed and left out", {
  an <- cov_ars_df(list(output_id = "DM", analysis_id = "X",
                        method = "continuous", variables = "AGE"))
  ars <- tfl_ars(cov_ars_spec(an))
  u <- tfl_ars_unmapped(ars)
  expect_identical(u$item, "dataset")
  expect_length(ars$analyses, 0L)
})

test_that("tfl_ars: a row's own reason is the analysis's reason", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "continuous",
                                          variables = "AGE",
                                          reason = "data driven")))
  expect_identical(ars$analyses[[1L]]$reason$controlledTerm, "DATA DRIVEN")
  expect_identical(nrow(tfl_ars_unmapped(ars)), 0L)
})

test_that("tfl_ars: a purpose that is no CDISC term is listed", {
  u <- tfl_ars_unmapped(tfl_ars(cov_ars_spec(cov_ars_row(
    method = "continuous", variables = "AGE", purpose = "CURIOSITY"))))
  expect_identical(u$item, "purpose")
  expect_match(u$reason, "`CURIOSITY` is not a CDISC analysis purpose",
               fixed = TRUE)
})

test_that("tfl_ars: a reason that is no CDISC term is listed", {
  u <- tfl_ars_unmapped(tfl_ars(cov_ars_spec(cov_ars_row(
    method = "continuous", variables = "AGE", reason = "HUNCH"))))
  expect_identical(u$item, "reason")
  expect_match(u$reason, "`HUNCH` is not a CDISC analysis reason",
               fixed = TRUE)
})

test_that("tfl_ars: steps on the results after the method (post) are listed", {
  u <- tfl_ars_unmapped(tfl_ars(cov_ars_spec(cov_ars_row(
    method = "continuous", variables = "AGE",
    post = "dplyr::filter(TRUE)"))))
  expect_identical(u$item, "post")
})

test_that("tfl_ars: a pkg::function method with strata and a denominator still carries a call", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "mypkg::my_ard",
                                          by = "TRT01A", variables = "AGE",
                                          strata = "SEX",
                                          denominator = "population")))
  ct <- ars$methods[[1L]]$codeTemplate
  expect_identical(ct$context, "R")
  expect_match(ct$code, "mypkg::my_ard", fixed = TRUE)
})

test_that("tfl_ars: a categorical of an output's by variable is its subject count by group", {
  an <- cov_ars_df(
    list(output_id = "DM", analysis_id = "X", population_id = "SAF",
         method = "categorical", variables = "TRT01A"),
    list(output_id = "DM", analysis_id = "AGE", population_id = "SAF",
         method = "continuous", by = "TRT01A", variables = "AGE"))
  ars <- tfl_ars(cov_ars_spec(an))
  expect_identical(ars$analyses[[1L]]$methodId, "Mth_total_n")
  expect_identical(ars$analyses[[1L]]$name, "Number of subjects")
})

test_that("tfl_ars: a pkg::function method carries the call as its code", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "mypkg::my_ard",
                                          variables = "AGE")))
  ct <- ars$methods[[1L]]$codeTemplate
  expect_identical(ct$context, "R")
  expect_match(ct$code, "mypkg::my_ard(", fixed = TRUE)
})

test_that("tfl_ars: an overall analysis's label says (overall)", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "categorical",
                                          by = "TRT01A", variables = "SEX",
                                          overall = "Y", label = "Sex")))
  nm <- vapply(ars$analyses, `[[`, "", "name")
  expect_true("Sex" %in% nm)
  expect_true("Sex (overall)" %in% nm)
})

test_that("tfl_ars: the total_n keyword is the output's subject count by group", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "total_n", by = "TRT01A")))
  expect_identical(cov_ars_ids(ars), "An_DM_X")
  expect_identical(ars$methods[[1L]]$id, "Mth_total_n")
  expect_identical(unname(ars$analyses[[1L]]$orderedGroupings[[1L]]$groupingId),
                   "AG_ADSL_TRT01A")
})

test_that("tfl_ars: subjects with a record are counted by the grouping, with a BIGN added", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "subjects",
                                          dataset = "ADAE", by = "TRTA")))
  expect_identical(cov_ars_ids(ars), c("An_DM_BIGN", "An_DM_X"))
  expect_identical(ars$analyses[[2L]]$variable, "USUBJID")
})

test_that("tfl_ars: a test of counts of one variable is one analysis by the output's id", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "fisher", by = "TRT01A",
                                          variables = "SEX")))
  expect_identical(cov_ars_ids(ars), "An_DM_X")
})

test_that("tfl_ars: a test of counts with several variables is an analysis each", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "chisq", by = "TRT01A",
                                          variables = "SEX | RACE")))
  expect_identical(cov_ars_ids(ars), c("An_DM_X_SEX", "An_DM_X_RACE"))
  g <- ars$analyses[[1L]]$orderedGroupings
  expect_false(any(vapply(g, `[[`, NA, "resultsByGroup")))
})

test_that("tfl_ars: a custom analysis of several variables lists those beyond the first", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "custom",
                                          variables = "AGE | SEX",
                                          code = "my_fun(adsl)")))
  expect_identical(ars$analyses[[1L]]$variable, "AGE")
  u <- tfl_ars_unmapped(ars)
  expect_identical(u$item, "variables")
})

test_that("tfl_ars: an added subject count skips each BIGN id already taken", {
  an <- cov_ars_df(
    list(output_id = "DM", analysis_id = "BIGN", method = "continuous",
         population_id = "SAF", variables = "AGE"),
    list(output_id = "DM", analysis_id = "BIGN_2", method = "continuous",
         population_id = "SAF", variables = "WEIGHTBL"),
    list(output_id = "DM", analysis_id = "SEX", method = "categorical",
         population_id = "SAF", by = "TRT01A", variables = "SEX",
         denominator = "population"))
  ars <- tfl_ars(cov_ars_spec(an))
  expect_true("An_DM_BIGN_3" %in% cov_ars_ids(ars))
})

test_that("tfl_ars: a table spec's layout is listed as the table's look", {
  ts <- tfl_table_spec(list(
    layout = cov_ars_df(list(output_id = "DM", pages_max_rows = "20"))))
  u <- tfl_ars_unmapped(tfl_ars(cov_ars_spec(cov_ars_row(
    method = "continuous", variables = "AGE")), ts))
  expect_identical(u$item, "layout")
  expect_match(u$reason, "the table's look", fixed = TRUE)
})

test_that("tfl_ars: a title line with no text is not a display subsection", {
  ts <- tfl_table_spec(list(
    report = cov_ars_df(list(output_id = "DM", type = "table")),
    titles = cov_ars_df(list(output_id = "DM", line = "1", center = NA),
                        list(output_id = "DM", line = "2",
                             center = "Table 1"))))
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "continuous",
                                          variables = "AGE")), ts)
  sec <- ars$outputs[[1L]]$displays[[1L]]$display$displaySections
  expect_length(sec[[1L]]$orderedSubSections, 1L)
  expect_identical(sec[[1L]]$orderedSubSections[[1L]]$subSection$text,
                   "Table 1")
})

test_that("print.tfl_ars: counts the parts and points at what is unmapped", {
  ars <- tfl_ars(cov_ars_spec(cov_ars_row(method = "continuous",
                                          variables = "AGE",
                                          post = "head()")))
  out <- capture.output(res <- print(ars))
  expect_identical(res, ars)
  expect_match(out[1L], "<tfl_ars> STUDY01 (CDISC ARS v1.0, profile cdisc)",
               fixed = TRUE)
  expect_match(out[2L], "1 output(s), 1 analyses", fixed = TRUE)
  expect_match(out[3L], "1 item(s) not in the ARS", fixed = TRUE)
})

# ---- tfl_read_ars_json() and WhereClauses as R ------------------------------

test_that("tfl_read_ars_json: JSON with no id or analyses stops", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  writeLines("{\"name\": \"not a reporting event\"}", f)
  expect_error(tfl_read_ars_json(f), "is not an ARS reporting event")
})

test_that(".ars_r_cond: a comparator with no R form is NA", {
  w <- list(condition = list(dataset = "ADSL", variable = "AGE",
                             comparator = "LIKE", value = list("6%")))
  expect_identical(.ars_r_cond(w), NA_character_)
})

test_that(".ars_r_cond: a clause with neither condition nor compound is NA", {
  expect_identical(.ars_r_cond(list(level = 1L, order = 1L)), NA_character_)
})

test_that(".ars_r_cond: a compound with a part that has no R form is NA", {
  w <- list(compoundExpression = list(logicalOperator = "AND", whereClauses =
    list(list(condition = list(variable = "AGE", comparator = "GT",
                               value = list("1"))),
         list(condition = list(variable = "SEX", comparator = "LIKE",
                               value = list("F"))))))
  expect_identical(.ars_r_cond(w), NA_character_)
})

test_that(".ars_r_cond: a NOT compound is !( )", {
  w <- list(compoundExpression = list(logicalOperator = "NOT", whereClauses =
    list(list(condition = list(variable = "AGE", comparator = "LT",
                               value = list("18"))))))
  expect_identical(.ars_r_cond(w), "!(AGE < 18)")
})

test_that(".ars_cond_datasets: a compound names each clause's dataset", {
  w <- .ars_where("AGE > 1 & SEX == \"F\"", "ADSL")
  expect_identical(.ars_cond_datasets(w), c("ADSL", "ADSL"))
})

# ---- .ars_method_of(): a method read from what it computes ------------------

test_that(".ars_method_of: a tflspec id with no keyword reads a pkg::fn code template", {
  m <- cov_method("Mth_ownthing", "result", code = "mypkg::my_ard(data)")
  expect_identical(.ars_method_of(m)$method, "mypkg::my_ard")
})

test_that(".ars_method_of: a p-value named Fisher is fisher", {
  expect_identical(cov_method_read("p-value", "Fisher exact test"), "fisher")
})

test_that(".ars_method_of: a p-value named chi-square is chisq", {
  expect_identical(cov_method_read("p-value", "Chi-square test"), "chisq")
})

test_that(".ars_method_of: a p-value named rank sum is wilcox", {
  expect_identical(cov_method_read("p-value", "Rank sum test"), "wilcox")
})

test_that(".ars_method_of: a p-value named t-test is ttest", {
  expect_identical(cov_method_read("p-value", "Two-sample t-test"), "ttest")
})

test_that(".ars_method_of: a p-value of an unknown test is no method", {
  expect_identical(cov_method_read("p-value", "ANOVA"), NA_character_)
})

test_that(".ars_method_of: an estimate with its CI is proportion_ci", {
  expect_identical(cov_method_read(c("estimate", "conf.low", "conf.high"),
                                   "Proportion"), "proportion_ci")
})

test_that(".ars_method_of: a count with a percentage is categorical", {
  expect_identical(cov_method_read(c("n", "pct"), "Frequency"),
                   "categorical")
})

test_that(".ars_method_of: counts only are total_n", {
  expect_identical(cov_method_read("count", "Big N"), "total_n")
})

test_that(".ars_method_of: operations tflspec does not know are no method", {
  expect_identical(cov_method_read("geometric thing", "Something"),
                   NA_character_)
})

# ---- tfl_ars_to_specs(): other reporting events ------------------------------

test_that("tfl_ars_to_specs: what is not a tfl_ars stops", {
  expect_error(tfl_ars_to_specs(list(id = "RE1")), "must be a tfl_ars()",
               fixed = TRUE)
})

test_that("tfl_ars_to_specs: the subject key is the most used SUBJ variable when no USUBJID", {
  re <- cov_re(list(cov_analysis("A1", "M1", variable = "SUBJID",
                                 groupings = "G1")),
               list(cov_method("M1", c("n", "pct"))),
               groupings = list(cov_grouping("G1", "SEX")))
  sp <- tfl_ars_to_specs(re)
  expect_identical(sp$ard$study$value[sp$ard$study$key == "id"], "SUBJID")
  expect_identical(sp$ard$analyses$variables, "SEX")
})

test_that("tfl_ars_to_specs: an analysis set's condition with no R form is listed", {
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))),
               sets = list(list(id = "SAF", name = "SAF", level = 1L,
                                order = 1L,
                                condition = list(dataset = "ADSL",
                                                 variable = "SAFFL",
                                                 comparator = "LIKE",
                                                 value = list("Y")))))
  expect_true(any(grepl("SAF analysisSets a condition with no R form",
                        cov_specs_unmapped(re), fixed = TRUE)))
})

test_that("tfl_ars_to_specs: an analysis set over two datasets is listed", {
  set <- list(id = "SAF", name = "SAF", level = 1L, order = 1L,
              compoundExpression = list(logicalOperator = "AND",
                                        whereClauses = list(
    list(level = 2L, order = 1L,
         condition = list(dataset = "ADSL", variable = "SAFFL",
                          comparator = "EQ", value = list("Y"))),
    list(level = 2L, order = 2L,
         condition = list(dataset = "ADAE", variable = "AESER",
                          comparator = "EQ", value = list("Y"))))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), sets = list(set))
  expect_true(any(grepl("conditions on several datasets (ADSL, ADAE)",
                        cov_specs_unmapped(re), fixed = TRUE)))
})

test_that("tfl_ars_to_specs: outputs nested under a heading item are found", {
  items <- list(list(name = "Section", level = 1L, order = 1L,
                     sublist = list(listItems = list(list(
                       name = "T1", level = 2L, order = 1L, outputId = "T1",
                       sublist = list(listItems = list(list(
                         name = "A1", level = 3L, order = 1L,
                         analysisId = "A1"))))))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), items = items)
  sp <- tfl_ars_to_specs(re)
  expect_identical(sp$ard$analyses$output_id, "T1")
})

test_that("tfl_ars_to_specs: analyses in no output go under UNLISTED", {
  items <- list(list(name = "T1", level = 1L, order = 1L, outputId = "T1",
                     sublist = list(listItems = list(list(
                       name = "A1", level = 2L, order = 1L,
                       analysisId = "A1")))))
  re <- cov_re(list(cov_analysis("A1", "M1"), cov_analysis("A2", "M1")),
               list(cov_method("M1", c("mean", "sd"))), items = items)
  sp <- tfl_ars_to_specs(re)
  expect_identical(sp$ard$analyses$output_id, c("T1", "UNLISTED"))
  expect_identical(sp$report$report$output_id, "T1")
  expect_true(any(grepl("1 analyses in no output", cov_specs_unmapped(re),
                        fixed = TRUE)))
})

test_that("tfl_ars_to_specs: a list item naming no analysis is passed over", {
  items <- list(list(name = "T1", level = 1L, order = 1L, outputId = "T1",
                     sublist = list(listItems = list(
                       list(name = "Gone", level = 2L, order = 1L,
                            analysisId = "GONE"),
                       list(name = "A1", level = 2L, order = 2L,
                            analysisId = "A1")))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), items = items)
  sp <- tfl_ars_to_specs(re)
  expect_identical(sp$ard$analyses$analysis_id, "A1")
})

test_that("tfl_ars_to_specs: a count of subjects in record data by one grouping is subjects", {
  re <- cov_re(list(cov_analysis("A1", "M1", variable = "USUBJID",
                                 dataset = "ADAE", groupings = "G1")),
               list(cov_method("M1", c("n", "pct"))),
               groupings = list(cov_grouping("G1", "TRTA", ds = "ADAE")))
  an <- tfl_ars_to_specs(re)$ard$analyses
  expect_identical(an$method, "subjects")
  expect_identical(an$by, "TRTA")
  expect_true(is.na(an$variables))
})

test_that("tfl_ars_to_specs: a count of subjects in record data by two groupings is a hierarchy", {
  re <- cov_re(list(cov_analysis("A1", "M1", variable = "USUBJID",
                                 dataset = "ADAE",
                                 groupings = c("G1", "G2"))),
               list(cov_method("M1", c("n", "pct"))),
               groupings = list(cov_grouping("G1", "TRTA", ds = "ADAE"),
                                cov_grouping("G2", "AEDECOD", ds = "ADAE")))
  an <- tfl_ars_to_specs(re)$ard$analyses
  expect_identical(an$method, "hierarchical")
  expect_identical(an$by, "TRTA")
  expect_identical(an$variables, "AEDECOD")
})

test_that("tfl_ars_to_specs: a grouping without results by group is listed", {
  a <- cov_analysis("A1", "M1", groupings = "G1")
  a$orderedGroupings[[1L]]$resultsByGroup <- FALSE
  re <- cov_re(list(a), list(cov_method("M1", c("mean", "sd"))),
               groupings = list(cov_grouping("G1", "TRT01A")))
  expect_true(any(grepl("a grouping without results by group",
                        cov_specs_unmapped(re), fixed = TRUE)))
})

test_that("tfl_ars_to_specs: a data subset with no R form is listed", {
  a <- cov_analysis("A1", "M1", dataSubsetId = "DS1")
  re <- cov_re(list(a), list(cov_method("M1", c("mean", "sd"))),
               subsets = list(list(id = "DS1", name = "odd", level = 1L,
                                   order = 1L,
                                   condition = list(dataset = "ADSL",
                                                    variable = "AGE",
                                                    comparator = "LIKE",
                                                    value = list("1")))))
  expect_true(any(grepl("data subset DS1 has no R form",
                        cov_specs_unmapped(re), fixed = TRUE)))
})

test_that("tfl_ars_to_specs: a hierarchy's any-event row listed last still reads over_variables", {
  an <- cov_ars_row(method = "hierarchical", dataset = "ADAE", by = "TRTA",
                    variables = "AEBODSYS | AEDECOD",
                    args = "over_variables = TRUE")
  ars <- tfl_ars(cov_ars_spec(an))
  it <- ars$mainListOfContents$contentsList$listItems[[1L]]$sublist$listItems
  ids <- vapply(it, `[[`, "", "analysisId")
  # the any-event row after the depths, as another tool may list them
  last <- c(setdiff(seq_along(ids), grep("_ANY$", ids)), grep("_ANY$", ids))
  ars$mainListOfContents$contentsList$listItems[[1L]]$sublist$listItems <-
    it[last]
  back <- tfl_ars_to_specs(ars)$ard$analyses
  back <- back[back$method == "hierarchical", , drop = FALSE]
  expect_identical(back$args, "over_variables = TRUE")
  expect_identical(back$variables, "AEBODSYS | AEDECOD")
})

test_that("tfl_ars_to_specs: a hierarchy's label comes back without its depth", {
  an <- cov_ars_row(method = "hierarchical", dataset = "ADAE", by = "TRTA",
                    variables = "AEDECOD", label = "TEAE")
  back <- tfl_ars_to_specs(tfl_ars(cov_ars_spec(an)))$ard$analyses
  expect_identical(back$label[back$method == "hierarchical"], "TEAE")
})

test_that("tfl_ars_to_specs: two analyses with one id in an output are made unique", {
  re <- cov_re(list(cov_analysis("X", "M1"), cov_analysis("An_T1_X", "M1")),
               list(cov_method("M1", c("mean", "sd"))))
  an <- tfl_ars_to_specs(re)$ard$analyses
  expect_identical(an$analysis_id, c("X", "X_1"))
})

test_that("tfl_ars_to_specs: an Output with no displays has no titles", {
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), outputs = list(list(
                 id = "T1", name = "T1", version = 1L, displays = list())))
  sp <- tfl_ars_to_specs(re)
  expect_identical(sp$report$report$output_id, "T1")
  expect_identical(NROW(sp$report$titles), 0L)
})

test_that("tfl_ars_to_specs: a listed output with no Output is a report row with no file", {
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), outputs = list(list(
                 id = "OTHER", name = "OTHER", version = 1L,
                 displays = list())))
  rp <- tfl_ars_to_specs(re)$report$report
  expect_identical(rp$output_id, "T1")
  expect_true(is.na(rp$file))
})

test_that("tfl_ars_to_specs: several displays are listed, the first read", {
  disp <- function(id, title) list(order = 1L, display = list(
    id = id, name = id, displaySections = list(list(
      sectionType = "Title", orderedSubSections = list(list(
        order = 1L, subSection = list(id = paste0(id, "_T"),
                                      text = title)))))))
  out <- list(id = "T1", name = "T1", version = 1L,
              displays = list(disp("D1", "First"), disp("D2", "Second")))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), outputs = list(out))
  sp <- tfl_ars_to_specs(re)
  expect_identical(sp$report$titles$center, "First")
  expect_true(any(grepl("T1 displays several displays",
                        cov_specs_unmapped(re), fixed = TRUE)))
})

test_that("tfl_ars_to_specs: a subsection with no text nor a global one is passed over", {
  out <- list(id = "T1", name = "T1", version = 1L, displays = list(list(
    order = 1L, display = list(id = "D1", name = "D1", displaySections = list(
      list(sectionType = "Footnote", orderedSubSections = list(
        list(order = 1L, subSectionId = "NOWHERE"),
        list(order = 2L, subSection = list(id = "F2", text = "Note.")))))))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), outputs = list(out))
  fn <- tfl_ars_to_specs(re)$report$footnotes
  expect_identical(fn$left, "Note.")
  expect_identical(fn$line, "1")
})

test_that("tfl_ars_to_specs: a data-driven grouping gives no table spec levels", {
  re <- cov_re(list(cov_analysis("A1", "M1", groupings = c("G1", "G2"))),
               list(cov_method("M1", c("mean", "sd"))),
               groupings = list(cov_grouping("G1", "TRT01A"),
                                cov_grouping("G2", "SEX",
                                             levels = c("F", "M"))))
  v <- tfl_ars_to_specs(re, table = TRUE)$table$variables
  expect_identical(v$variable, "SEX")
  expect_identical(v$levels, "F | M")
})

# ---- tfl_write_ars_json() and tfl_check_ars() --------------------------------

cov_ok_re <- function() {
  cov_re(list(cov_analysis("A1", "M1")),
         list(cov_method("M1", c("mean", "sd"))))
}

cov_problems <- function(re) {
  ck <- tfl_check_ars(re, schema = FALSE)
  paste(ck$part, ck$field, ck$problem)
}

test_that(".ars_plain: a factor is written as its labels", {
  expect_identical(.ars_plain(factor(c("b", "a"))), c("b", "a"))
})

test_that("tfl_write_ars_json: what is not a tfl_ars stops", {
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  expect_error(tfl_write_ars_json(list(id = "RE1"), f),
               "must be a tfl_ars()", fixed = TRUE)
  expect_false(file.exists(f))
})

test_that("tfl_write_ars_json: a path that is not one .json file stops", {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f), add = TRUE)
  expect_error(tfl_write_ars_json(cov_ok_re(), f), "must be one .json file",
               fixed = TRUE)
  expect_error(tfl_write_ars_json(cov_ok_re(), c("a.json", "b.json")),
               "must be one .json file", fixed = TRUE)
  expect_false(file.exists(f))
})

test_that("tfl_check_ars: a hand-made reporting event that holds together has no problem", {
  expect_identical(nrow(tfl_check_ars(cov_ok_re(), schema = FALSE)), 0L)
})

test_that("tfl_check_ars: a path that is not there stops", {
  expect_error(tfl_check_ars(file.path(tempdir(), "no-such-ars.json")))
})

test_that("tfl_check_ars: what is neither a tfl_ars nor a path stops", {
  expect_error(tfl_check_ars(list(id = "RE1")),
               "must be a tfl_ars() or the path", fixed = TRUE)
})

test_that("tfl_check_ars: an id used twice in a class is named", {
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd")),
                    cov_method("M1", c("mean", "sd"))))
  expect_true("AnalysisMethod M1 id used more than once" %in%
                cov_problems(re))
})

test_that("tfl_check_ars: a subClauseId naming no analysis set is named", {
  set <- list(id = "SAF", name = "SAF", level = 1L, order = 1L,
              compoundExpression = list(logicalOperator = "AND",
                                        whereClauses = list(
    list(level = 2L, order = 1L, subClauseId = "NOSET"))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), sets = list(set))
  expect_true(any(grepl("subClauseId no analysis set NOSET",
                        cov_problems(re), fixed = TRUE)))
})

test_that("tfl_check_ars: a subClauseId naming no data subset is named", {
  sub <- list(id = "DS1", name = "DS1", level = 1L, order = 1L,
              compoundExpression = list(logicalOperator = "AND",
                                        whereClauses = list(
    list(level = 2L, order = 1L, subClauseId = "NOSUB"))))
  re <- cov_re(list(cov_analysis("A1", "M1", dataSubsetId = "DS1")),
               list(cov_method("M1", c("mean", "sd"))), subsets = list(sub))
  expect_true(any(grepl("subClauseId no data subset NOSUB",
                        cov_problems(re), fixed = TRUE)))
})

test_that("tfl_check_ars: a nested compound's sub-clauses are checked too", {
  inner <- list(logicalOperator = "OR", whereClauses = list(
    list(order = 1L, condition = list(dataset = "ADSL", variable = "SEX",
                                      comparator = "EQ", value = list("F")))))
  set <- list(id = "SAF", name = "SAF", level = 1L, order = 1L,
              compoundExpression = list(logicalOperator = "AND",
                                        whereClauses = list(
    list(level = 2L, order = 1L, compoundExpression = inner))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), sets = list(set))
  p <- cov_problems(re)
  expect_true(any(grepl("whereClauses[1] whereClauses[1] level required",
                        p, fixed = TRUE)))
})

test_that("tfl_check_ars: an operation relationship naming no operation is named", {
  m <- cov_method("M1", c("n", "pct"))
  m$operations[[2L]]$referencedOperationRelationships <- list(list(
    id = "R1", referencedOperationRole = list(controlledTerm = "NUMERATOR"),
    operationId = "NO_OP"))
  re <- cov_re(list(cov_analysis("A1", "M1")), list(m))
  expect_true(any(grepl("names operation NO_OP, which no method has",
                        cov_problems(re), fixed = TRUE)))
})

test_that("tfl_check_ars: a sponsor's purpose term is accepted", {
  a <- cov_analysis("A1", "M1")
  a$purpose <- list(sponsorTermId = "SP_PURPOSE_1")
  re <- cov_re(list(a), list(cov_method("M1", c("mean", "sd"))))
  expect_identical(nrow(tfl_check_ars(re, schema = FALSE)), 0L)
})

test_that("tfl_check_ars: a purpose that is no CDISC term is named", {
  a <- cov_analysis("A1", "M1")
  a$purpose <- list(controlledTerm = "CURIOSITY")
  re <- cov_re(list(a), list(cov_method("M1", c("mean", "sd"))))
  expect_true("Analysis A1 purpose `CURIOSITY` is not a CDISC analysis purpose"
              %in% cov_problems(re))
})

test_that("tfl_check_ars: an analysisSetId naming no analysis set is named", {
  a <- cov_analysis("A1", "M1")
  a$analysisSetId <- "ITT"
  re <- cov_re(list(a), list(cov_method("M1", c("mean", "sd"))))
  expect_true("Analysis A1 analysisSetId no analysis set ITT" %in%
                cov_problems(re))
})

test_that("tfl_check_ars: a dataSubsetId naming no data subset is named", {
  re <- cov_re(list(cov_analysis("A1", "M1", dataSubsetId = "DS9")),
               list(cov_method("M1", c("mean", "sd"))))
  expect_true("Analysis A1 dataSubsetId no data subset DS9" %in%
                cov_problems(re))
})

test_that("tfl_check_ars: a groupingId naming no grouping is named", {
  re <- cov_re(list(cov_analysis("A1", "M1", groupings = "G9")),
               list(cov_method("M1", c("mean", "sd"))))
  expect_true("Analysis A1 orderedGroupings no grouping G9" %in%
                cov_problems(re))
})

test_that("tfl_check_ars: referenced analysis operations naming nothing are named", {
  a <- cov_analysis("A1", "M1")
  a$referencedAnalysisOperations <- list(list(
    referencedOperationRelationshipId = "NO_REL", analysisId = "NO_AN"))
  re <- cov_re(list(a), list(cov_method("M1", c("n", "pct"))))
  p <- cov_problems(re)
  expect_true(paste("Analysis A1 referencedAnalysisOperations",
                    "no operation relationship NO_REL") %in% p)
  expect_true("Analysis A1 referencedAnalysisOperations no analysis NO_AN" %in%
                p)
})

test_that("tfl_check_ars: a list of contents naming no output or analysis is named", {
  items <- list(list(name = "T9", level = 1L, order = 1L, outputId = "T9",
                     sublist = list(listItems = list(list(
                       name = "Gone", level = 2L, order = 1L,
                       analysisId = "GONE")))))
  re <- cov_re(list(cov_analysis("A1", "M1")),
               list(cov_method("M1", c("mean", "sd"))), items = items)
  p <- cov_problems(re)
  expect_true("ListOfContents outputId no output T9" %in% p)
  expect_true("ListOfContents analysisId no analysis GONE" %in% p)
})

test_that("tfl_check_ars: what the JSON Schema rejects is named by its path", {
  skip_if_not_installed("jsonvalidate")
  re <- cov_ok_re()
  re$analyses[[1L]]$version <- "one"
  ck <- tfl_check_ars(re)
  expect_true(any(startsWith(ck$part, "schema ")))
})
