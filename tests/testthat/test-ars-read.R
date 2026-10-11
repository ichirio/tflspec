read_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

read_spec <- function() {
  s <- list(
    study = read_df(list(key = "id", value = "USUBJID"),
                    list(key = "study_id", value = "PILOT01")),
    datasets = read_df(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                       list(dataset = "ADAE", path = "adam/ADAE.rds")),
    populations = read_df(list(population_id = "SAF", dataset = "ADSL",
                               where = "SAFFL == \"Y\"")),
    analyses = read_df(
      list(output_id = "DM", analysis_id = "GROUPN", method = "categorical",
           population_id = "SAF", variables = "TRT01A"),
      list(output_id = "DM", analysis_id = "AGE", method = "continuous",
           population_id = "SAF", by = "TRT01A", variables = "AGE",
           statistics = "N | mean | sd"),
      list(output_id = "DM", analysis_id = "CAT", method = "categorical",
           population_id = "SAF", by = "TRT01A",
           variables = "AGEGR1 | SEX | RACE", label = "Demography"),
      list(output_id = "DM", analysis_id = "PCHI", method = "chisq",
           population_id = "SAF", by = "TRT01A", variables = "SEX"),
      list(output_id = "AE", analysis_id = "TEAE", method = "hierarchical",
           dataset = "ADAE", population_id = "SAF",
           where = "TRTEMFL == \"Y\" & AESEV %in% c(\"MILD\", \"MODERATE\")",
           by = "TRTA", variables = "AEBODSYS | AEDECOD",
           args = "over_variables = TRUE"),
      list(output_id = "CI", analysis_id = "GROUPN", method = "categorical",
           population_id = "SAF", variables = "TRT01A"),
      list(output_id = "CI", analysis_id = "SEX", method = "proportion_ci",
           population_id = "SAF", by = "TRT01A", variables = "SEX",
           args = "method = \"wilson\"")))
  s$analyses$purpose <- "SECONDARY OUTCOME MEASURE"
  s$analyses$purpose[s$analyses$output_id == "CI"] <-
    "EXPLORATORY OUTCOME MEASURE"
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) {
      if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

through_json <- function(ars) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  tfl_write_ars_json(ars, f)
  tfl_read_ars_json(f)
}

key_cols <- c("output_id", "analysis_id", "label", "method", "dataset",
              "population_id", "by", "variables", "statistics", "args",
              "purpose")

test_that("a WhereClause comes back as the R it was written from", {
  for (w in c("SAFFL == \"Y\"", "AGE >= 65",
              "TRTEMFL == \"Y\" & AESEV %in% c(\"MILD\", \"MODERATE\")",
              "!(AESEV %in% c(\"SEVERE\"))",
              "SEX == \"F\" & (RACE == \"WHITE\" | RACE == \"ASIAN\")")) {
    back <- .ars_r_cond(.ars_where(w, "ADSL"))
    expect_identical(.ars_where(back, "ADSL"), .ars_where(w, "ADSL"),
                     label = w)
  }
  expect_identical(.ars_r_cond(.ars_where("AGE >= 65", "ADSL")), "AGE >= 65")
})

test_that("tfl_ars()'s own JSON comes back as the spec it was written from", {
  sp <- read_spec()
  rs <- list(
    report = read_df(list(output_id = "DM", file = "t14_1_1.rtf")),
    titles = read_df(list(output_id = "DM", line = "1", center = "Table 14.1.1"),
                     list(output_id = "DM", line = "2",
                          center = "Demographics")),
    footnotes = read_df(list(output_id = "AE", line = "1",
                             left = "A subject is counted once.")))
  back <- tfl_ars_to_specs(through_json(tfl_ars(sp, report_spec = rs)))
  a <- back$ard$analyses
  expect_identical(a[key_cols], as.data.frame(sp$analyses)[key_cols],
                   ignore_attr = TRUE)
  expect_identical(.ars_where(a$where[5L], "ADAE"),
                   .ars_where(sp$analyses$where[5L], "ADAE"))
  expect_identical(back$ard$populations$where, "SAFFL == \"Y\"")
  expect_identical(back$ard$study$value[back$ard$study$key == "study_id"],
                   "PILOT01")
  # the report spec: outputs in order, file, titles, footnotes
  r <- back$report
  expect_identical(r$report$output_id, c("DM", "AE", "CI"))
  expect_identical(r$report$file[1L], "t14_1_1.rtf")
  expect_identical(r$titles$center, c("Table 14.1.1", "Demographics"))
  expect_identical(r$footnotes$left, "A subject is counted once.")
  # ARS has no ADaM path
  expect_true(all(is.na(back$ard$datasets$path)))
  expect_true("path" %in% attr(back, "unmapped")$item)
})

test_that("the siera profile's JSON comes back too", {
  back <- tfl_ars_to_specs(through_json(tfl_ars(read_spec(),
                                                profile = "siera")))
  a <- back$ard$analyses
  expect_identical(a$method, c("categorical", "continuous", "categorical",
                               "chisq", "hierarchical", "categorical",
                               "proportion_ci"))
  ae <- a[a$method == "hierarchical", ]
  expect_identical(ae$variables, "AEBODSYS | AEDECOD")
  expect_identical(ae$by, "TRTA")
  expect_identical(ae$args, "over_variables = TRUE")
  ci <- a[a$method == "proportion_ci", ]
  expect_identical(ci$variables, "SEX")
  expect_identical(ci$args, "method = \"wilson\"")
})

test_that("the ARD from the specs read back is the ARD of the spec", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  sp <- read_spec()
  back <- tfl_ars_to_specs(through_json(tfl_ars(sp)))$ard
  back$datasets$path <- sp$datasets$path[match(back$datasets$dataset,
                                               sp$datasets$dataset)]
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "adam"))
  adsl <- as.data.frame(cards::ADSL)
  adsl$TRTA <- adsl$TRT01A
  saveRDS(adsl, file.path(dir, "adam", "ADSL.rds"))
  saveRDS(as.data.frame(cards::ADAE), file.path(dir, "adam", "ADAE.rds"))
  a <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  b <- suppressMessages(tfl_build_ard(back, dir = dir, save = FALSE))
  expect_identical(nrow(b), nrow(a))
  expect_equal(b$stat, a$stat)
  expect_identical(b$stat_name, a$stat_name)
})

test_that("CDISC's own example reads as specs, and what has no place is said", {
  ars <- tfl_read_ars_json(test_path("fixtures", "ars-csd-demographics.json"))
  expect_s3_class(ars, "tfl_ars")
  expect_identical(attr(ars, "profile"), "cdisc")
  sp <- tfl_ars_to_specs(ars, table = TRUE)
  a <- sp$ard$analyses
  m <- stats::setNames(a$method, a$analysis_id)
  expect_identical(m[["An01_05_SAF_Summ_ByTrt"]], "categorical")
  expect_identical(m[["An03_01_Age_Summ_ByTrt"]], "continuous")
  expect_identical(m[["An03_02_AgeGrp_Summ_ByTrt"]], "categorical")
  expect_identical(m[["An03_02_AgeGrp_Comp_ByTrt"]], "chisq")
  sex <- a[a$analysis_id == "An03_03_Sex_Summ_ByTrt", ]
  expect_identical(c(sex$by, sex$variables), c("TRT01A", "SEX"))
  # its summary's n is cards' N; Q1 / Q3 are p25 / p75
  age <- a[a$analysis_id == "An03_01_Age_Summ_ByTrt", ]
  expect_true(is.na(age$statistics) ||
                all(.split_bar(age$statistics) %in%
                      tfl_ard_statistics("continuous")$statistic))
  # an ANOVA has no tflspec keyword: not a row, and said
  expect_false("An03_01_Age_Comp_ByTrt" %in% a$analysis_id)
  un <- attr(sp, "unmapped")
  expect_true(any(grepl("An03_01_Age_Comp_ByTrt", un$where)))
  expect_identical(sp$ard$populations$where, "SAFFL == \"Y\"")
  # the report: titles with the global ones, the header from the global one
  expect_true("Summary of Demographics" %in% sp$report$titles$center)
  expect_true("Study - CDISC 360" %in% sp$report$header$left)
  # the table spec: the listed groups as levels
  v <- sp$table$variables
  expect_identical(v$levels[v$variable == "TRT01A"],
                   "Placebo | Xanomeline Low Dose | Xanomeline High Dose")
  expect_identical(v$label[v$variable == "TRT01A"], "Treatment")
})

test_that("an ARS that does not hold together is refused", {
  f <- withr::local_tempfile(fileext = ".json")
  j <- jsonlite::fromJSON(test_path("fixtures", "ars-csd-demographics.json"),
                          simplifyVector = FALSE)
  j$analyses[[1L]]$methodId <- "nope"
  writeLines(jsonlite::toJSON(j, auto_unbox = TRUE), f)
  expect_error(tfl_read_ars_json(f), "nope")
  expect_error(tfl_read_ars_json(tempfile(fileext = ".json")), "existing")
})
