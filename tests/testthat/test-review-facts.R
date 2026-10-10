# tfl_data_facts() / tfl_ard_facts(): what the review keeps of the data and
# of an ARD -- small, no records.

test_that("a dataset's facts: classes, labels, values up to max_levels", {
  d <- data.frame(USUBJID = c("1", "2", "2"), AGE = c(50, 61, 61),
                  SEX = factor(c("F", "M", "M")), ID = c("a", "b", "c"),
                  DT = as.Date("2020-01-01") + 0:2, FL = c(TRUE, FALSE, NA),
                  stringsAsFactors = FALSE)
  attr(d$AGE, "label") <- "Age"
  f <- tfl_data_facts(list(adsl = d), max_levels = 2L)
  expect_s3_class(f, "tfl_data_facts")
  expect_named(f$datasets, "ADSL")
  cols <- f$datasets$ADSL$columns
  expect_identical(cols$class, c("character", "numeric", "factor", "character",
                                 "Date", "logical"))
  expect_identical(cols$label[2], "Age")
  expect_identical(cols$n_distinct, c(2L, 2L, 2L, 3L, 3L, 2L))
  expect_identical(cols$values[[3]], c("F", "M"))
  expect_null(cols$values[[4]])          # three values, more than max_levels
  expect_null(cols$values[[2]])          # numeric: no values kept
  expect_identical(f$datasets$ADSL$n, 3L)
  expect_identical(f$datasets$ADSL$n_subjects, 2L)
  expect_output(print(f), "ADSL")
})

test_that("an analysis set's subjects and flag, and conditions that fail say why", {
  s <- .rv_study()
  s$data$ADSL$SAFFL[8] <- "Yes"
  f <- tfl_data_facts(s$data, populations = s$ard,
                      conditions = data.frame(dataset = c("ADSL", "ADSL", "ADAE"),
                                              where = c("NOPE == 1", "AGE >", "AGE > 200")))
  expect_identical(f$populations$SAF$n_subjects, 6L)
  expect_identical(f$populations$SAF$flag, "SAFFL")
  expect_identical(f$populations$SAF$flag_values, c("N", "Y", "Yes"))
  cd <- f$conditions
  expect_identical(cd$error[cd$where %in% "NOPE == 1"], "no column NOPE")
  expect_identical(cd$error[cd$where %in% "AGE >"], "it does not read as R")
  expect_match(cd$error[cd$where %in% "AGE > 200"], "no column AGE")
  # the analysis data's: TEAEs of the safety set
  te <- cd[cd$dataset == "ADAE" & cd$where %in% 'TRTEMFL == "Y"', ]
  expect_identical(te$population_id, "SAF")
  expect_identical(te$n_rows, 4L)
  expect_identical(te$n_subjects, 3L)
  expect_identical(te$values[[1]]$AEDECOD, c("NAUSEA", "RASH", "VOMITING"))
  # a function the condition does not name with its package is not counted
  f2 <- tfl_data_facts(s$data, conditions = data.frame(dataset = "ADSL",
                                                       where = "between(AGE, 1, 2)"))
  expect_true(is.na(f2$conditions$error) && is.na(f2$conditions$n_rows))
})

test_that("tfl_data_facts() takes only named data frames", {
  expect_error(tfl_data_facts(data.frame(a = 1)), "named list of data frames")
  expect_error(tfl_data_facts(list(1)), "named list of data frames")
})

test_that("an ARD's facts are what tfl_check_ard() sees", {
  skip_if_not_installed("cards")
  ard <- cards::ard_summary(cards::ADSL, by = ARM, variables = AGE)
  f <- tfl_ard_facts(ard)
  expect_identical(names(f$groups), "ARM")
  expect_setequal(f$groups$ARM, unique(as.character(cards::ADSL$ARM)))
  expect_true(all(c("mean", "sd", "N") %in% f$variables$AGE$stats))
  expect_identical(f$stats, unique(ard$stat_name))
  expect_identical(nrow(f$conditions), 0L)
  # the same three rules: the statistics a template reads that are not there
  spec <- tfl_table_spec(tables = data.frame(output_id = "T-1", cols = "ARM"),
                         cells = data.frame(output_id = "T-1", variable = "AGE",
                                            template = "{mean} ({gmean})"))
  r <- tfl_review_spec(spec, facts = list(ard = list(`T-1` = f)))
  ck <- tfl_check_ard(ard, spec, "T-1")
  expect_identical(r$rule, "T06")
  expect_match(r$message, "{gmean}", fixed = TRUE)
  expect_match(ck$message, "{gmean}", fixed = TRUE)
})
