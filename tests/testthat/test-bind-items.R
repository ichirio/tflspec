# bind_items(): one report's items into one ARD (study helper)

bi_env <- function() {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  e
}

bi_data <- function() {
  data.frame(ARM = c("A", "A", "B", "B", "B"),
             SEX = c("F", "M", "F", "F", "M"),
             SEV = c("MILD", "SEV", "MILD", "SEV", "SEV"),
             VIS = c("V1", "V2", "V1", "V2", "V1"),
             stringsAsFactors = FALSE)
}

# DM: group1 only; Lab: group1 to group3
bi_dm <- function(e, id = "DM") {
  e$tag_ard(cards::ard_tabulate(bi_data(), by = "ARM", variables = "SEX"), "T-1", id, "SAF")
}
bi_lab <- function(e, id = "LAB") {
  e$tag_ard(cards::ard_tabulate(bi_data(), by = c("ARM", "SEX", "VIS"), variables = "SEV"),
            "T-1", id, "SAF")
}

test_that("bind_items() orders the columns and keeps the class, with mixed group counts", {
  e <- bi_env()
  dm <- bi_dm(e)
  lab <- bi_lab(e)
  a <- e$bind_items(dm, lab)
  expect_s3_class(a, "card")
  nm <- names(a)
  expect_identical(nm[1:3], c("output_id", "analysis_id", "population_id"))
  expect_identical(nm[4:9], c("group1", "group1_level", "group2", "group2_level",
                              "group3", "group3_level"))
  expect_identical(nm[10:13], c("variable", "variable_level", "context", "stat_name"))
  expect_true(all(c("stat", "fmt_fun") %in% nm))
  expect_lt(match("stat_label", nm), match("stat", nm))
  # the other order of the items gives the same order of columns
  expect_identical(names(e$bind_items(lab, dm)), nm)
  # a list of items works too
  expect_identical(names(e$bind_items(list(dm, lab))), nm)
})

test_that("bind_items() never loses a row", {
  e <- bi_env()
  dm <- bi_dm(e)
  lab <- bi_lab(e)
  a <- e$bind_items(dm, lab)
  expect_identical(nrow(a), nrow(dm) + nrow(lab))
  expect_identical(sum(a$analysis_id == "DM"), nrow(dm))
  expect_identical(sum(a$analysis_id == "LAB"), nrow(lab))
})

test_that("bind_items() stops on a statistic twice in one item", {
  e <- bi_env()
  twice <- dplyr::bind_rows(bi_dm(e), bi_dm(e))
  expect_error(e$bind_items(twice, bi_lab(e)),
               "item DM: .*rows with the same key .*SEX n")
  expect_error(e$bind_items(twice), "item DM: 24 rows")
  # the same key under another item id is not an error
  expect_no_error(suppressWarnings(e$bind_items(bi_dm(e), bi_dm(e, "DM2"))))
})

test_that("bind_items() warns on the same statistic in two items, rows kept", {
  e <- bi_env()
  dm <- bi_dm(e)
  dm2 <- bi_dm(e, "DM2")
  expect_warning(a <- e$bind_items(dm, dm2),
                 "items DM and DM2 .*SEX n.*the same values.*all rows kept")
  expect_identical(nrow(a), nrow(dm) + nrow(dm2))
  # one warning per pair of items, not per row
  w <- 0L
  withCallingHandlers(e$bind_items(dm, dm2), warning = function(c) {
    w <<- w + 1L
    invokeRestart("muffleWarning")
  })
  expect_identical(w, 1L)
  # another output or analysis set is not the same key
  other <- e$tag_ard(cards::ard_tabulate(bi_data(), by = "ARM", variables = "SEX"),
                     "T-2", "DM", "SAF")
  expect_no_warning(e$bind_items(dm, other))
})

test_that("bind_items() warns when the same key has different values, rows kept", {
  e <- bi_env()
  dm <- bi_dm(e)
  dm2 <- e$tag_ard(cards::ard_tabulate(bi_data()[-1L, ], by = "ARM", variables = "SEX"),
                   "T-1", "DM2", "SAF")
  expect_warning(a <- e$bind_items(dm, dm2), "values differ")
  expect_identical(nrow(a), nrow(dm) + nrow(dm2))
  expect_true(all(c("DM", "DM2") %in% a$analysis_id))
})
