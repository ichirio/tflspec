# tfl_review_spec() (R/review.R): the branches no other test reaches -- the
# definitions it cannot read, the analysis data whose source it cannot
# follow, and the data facts' edge cases.  On the clean study of
# helper-review.R; fictional data only.

.cov_rv_rows <- function(r, rule) r[r$rule == rule, , drop = FALSE]

.cov_rv_facts <- function(s) {
  tfl_data_facts(s$data, populations = s$ard,
                 listings = tfl_listing_spec(s$listings, check = FALSE))
}

# ---- the definitions it cannot read ----------------------------------------

test_that("a table definition whose sheet is not a frame is one S01 row", {
  r <- tfl_review_spec(spec = list(tables = list(a = function() 1)))
  expect_identical(r$rule, "S01")
  expect_identical(r$level, "error")
  expect_match(r$message, "coerce")
})

test_that("a table definition its reader stops on is one S01 row", {
  local_mocked_bindings(.table_spec_problems = function(x) stop("a fictional stop"))
  r <- tfl_review_spec(spec = list(tables = data.frame(output_id = "T-1")))
  expect_identical(r$rule, "S01")
  expect_identical(r$message, "a fictional stop")
})

test_that("an ARD definition whose sheet is not a frame is one S01 row", {
  r <- tfl_review_spec(ard = list(analyses = list(function() 1)))
  expect_identical(r$rule, "S01")
  expect_identical(r$area, "spec")
})

test_that("a listing definition that cannot be built is one S01 row", {
  r <- tfl_review_spec(listings = list(listings = list(function() 1)))
  expect_identical(r$rule, "S01")
  expect_identical(r$area, "spec")
})

test_that("print() lists each row after the counts", {
  s <- .rv_study()
  s$ard$analyses$by[2] <- "context"
  r <- .rv_review(s, data = FALSE)
  out <- capture.output(res <- print(r))
  expect_identical(res, r)
  expect_match(out[1L], "^<tfl_review> 1 error, ")
  expect_true(any(grepl("A15", out[-1L], fixed = TRUE)))
})

test_that("print() of an empty review says the counts only", {
  r <- .rv_review(.rv_study(), data = FALSE)
  out <- capture.output(print(r))
  expect_identical(out, "<tfl_review> 0 errors, 0 to check, 0 to set by hand")
})

test_that("a listing problem L01 does not know by its words is kept as it stands", {
  local_mocked_bindings(.listing_problems = function(x) {
    data.frame(message = "something odd: no `vars`", sheet = "listing_cols",
               output_id = "L-9", row = "1", field = "vars",
               stringsAsFactors = FALSE)
  })
  out <- tflspec:::.rv_listing_problems(list())
  expect_length(out, 1L)
  expect_identical(out[[1L]]$rule, "L01")
  expect_identical(out[[1L]]$message, "something odd: no `vars`")
})

# ---- what the analyses read ------------------------------------------------

test_that("an analysis inside a parent takes the parent's data", {
  a <- data.frame(output_id = "T-1", analysis_id = c("P", "C"),
                  method = c("cards::ard_stack", "categorical"),
                  data = c("adsl_sub", NA), parent = c(NA, "P"),
                  by = c("TRT01A", NA), variables = c(NA, "SEX"),
                  stringsAsFactors = FALSE)
  f <- tflspec:::.review_flat(a)
  expect_identical(f$analysis_id, "C")
  expect_identical(f$data, "adsl_sub")
})

test_that("an analysis whose data chain is broken has no source", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  a <- tflspec:::.review_flat(x$analyses)
  a$data[2] <- "no_such_data"
  expect_null(tflspec:::.review_source(x, a, 2L))
})

test_that("an analysis whose data is made by code has no source", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  x$analysis_data$code <- c(NA, NA, "adae_sub <- my_data()")
  a <- tflspec:::.review_flat(x$analyses)
  expect_null(tflspec:::.review_source(x, a, 5L))
})

test_that("an analysis with no data takes its dataset from its analysis set", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  a <- tflspec:::.review_flat(x$analyses)
  a$data <- NA_character_
  a$dataset <- NA_character_
  a$population_id <- "SAF"
  a$where <- c(NA, "AGE > 50", NA, NA, NA)
  s <- tflspec:::.review_source(x, a, 2L)
  expect_identical(s$dataset, "ADSL")
  expect_identical(s$population_id, "SAF")
  expect_identical(s$where, "AGE > 50")
})

# ---- rules on the ARD definition -------------------------------------------

test_that("A06: a strata variable also analysed is named by `strata`", {
  s <- .rv_study()
  s$ard$analyses$strata <- c(NA, NA, "SEX", NA, NA)
  r <- .cov_rv_rows(.rv_review(s, data = FALSE), "A06")
  expect_identical(r$field, "strata")
  expect_identical(r$row, "SEX")
})

test_that("A15: a derived column named as an R reserved word", {
  s <- .rv_study()
  s$ard$datasets$derive <- c("TRUE = AGE > 50", NA)
  r <- .cov_rv_rows(.rv_review(s, data = FALSE), "A15")
  expect_identical(r$sheet, "datasets")
  expect_identical(r$row, "ADSL")
  expect_identical(r$field, "derive")
})

test_that("A15: an analysis data deriving a column named as one of the ARD's own", {
  s <- .rv_study()
  s$ard$analysis_data$derive <- c(NA, NA, "stat = 1")
  r <- .cov_rv_rows(.rv_review(s, data = FALSE), "A15")
  expect_identical(r$sheet, "analysis_data")
  expect_identical(r$output_id, "T-2")
})

# ---- rules on the table definition -----------------------------------------

test_that("a sheet with no rows gives a report no rows of its own", {
  d <- data.frame(output_id = character(), cols = character())
  expect_identical(tflspec:::.review_rows(d, "T-1"), d)
  expect_null(tflspec:::.review_rows(NULL, "T-1"))
})

test_that("T03 skips a cells row for no variable when the report has two kinds", {
  s <- .rv_study()
  s$spec$cells <- rbind(s$spec$cells,
    data.frame(output_id = "T-1", variable = NA, row = "x",
               template = "{n} {mean}"))
  r <- .rv_review(s, data = FALSE)
  expect_identical(nrow(.cov_rv_rows(r, "T03")), 0L)
})

test_that("T03: a cells row for no variable in a report of one kind names the report", {
  s <- .rv_study()
  s$ard$analyses <- s$ard$analyses[-c(1, 3), ]
  s$spec$cells <- rbind(s$spec$cells,
    data.frame(output_id = "T-1", variable = NA, row = "x", template = "{p}"))
  r <- .cov_rv_rows(.rv_review(s, data = FALSE), "T03")
  expect_identical(r$output_id, "T-1")
  expect_match(r$message, "T-1")
})

test_that("T08 skips a variable whose levels the ARD does not know", {
  s <- .rv_study()
  s$spec$variables$levels <- c("young | old", NA)
  ard <- list(`T-1` = list(
    groups = list(TRT01A = c("Drug", "Placebo")),
    variables = list(AGE = list(levels = character(), stats = c("N", "mean", "sd"))),
    stats = c("N", "mean", "sd", "n", "p")))
  r <- .rv_review(s, data = FALSE, ard_facts = ard)
  expect_identical(nrow(.cov_rv_rows(r, "T08")), 0L)
})

test_that("T08: a level the ARD does not have", {
  s <- .rv_study()
  s$spec$variables$levels <- c(NA, "F | M | U")
  ard <- list(`T-1` = list(
    groups = list(TRT01A = c("Drug", "Placebo")),
    variables = list(AGE = list(levels = character(), stats = c("N", "mean", "sd")),
                     SEX = list(levels = c("F", "M"), stats = c("n", "p"))),
    stats = c("N", "mean", "sd", "n", "p")))
  r <- .cov_rv_rows(.rv_review(s, data = FALSE, ard_facts = ard), "T08")
  expect_identical(r$row, "SEX")
  expect_match(r$message, "U")
})

test_that("C01 has nothing to say when no code list is written", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  sp <- list(codelists = data.frame(output_id = character(), variable = character()))
  expect_identical(tflspec:::.rule_c01(sp, x), list())
  expect_identical(tflspec:::.rules_codelists(sp, x, list()), list())
})

# ---- rules on the data facts -----------------------------------------------

test_that("a condition the facts did not count has no facts", {
  facts <- .cov_rv_facts(.rv_study())
  expect_null(tflspec:::.facts_condition(facts, "ADSL", NA, "AGE > 99"))
  expect_null(tflspec:::.facts_condition(list(), "ADSL", NA, NA))
})

test_that("A02: an analysis set whose condition fails on its data", {
  s <- .rv_study()
  facts <- .cov_rv_facts(s)
  facts$populations$SAF$error <- "object of a fictional kind"
  r <- tfl_review_spec(ard = s$ard, facts = facts)
  r <- .cov_rv_rows(r, "A02")
  expect_identical(r$sheet, "populations")
  expect_match(r$message, "fictional kind")
})

test_that("A03: an analysis set that keeps no subject", {
  s <- .rv_study()
  facts <- .cov_rv_facts(s)
  facts$populations$SAF$n_subjects <- 0L
  r <- .cov_rv_rows(tfl_review_spec(ard = s$ard, facts = facts), "A03")
  expect_identical(r$sheet, "populations")
  expect_identical(r$row, "SAF")
})

test_that("an analysis set with no facts of its own is not judged", {
  s <- .rv_study()
  facts <- .cov_rv_facts(s)
  facts$populations$SAF <- NULL
  r <- tfl_review_spec(ard = s$ard, facts = facts)
  expect_false(any(r$sheet == "populations"))
})

test_that("analysis data made by code, or with no data_id, are not checked", {
  s <- .rv_study()
  s$ard$analysis_data$code <- c(NA, NA, "adae_sub <- my_adae()")
  s$ard$analysis_data$keep <- c(NA, NA, "NOSUCH")
  r <- .rv_review(s)
  expect_false(any(r$rule == "A01" & r$sheet == "analysis_data"))
})

test_that("analysis data whose chain goes round are not checked", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  x$analysis_data$from[3] <- "adae_sub"
  x$analysis_data$keep <- c(NA, NA, "NOSUCH")
  out <- tflspec:::.rules_data_ard(x, .cov_rv_facts(.rv_study()))
  expect_false(any(vapply(out, function(r) r$sheet == "analysis_data", NA)))
})

test_that("analysis data made from code higher up are not checked", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  extra <- x$analysis_data[3, ]
  extra$data_id <- "adae_two"
  extra$from <- "adae_sub"
  extra$keep <- "NOSUCH"
  x$analysis_data <- rbind(x$analysis_data, extra)
  x$analysis_data$code <- c(NA, NA, "adae_sub <- my_adae()", NA)
  out <- tflspec:::.rules_data_ard(x, .cov_rv_facts(.rv_study()))
  expect_false(any(vapply(out, function(r) r$sheet == "analysis_data", NA)))
})

test_that("analysis data of a dataset the facts do not hold are not checked", {
  s <- .rv_study()
  facts <- .cov_rv_facts(s)
  facts$datasets$ADAE <- NULL
  s$ard$analysis_data$keep <- c(NA, NA, "NOSUCH")
  r <- tfl_review_spec(ard = s$ard, facts = facts)
  expect_false(any(r$rule == "A01" & r$sheet == "analysis_data"))
})

test_that("A01: `add` names a column the analysis set's data do not have", {
  s <- .rv_study()
  s$ard$analysis_data$add <- c(NA, NA, "NOSUCH")
  r <- .cov_rv_rows(.rv_review(s), "A01")
  expect_identical(r$field, "add")
  expect_identical(r$row, "adae_sub")
})

test_that("`add` is not checked when the analysis set's data have no facts", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  x$analysis_data$add <- c(NA, NA, "NOSUCH")
  x$populations$dataset <- "ADXX"
  out <- tflspec:::.rules_data_ard(x, .cov_rv_facts(.rv_study()))
  expect_false(any(vapply(out, function(r) identical(r$field, "add"), NA)))
})

test_that("A01: `arrange` names a column the data do not have", {
  s <- .rv_study()
  s$ard$analysis_data$arrange <- c(NA, NA, "desc(NOSUCH) | USUBJID")
  r <- .cov_rv_rows(.rv_review(s), "A01")
  expect_identical(r$field, "arrange")
  expect_match(r$message, "NOSUCH")
})

test_that("A02: an analysis data's condition names a column the data do not have", {
  s <- .rv_study()
  s$ard$analysis_data$where[3] <- 'NOSUCH == "Y"'
  r <- .cov_rv_rows(.rv_review(s), "A02")
  expect_identical(r$sheet, "analysis_data")
  expect_match(r$message, "no column NOSUCH")
})

test_that("A16: an analysis data derives a column its dataset already has", {
  s <- .rv_study()
  s$ard$analysis_data$derive <- c(NA, NA, "AESEV = tolower(AESEV)")
  r <- .cov_rv_rows(.rv_review(s), "A16")
  expect_identical(r$sheet, "analysis_data")
  expect_identical(r$row, "adae_sub")
})

test_that("A02: an analysis's own condition names a column the data do not have", {
  s <- .rv_study()
  s$ard$analyses$where <- c(NA, "NOSUCH > 1", NA, NA, NA)
  r <- .cov_rv_rows(.rv_review(s), "A02")
  expect_identical(r$sheet, "analyses")
  expect_identical(r$row, "AGE")
})

test_that("A02: an analysis's condition that fails on the data it has", {
  s <- .rv_study()
  s$ard$analyses$where <- c(NA, "AGE > stop('fictional')", NA, NA, NA)
  r <- .cov_rv_rows(.rv_review(s), "A02")
  expect_identical(r$row, "AGE")
  expect_identical(r$field, "where")
})

test_that("a custom analysis's columns are not checked against the data", {
  s <- .rv_study()
  s$ard$analyses$method[2] <- "custom"
  s$ard$analyses$by[2] <- "NOSUCH"
  r <- .rv_review(s)
  expect_false(any(r$rule == "A01" & r$row == "AGE"))
})

test_that("A03: an analysis's own condition that keeps no row names `where`", {
  s <- .rv_study()
  s$ard$analyses$where <- c(NA, NA, NA, NA, 'TRTEMFL == "Z"')
  r <- .cov_rv_rows(.rv_review(s), "A03")
  expect_identical(r$row, "TEAE")
  expect_identical(r$field, "where")
  expect_match(r$message, '(TRTEMFL == "Y") & (TRTEMFL == "Z")', fixed = TRUE)
})

test_that("A02: an analysis data's condition that fails names `data`", {
  s <- .rv_study()
  s$ard$analysis_data$where[3] <- 'TRTEMFL == stop("fictional")'
  r <- .cov_rv_rows(.rv_review(s), "A02")
  expect_identical(r$sheet, "analyses")
  expect_identical(r$field, "data")
})

test_that("a condition the facts did not count is not judged a second time", {
  s <- .rv_study()
  s$ard$analyses$where <- c(NA, "AGE > 1", "AGE > 1", NA, NA)
  r <- .rv_review(s)
  expect_false(any(r$rule %in% c("A02", "A03")))
})

test_that("a condition with an unparsable text names no missing column", {
  expect_identical(tflspec:::.where_missing("AGE >", "AGE"), character())
  expect_identical(tflspec:::.where_missing(NULL, "AGE"), character())
})

test_that("A05 skips analyses with no source, no facts or no such column", {
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  facts <- .cov_rv_facts(.rv_study())
  x$analyses$data[2] <- "no_such_data"
  x$analyses$variables[3] <- "NOSUCH"
  x$analysis_data$from[3] <- "ADXX"
  x$analyses$method[1] <- "my.pkg::my_fn"
  x$analyses$method[5] <- "categorical"
  x$analyses$variables[5] <- "AEDECOD"
  expect_identical(tflspec:::.rule_a05(x, facts), list())
})

test_that("C02 / C03 skip a report with only custom analyses or no source", {
  s <- .rv_study()
  sp <- suppressMessages(tfl_table_spec(s$spec))
  x <- tflspec:::.review_ard_sheets(s$ard)
  facts <- .cov_rv_facts(s)
  x$analyses$method[1:3] <- "custom"
  expect_identical(tflspec:::.rules_codelists(sp, x, facts), list())
  x <- tflspec:::.review_ard_sheets(s$ard)
  x$analyses$data[1:3] <- "no_such_data"
  expect_identical(tflspec:::.rules_codelists(sp, x, facts), list())
})

test_that("C02 / C03 skip a code list of a variable no analysis reads", {
  s <- .rv_study()
  s$spec$codelists <- rbind(s$spec$codelists,
    data.frame(output_id = "T-1", variable = "RACE", value = "A", order = 1))
  sp <- suppressMessages(tfl_table_spec(s$spec))
  x <- tflspec:::.review_ard_sheets(s$ard)
  out <- tflspec:::.rules_codelists(sp, x, .cov_rv_facts(s))
  expect_false(any(vapply(out, function(r) grepl("RACE", r$row), NA)))
})

test_that("C02 / C03 skip a variable whose values are not known", {
  s <- .rv_study()
  sp <- suppressMessages(tfl_table_spec(s$spec))
  x <- tflspec:::.review_ard_sheets(s$ard)
  facts <- .cov_rv_facts(s)
  facts$populations$SAF$values <- list()
  facts$conditions <- NULL
  expect_identical(tflspec:::.rules_codelists(sp, x, facts), list())
})

test_that("the values come from the analysis set when the condition has none", {
  facts <- list(conditions = data.frame(dataset = "ADSL", population_id = "SAF",
                                        where = NA, error = NA, n_rows = 6L),
                populations = list(SAF = list(dataset = "ADSL",
                                              values = list(SEX = c("F", "M")))))
  facts$conditions$values <- list(list(AGE = 1))
  s <- list(dataset = "ADSL", population_id = "SAF", where = NA_character_)
  expect_identical(tflspec:::.facts_values_for(facts, s, "SEX"), c("F", "M"))
})

test_that("the values of a whole dataset, capped or not known", {
  cols <- data.frame(name = c("SEX", "AGE", "RACE"),
                     class = c("character", "numeric", "character"))
  cols$values <- list(NULL, NULL, c("A", "B"))
  facts <- list(datasets = list(ADSL = list(columns = cols)))
  s <- list(dataset = "ADSL", population_id = NA_character_, where = NA_character_)
  expect_identical(tflspec:::.facts_values_for(facts, s, "SEX"), "capped")
  expect_null(tflspec:::.facts_values_for(facts, s, "AGE"))
  expect_null(tflspec:::.facts_values_for(facts, s, "NOSUCH"))
  expect_identical(tflspec:::.facts_values_for(facts, s, "RACE"), c("A", "B"))
  s$where <- "AGE > 1"
  expect_null(tflspec:::.facts_values_for(facts, s, "RACE"))
})

# ---- L02 -------------------------------------------------------------------

test_that("L02: a listing of a dataset not in the catalog", {
  s <- .rv_study()
  s$listings$listings$dataset <- "ADXX"
  r <- .cov_rv_rows(.rv_review(s), "L02")
  expect_identical(r$field, "dataset")
  expect_match(r$message, "ADXX")
})

test_that("L02 skips a listing with no output_id or no dataset", {
  lf <- tfl_listing_spec(.rv_study()$listings, check = FALSE)
  lf$listings$dataset <- NA
  x <- tflspec:::.review_ard_sheets(.rv_study()$ard)
  expect_identical(tflspec:::.rule_l02(lf, x, .cov_rv_facts(.rv_study())), list())
})

test_that("L02 skips a listing whose dataset is in the catalog but has no facts", {
  s <- .rv_study()
  lf <- tfl_listing_spec(s$listings, check = FALSE)
  x <- tflspec:::.review_ard_sheets(s$ard)
  facts <- .cov_rv_facts(s)
  facts$datasets$ADAE <- NULL
  expect_identical(tflspec:::.rule_l02(lf, x, facts), list())
})

test_that("L02: a listing sorted by a column the data do not have", {
  s <- .rv_study()
  s$listings$listings$sort <- "USUBJID | -NOSUCH"
  r <- .cov_rv_rows(.rv_review(s), "L02")
  expect_identical(r$field, "sort")
  expect_match(r$message, "NOSUCH")
})

test_that("L02: a listing column showing a variable the data do not have", {
  s <- .rv_study()
  s$listings$listing_cols$vars[2] <- "NOSUCH"
  r <- .cov_rv_rows(.rv_review(s), "L02")
  expect_identical(r$sheet, "listing_cols")
  expect_identical(r$row, "2")
})

test_that("A02: a listing's condition names a column the data do not have", {
  s <- .rv_study()
  s$listings$listings$where <- 'NOSUCH == "Y"'
  r <- .cov_rv_rows(.rv_review(s), "A02")
  expect_identical(r$sheet, "listings")
  expect_identical(r$area, "listing")
})

test_that("A02 / A03: a listing's condition that fails, or keeps no row", {
  s <- .rv_study()
  lf <- tfl_listing_spec(s$listings, check = FALSE)
  x <- tflspec:::.review_ard_sheets(s$ard)
  facts <- .cov_rv_facts(s)
  k <- which(facts$conditions$where %in% 'AESEV == "SEVERE"')
  expect_true(length(k) >= 1L)
  facts$conditions$error[k] <- "a fictional failure"
  out <- tflspec:::.rule_l02(lf, x, facts)
  expect_identical(out[[1L]]$rule, "A02")
  facts$conditions$error[k] <- NA
  facts$conditions$n_rows[k] <- 0L
  out <- tflspec:::.rule_l02(lf, x, facts)
  expect_identical(out[[1L]]$rule, "A03")
})

# ---- A17 -------------------------------------------------------------------

test_that("A17 skips a report whose ARD run said nothing, and reads no analysis_id", {
  facts <- list(ard = list(
    `T-1` = list(conditions = NULL),
    `T-2` = list(conditions = data.frame(level = "warning", variable = "AGE",
                                         groups = "TRT01A", message = "a fictional note"))))
  out <- tflspec:::.rule_a17(facts)
  expect_length(out, 1L)
  expect_identical(out[[1L]]$output_id, "T-2")
  expect_identical(out[[1L]]$row, "")
  expect_identical(out[[1L]]$level, "check")
  expect_match(out[[1L]]$message, "AGE (TRT01A): a fictional note", fixed = TRUE)
})

# ---- figures ---------------------------------------------------------------

test_that("a figure design that cannot be read is one S01 row", {
  r <- tfl_review_spec(figures = list(`F-1` = "not a design"))
  r <- .cov_rv_rows(r, "S01")
  expect_identical(r$output_id, "F-1")
  expect_identical(r$area, "figure")
})

test_that("a design problem other than a missing field is an S01 row of the figure", {
  d <- tfl_fig_design(data = list(list(step = "read", dataset = "ADSL")),
                      layers = list(list(geom = "point", x = "AGE", y = "AGE")))
  r <- .cov_rv_rows(tfl_review_spec(figures = list(`F-1` = d)), "S01")
  expect_match(r$row, "^layers\\[1\\]")
  expect_identical(r$area, "figure")
  expect_match(r$message, "unknown layer")
})

test_that("F03 says a problem it has no words for as it stands", {
  r <- tflspec:::.rv_f03("F-1", "layer 1", "x", "something fictional")
  expect_identical(r$rule, "F03")
  expect_identical(r$message, "layer 1 x something fictional")
})

test_that("the facts' frames hold a row per value, typed by class", {
  cols <- data.frame(name = c("A", "B", "C", "D", "E"),
                     class = c("numeric", "logical", "Date", "character", "character"))
  cols$values <- list(NULL, NULL, NULL, c("x", "y"), NULL)
  f <- tflspec:::.facts_frames(list(datasets = list(ADXX = list(columns = cols))))
  expect_identical(nrow(f$ADXX), 2L)
  expect_true(is.numeric(f$ADXX$A))
  expect_true(is.logical(f$ADXX$B))
  expect_s3_class(f$ADXX$C, "Date")
  expect_identical(f$ADXX$D, c("x", "y"))
  expect_identical(f$ADXX$E, c(NA_character_, NA_character_))
})
