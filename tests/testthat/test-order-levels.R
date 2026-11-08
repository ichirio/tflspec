# The arms and the categories as factor levels set in the data step, before
# the ARD (order_levels()): a code list, else the numeric pair (TRT01A by
# TRT01AN, SEX by SEXN, RACE by RACEN), else the values sorted.  The arm of
# an analysis's records always comes from the analysis set's ADSL.

test_that("order_levels(): a code list, else the numeric pair, else the values sorted; a factor keeps its levels", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  d <- data.frame(TRT01A = c("Placebo", "Drug B", "Drug A", "Drug B", ""),
                  TRT01AN = c(1, 2, 3, 2, NA),
                  SEX = c("F", "M", "M", "F", "F"), SEXN = c(2, 1, 1, 2, 2),
                  RACE = c("WHITE", "ASIAN", "BLACK", "WHITE", "ASIAN"), RACEN = c(1, 3, 2, 1, 3),
                  EOSSTT = c("ONGOING", "COMPLETED", "DISCONTINUED", "COMPLETED", "ONGOING"),
                  ETHNIC = factor(c("Z", "A", "Z", "A", "Z"), levels = c("Z", "A")),
                  AGEGR1 = c("<65", ">=65", "<65", ">=65", "<65"),
                  AGE = c(40, 70, 50, 66, 30), stringsAsFactors = FALSE)
  r <- e$order_levels(d, c("SEX", "RACE", "EOSSTT", "ETHNIC"))
  # the numeric pair: the arm (a blank is missing), SEX, RACE
  expect_identical(levels(r$TRT01A), c("Placebo", "Drug B", "Drug A"))
  expect_true(is.na(r$TRT01A[5L]))
  expect_identical(levels(r$SEX), c("M", "F"))
  expect_identical(levels(r$RACE), c("WHITE", "BLACK", "ASIAN"))
  # no pair: the fallback is today's -- the column as it is (the ARD sorts text
  # by the C locale's byte order, order(x, method = "radix"), whatever the
  # session's locale), not a factor of the values this data has
  expect_identical(r$EOSSTT, d$EOSSTT)
  # a factor already in the data keeps its levels
  expect_identical(levels(r$ETHNIC), c("Z", "A"))
  # what is not named is as it was; a number is left alone
  expect_type(r$AGEGR1, "character")
  expect_identical(r$AGE, d$AGE)
  expect_identical(r$TRT01AN, d$TRT01AN)
  # a variable the data does not have is skipped
  expect_identical(e$order_levels(d, "NOPE")$SEX, d$SEX)
  # a code list wins, with set_levels()'s stop on a value it does not have
  cl_arm <- c(`Drug A` = "Drug A", `Drug B` = "Drug B", Placebo = "Placebo")
  r2 <- e$order_levels(d[1:4, ], c("SEX", "RACE"), SEX = c(F = "Female", M = "Male"), TRT01A = cl_arm)
  expect_identical(levels(r2$SEX), c("Female", "Male"))
  expect_identical(levels(r2$TRT01A), names(cl_arm))
  expect_identical(levels(r2$RACE), c("WHITE", "BLACK", "ASIAN"))
  expect_error(e$order_levels(d, SEX = c(M = "Male")), "SEX has values its code list does not")
  # (the same as set_levels() on the arms in their order)
  expect_identical(e$order_levels(d, SEX = c(F = "Female", M = "Male")),
                   e$set_levels(e$order_arms(d), SEX = c(F = "Female", M = "Male")))
  # order_arms() is order_levels() with no categories
  expect_identical(e$order_arms(d), e$order_levels(d))
})

test_that("the program names the categories of its categorical analyses, the ones with no code list", {
  skip_on_cran()
  skip_if_not_installed("cards")
  spec <- exact_spec(list(method = "categorical", dataset = "ADSL", population_id = "SAF",
                          by = "ARM", variables = "SEX|RACE"))
  code <- paste(tfl_ard_code(spec, part = "body"), collapse = "\n")
  expect_match(code, "adsl <- readRDS(\"adam/ADSL.rds\") |> order_levels(c(\"SEX\", \"RACE\"))", fixed = TRUE)
  # a code list is the order of its variable: not named
  cl <- data.frame(output_id = "T", variable = "SEX", value = c("M", "F"),
                   label = c("Male", "Female"), order = 1:2)
  code2 <- paste(tfl_ard_code(spec, part = "body", codelists = cl), collapse = "\n")
  expect_match(code2, "order_levels(\"RACE\")", fixed = TRUE)
  # no categorical analysis: the arms alone
  cont <- exact_spec(list(method = "continuous", dataset = "ADSL", population_id = "SAF",
                          by = "ARM", variables = "AGE"))
  expect_match(paste(tfl_ard_code(cont, part = "body"), collapse = "\n"),
               "adsl <- readRDS(\"adam/ADSL.rds\") |> order_levels()", fixed = TRUE)
})

# a study of the package's data: the arms numbered against the alphabet
# (TRT01AN counts down the sorted names), the records without their own
# TRT01AN, ADSL with SEXN and RACEN against the alphabet
lv_study <- function(root, datasets, drop_arm_n = TRUE, pairs = FALSE) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  arms <- sort(unique(as.character(adsl$TRT01A)))
  num <- function(x) length(arms) + 1L - match(x, arms)
  for (x in datasets) {
    z <- getExportedValue("tflspec", x)
    if ("TRT01A" %in% names(z)) z$TRT01AN <- num(z$TRT01A)
    if (identical(x, "adsl") && pairs) {
      z$SEXN <- ifelse(z$SEX == "M", 1, 2)
      lv <- sort(unique(z$RACE))
      z$RACEN <- length(lv) + 1L - match(z$RACE, lv)
    }
    if (drop_arm_n && !identical(x, "adsl")) z$TRT01AN <- NULL
    haven::write_xpt(tflspec:::.ai_chr(z), file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "LV-1")
  d
}
lv_levels <- function(ard, analysis_id, col = "group1_level") {
  k <- ard$analysis_id %in% analysis_id & ard$stat_name %in% c("n", "N")
  unique(unlist(lapply(ard[[col]][k], as.character)))
}

test_that("a records dataset without TRT01AN gets the arms' order of ADSL", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  d <- lv_study(withr::local_tempdir(), c("adsl", "adlb", "adae"))
  arms <- sort(unique(as.character(adsl$TRT01A)))
  want <- rev(arms)
  expect_false(identical(want, arms))
  put <- function(id, name, args) {
    tfl_write_output(tfl_output(id, "table", population = "SAF", extends = "defaults",
                                template = list(name = name, args = args)),
                     file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  put("T-LA", "lab_abnormal", list())
  st <- tfl_read_study_spec(d)
  code <- paste(tfl_ard_code(st, output_id = "T-LA", save = FALSE, dir = d), collapse = "\n")
  # the records keep their arm; its order is ADSL's
  expect_match(code, "adlb <- haven::read_xpt(\"adam/adlb.xpt\") |> order_levels(", fixed = TRUE)
  expect_match(code, "from = adsl", fixed = TRUE)
  ard <- suppressMessages(suppressWarnings(tfl_build_ard(st, d, output_id = "T-LA", save = FALSE)))
  expect_identical(lv_levels(ard, "ABNORMAL"), want)
})

test_that("RACE follows RACEN, SEX follows SEXN, where ADSL has the pair", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  out <- function(d) {
    tfl_write_output(tfl_output("T-SU", "table", population = "SAF", extends = "defaults",
                                template = list(name = "summary",
                                                args = list(continuous = "AGE", categorical = c("SEX", "RACE")))),
                     file.path(d, "spec", "outputs", "T-SU.yml"))
    suppressMessages(suppressWarnings(tfl_build_ard(tfl_read_study_spec(d), d, output_id = "T-SU", save = FALSE)))
  }
  lev <- function(ard, v) {
    unique(unlist(lapply(ard$variable_level[ard$variable == v & ard$stat_name == "n"], as.character)))
  }
  races <- sort(unique(adsl$RACE))
  ard <- out(lv_study(withr::local_tempdir(), "adsl", pairs = TRUE))
  expect_identical(lev(ard, "SEX"), c("M", "F"))
  expect_identical(lev(ard, "RACE"), rev(races))
  # without the pairs: as today, the values in the C locale's byte order
  ard0 <- out(lv_study(withr::local_tempdir(), "adsl"))
  expect_identical(lev(ard0, "SEX"), c("F", "M"))
  expect_identical(lev(ard0, "RACE"), races[order(races, method = "radix")])
})

test_that("a blank category is missing, not a level (an XPT holds \"\" for a missing text)", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  d <- data.frame(TRT01A = c("Placebo", "Drug", "Drug"), TRT01AN = c(1, 2, 2),
                  RACE = c("WHITE", "", "ASIAN"), RACEN = c(1, NA, 2),
                  SEX = c("F", " ", "M"), SEXN = c(2, NA, 1), stringsAsFactors = FALSE)
  r <- e$order_levels(d, c("RACE", "SEX"))
  expect_identical(levels(r$RACE), c("WHITE", "ASIAN"))
  expect_true(is.na(r$RACE[2L]))
  expect_identical(levels(r$SEX), c("M", "F"))
  expect_true(is.na(r$SEX[2L]))
})

test_that("a records arm without its own number is ordered by ADSL's pair, its values kept", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  adsl <- data.frame(TRT01A = c("Drug A", "Placebo", "Drug B", "Drug A"),
                     TRT01AN = c(3, 1, 2, 3), stringsAsFactors = FALSE)
  ae <- data.frame(TRTA = c("Drug A", "Placebo", "Drug B", "Drug A"), stringsAsFactors = FALSE)
  r <- e$order_levels(ae, from = adsl)
  expect_identical(levels(r$TRTA), c("Placebo", "Drug B", "Drug A"))
  expect_identical(as.character(r$TRTA), ae$TRTA)   # no record dropped, no value changed
  expect_identical(names(r), names(ae))
  # a value ADSL does not have goes last
  ae2 <- data.frame(TRTA = c("Drug A", "Other", "Placebo", "Drug B"), stringsAsFactors = FALSE)
  expect_identical(levels(e$order_levels(ae2, from = adsl)$TRTA),
                   c("Placebo", "Drug B", "Drug A", "Other"))
  # a map that is not one to one (a value with two numbers) is an error naming the variable
  bad <- data.frame(TRT01A = c("Placebo", "Drug A", "Drug A"), TRT01AN = c(1, 2, 3),
                    stringsAsFactors = FALSE)
  expect_error(e$order_levels(ae, from = bad), "TRTA has values with more than one TRT01AN")
  # (numbers equal for two values are allowed: the order the values come)
  same <- data.frame(TRT01A = c("Placebo", "Drug A", "Drug B"), TRT01AN = c(1, 2, 2),
                     stringsAsFactors = FALSE)
  expect_identical(levels(e$order_levels(ae, from = same)$TRTA), c("Placebo", "Drug A", "Drug B"))
})

test_that("a category value only outside the analysed data prints no row, unless a code list names it", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  adam$ADSL$SAFFL <- "Y"
  adam$ADSL$SAFFL[1L] <- "N"
  adam$ADSL$RACE <- as.character(adam$ADSL$RACE)
  adam$ADSL$RACE[1L] <- "NATIVE HAWAIIAN"   # only a subject outside the safety set has it
  adam$ADSL$RACEN <- match(adam$ADSL$RACE, c("WHITE", "NATIVE HAWAIIAN", unique(adam$ADSL$RACE)))
  dir <- exact_dir(adam)
  spec <- exact_spec(list(method = "categorical", dataset = "ADSL", population_id = "SAF",
                          by = "ARM", variables = "RACE"))
  lev <- function(ard) unique(unlist(lapply(ard$variable_level[ard$variable == "RACE" &
                                                                  ard$stat_name == "n"], as.character)))
  code <- paste(tfl_ard_code(spec, part = "body"), collapse = "\n")
  expect_match(code, "across(any_of(\"RACE\") & where(is.factor), droplevels)",
               fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(spec, dir = dir, save = FALSE))
  expect_false("NATIVE HAWAIIAN" %in% lev(ard))
  expect_identical(lev(ard)[1L], "WHITE")   # (RACEN's order)
  # a code list names it: its zero row, as today; and the drop is not written
  races <- c("WHITE", "NATIVE HAWAIIAN", setdiff(unique(adam$ADSL$RACE), c("WHITE", "NATIVE HAWAIIAN")))
  cl <- data.frame(output_id = "T", variable = "RACE", value = races, label = races,
                   order = seq_along(races))
  code2 <- paste(tfl_ard_code(spec, part = "body", codelists = cl), collapse = "\n")
  expect_false(grepl("droplevels", code2, fixed = TRUE))
  ard2 <- suppressMessages(tfl_build_ard(spec, dir = dir, save = FALSE, codelists = cl))
  expect_true("NATIVE HAWAIIAN" %in% lev(ard2))
})
