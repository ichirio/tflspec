# A template's analysis item: its own keys (params with condition rows,
# derive, levels, type), the BIGN rule

item_spec <- function(r, extra = list()) {
  sp <- tfl_ard_spec(exact_spec(r))
  for (k in names(extra)) sp$analyses[[k]] <- extra[[k]]
  sp
}

test_that("condition rows: a tribble of the params, a function of one row, the derive reading them", {
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  cond <- data.frame(subgroup = c("ALL", "Sex", "Sex"), subgroup_lvl = c("ALL", "F", "M"),
                     where = c("TRUE", "SEX == \"F\"", "SEX %in% \"M\""),
                     stringsAsFactors = FALSE)
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX"),
    list(params = "subgroup | subgroup_lvl | where", conditions = list(cond),
         where = "{where}",
         derive = "subgroup = \"{subgroup}\" | subgroup_lvl = \"{subgroup_lvl}\""))
  code <- tfl_ard_code(sp, output_id = "T", save = FALSE)
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "~subgroup, ~subgroup_lvl, ~where,", fixed = TRUE)
  expect_match(txt, "\"Sex\", \"F\", quote(SEX == \"F\"),", fixed = TRUE)
  expect_match(txt, "filter(!!where)", fixed = TRUE)
  expect_match(txt, "function(subgroup, subgroup_lvl, where) {", fixed = TRUE)
  expect_match(txt, "mutate(subgroup = subgroup, subgroup_lvl = subgroup_lvl)", fixed = TRUE)
  expect_match(txt, "purrr::pmap(", fixed = TRUE)
  # (the item's `{where}` is each row's, not the data's)
  expect_false(grepl("filter({where})", txt, fixed = TRUE))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  saf <- dplyr::filter(adam$ADSL, SAFFL == "Y")
  n_of <- function(lvl) {
    d <- ard[ard$stat_name == "n" & vapply(ard$variable_level, as.character, "") == lvl, ]
    d
  }
  expect_true(nrow(n_of("F")) > 0L)
  all_n <- sum(unlist(ard$stat[ard$stat_name == "n"]))
  # ALL (every subject) and F and M (every subject again)
  expect_identical(as.integer(all_n), 2L * nrow(saf))
})

test_that("derive and levels without conditions: the item's data under a name of its own", {
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "AGEGR"),
    list(derive = "AGEGR = ifelse(AGE < 65, \"<65\", \">=65\")",
         levels = "AGEGR = c(\">=65\", \"<65\")"))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "ard_a_data <- mutate(", fixed = TRUE)
  expect_match(txt, "AGEGR = factor(AGEGR, levels = c(\">=65\", \"<65\"))", fixed = TRUE)
})

test_that("type: a model or a value is not bound into the report's ARD; BIGN with no variable is total_n", {
  sp <- tfl_ard_spec(exact_spec(list(method = "categorical", population_id = "SAF",
                                     variables = "SEX")))
  a <- sp$analyses
  b <- a
  b$analysis_id <- "BIGN"
  b$variables <- NA_character_
  m <- a
  m$analysis_id <- "FIT"
  sp$analyses <- rbind(a, b, m)
  sp$analyses$type <- c(NA, NA, "model")
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "ard_total_n", fixed = TRUE)
  expect_match(txt, "bind_items(ard_a, ard_bign)", fixed = TRUE)
})

test_that("missing_row: each variable's missing values a level of their own (missing_level())", {
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX | RACE"),
    list(missing_row = "TRUE", missing_label = "Not reported"))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "SEX = missing_level(SEX, \"Not reported\")", fixed = TRUE)
  expect_match(txt, "RACE = missing_level(RACE, \"Not reported\")", fixed = TRUE)
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  got <- e$missing_level(factor(c("M", NA, "F"), levels = c("M", "F")))
  expect_identical(levels(got), c("M", "F", "Missing"))
  expect_identical(as.character(got), c("M", "Missing", "F"))
})

test_that("an item's where reading its params is each condition row's (Lab: a test and a direction)", {
  cond <- data.frame(tox_dir = c("Low", "High"), sexv = c("F", "M"), stringsAsFactors = FALSE)
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "worst"),
    list(params = "tox_dir | sexv", conditions = list(cond),
         where = "SEX == \"{sexv}\"",
         derive = "worst = if (\"{tox_dir}\" == \"Low\") AGE else AGE + 1"))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "function(tox_dir, sexv) {", fixed = TRUE)
  expect_match(txt, "filter(SEX == sexv)", fixed = TRUE)
  expect_match(txt, "worst = if (tox_dir == \"Low\") AGE else AGE + 1", fixed = TRUE)
  expect_false(grepl("{sexv}", txt, fixed = TRUE))
})

test_that("analyses per level: each depth its own id, the by counts dropped (BIGN's)", {
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX | RACE"),
    list(analysis_per = "level"))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "variable == \"..ard_hierarchical_overall..\" ~ \"A_ANY\"", fixed = TRUE)
  expect_match(txt, "variable == \"SEX\" ~ \"A_L1\"", fixed = TRUE)
  expect_match(txt, "variable == \"RACE\" ~ \"A_L2\"", fixed = TRUE)
  expect_match(txt, "filter(!variable %in% \"TRTA\")", fixed = TRUE)
})

test_that("analyses per columns: a condition row's id from its data, or the row's analysis_id", {
  cond <- data.frame(sexv = c("F", "M"), analysis_id = c("", "MEN"), stringsAsFactors = FALSE)
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "RACE"),
    list(params = "sexv", conditions = list(cond), where = "SEX == \"{sexv}\"",
         analysis_per = "SEX | TRTA"))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "function(sexv, analysis_id) {", fixed = TRUE)
  expect_match(txt, "\"F\", \"\",", fixed = TRUE)
  expect_match(txt, paste0("analysis_id = if (nzchar(.env$analysis_id)) .env$analysis_id else ",
                           "toupper(paste(unique(data$SEX), unique(data$TRTA), sep = \"_\"))"),
               fixed = TRUE)
})

test_that("levels reading params: the function's argument (.env$p), not a column of that name", {
  cond <- data.frame(tox_dir = c("Low", "High"), stringsAsFactors = FALSE)
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX"),
    list(params = "tox_dir", conditions = list(cond),
         derive = "tox_dir = \"{tox_dir}\"", levels = "tox_dir = \"{tox_dir}\""))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "tox_dir = tox_dir, tox_dir = factor(tox_dir, levels = .env$tox_dir)", fixed = TRUE)
})

test_that("ards: an item's several calls, one analysis_id, bound in the item (and in each condition row)", {
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  calls <- list(list(by = "TRTA", variables = "SEX"), list(by = "TRTA", variables = "RACE"))
  sp <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX"),
    list(ards = list(calls)))
  txt <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt, "ard_a <- bind_items(ard_a_1, ard_a_2)", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  expect_identical(unique(ard$analysis_id), "A")
  expect_setequal(unique(ard$variable), c("SEX", "RACE"))
  # with condition rows: bound inside the row's function
  cond <- data.frame(sexv = c("F", "M"), stringsAsFactors = FALSE)
  sp2 <- item_spec(
    list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX"),
    list(params = "sexv", conditions = list(cond), where = "SEX == \"{sexv}\"",
         ards = list(calls)))
  txt2 <- paste(tfl_ard_code(sp2, output_id = "T", save = FALSE), collapse = "\n")
  expect_match(txt2, "  ard <- bind_items(ard_1, ard_2)", fixed = TRUE)
  ard2 <- suppressMessages(tfl_build_ard(sp2, dir = dir, output_id = "T", save = FALSE))
  saf <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  n_race <- ard2[ard2$variable == "RACE" & ard2$stat_name == "n", ]
  expect_identical(as.integer(sum(unlist(n_race$stat))), nrow(saf))
})

test_that("type: model: the call kept under its own name, its data the item's, not in the ARD", {
  sp <- tfl_ard_spec(exact_spec(list(method = "categorical", population_id = "SAF",
                                     variables = "SEX")))
  a <- sp$analyses
  m <- a
  m$analysis_id <- "FIT"
  m$method <- "stats::lm"
  m$args <- "AGE ~ SEX"
  sp$analyses <- rbind(a, m)
  sp$analyses$type <- c(NA, "model")
  code <- tfl_ard_code(sp, output_id = "T", save = FALSE)
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "# FIT: a model, kept as fit (not in the ARD)", fixed = TRUE)
  # (stats:: is left off, as the program writes an attached package's calls)
  expect_match(txt, "fit <- (stats::)?lm\\(AGE ~ SEX, data = ")
  expect_false(grepl("bind_items(ard_a, fit", txt, fixed = TRUE))
  expect_false(grepl("tag_ard(report_id, \"FIT\"", txt, fixed = TRUE))
})
