# The study's code lists applied to the data before the ARD is made.

test_that("code lists make the listed columns factors before the analyses", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "categorical", dataset = "ADSL",
                        population_id = "SAF", by = "ARM",
                        variables = "AGEGR1 | SEX"))
  cl <- data.frame(output_id = NA, variable = c("AGEGR1", "AGEGR1", "AGEGR1", "AGEGR1", "SEX"),
                   value = c("<65", "65-80", ">80", "Unknown", "M"),
                   order = c("1", "2", "3", "4", "1"))
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "adsl <- readRDS\\([^)]*\\) \\|> \\.levels\\(\\)")
  expect_match(code, "`AGEGR1` = c(\"<65\", \"65-80\", \">80\", \"Unknown\")", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  ag <- ard[ard$variable == "AGEGR1", ]
  lv <- unique(vapply(ag$variable_level, as.character, ""))
  expect_identical(lv, c("<65", "65-80", ">80", "Unknown"))   # in order, the empty one too
  n0 <- unlist(ag$stat[ag$stat_name == "n" &
                         vapply(ag$variable_level, as.character, "") == "Unknown"])
  expect_true(all(n0 == 0))
  # a value the list does not have comes after the listed ones
  sx <- unique(vapply(ard$variable_level[ard$variable == "SEX"], as.character, ""))
  expect_identical(sx, c("M", "F"))
  # without code lists: as before
  plain <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  expect_false(any(vapply(plain$variable_level[plain$variable == "AGEGR1"],
                          as.character, "") == "Unknown"))
  # the fingerprint changes with code lists, and not without
  expect_identical(tfl_ard_spec_hash(sp, "T"), tfl_ard_spec_hash(sp, "T", codelists = NULL))
  expect_false(identical(tfl_ard_spec_hash(sp, "T"), tfl_ard_spec_hash(sp, "T", codelists = cl)))
  # a report's own code list rows are not the study's
  cl2 <- cl; cl2$output_id <- "T1"
  expect_null(.codelist_levels(cl2))
})

test_that("variables$empty_levels = hide leaves out the values no record has", {
  skip_if_not_installed("cards")
  skip_if(utils::packageVersion("rtfreporter") < "0.8.2.9015")
  adsl <- data.frame(
    USUBJID = 1:6, TRT = rep(c("A", "B"), 3),
    RACE = factor(c("WHITE", "WHITE", "ASIAN", "WHITE", "ASIAN", "WHITE"),
                  levels = c("WHITE", "ASIAN", "OTHER")))
  d <- suppressMessages(rtfreporter::normalize_ard(
    cards::ard_tabulate(adsl, by = TRT, variables = RACE)))
  tab <- function(v) {
    sp <- tfl_table_spec(tables = data.frame(cols = "TRT"), variables = v)
    p <- tfl_table_plan(d, sp) |> rtfreporter::plan_cells(notes = FALSE)
    as.character(suppressMessages(rtfreporter::plan_apply(p, "table"))$label)
  }
  # the default (and `show`): every value of the code list has its row
  expect_identical(tab(data.frame(variable = "RACE", label = "Race")),
                   c("WHITE", "ASIAN", "OTHER"))
  expect_identical(tab(data.frame(variable = "RACE", empty_levels = "show")),
                   c("WHITE", "ASIAN", "OTHER"))
  hide <- data.frame(variable = "RACE", empty_levels = "hide")
  expect_identical(tab(hide), c("WHITE", "ASIAN"))
  # the code says so, and a plan gives the column back
  sp <- tfl_table_spec(tables = data.frame(cols = "TRT"), variables = hide)
  expect_true(any(grepl(".drop_empty = \"RACE\"", tfl_table_code(sp),
                        fixed = TRUE)))
  back <- suppressMessages(tfl_as_table_spec(tfl_table_plan(d, sp), "T1"))
  expect_identical(back$variables$empty_levels[back$variables$variable == "RACE"],
                   "hide")
  expect_error(tfl_table_spec(tables = data.frame(cols = "TRT"),
    variables = data.frame(variable = "RACE", empty_levels = "no")),
    "empty_levels")
})
