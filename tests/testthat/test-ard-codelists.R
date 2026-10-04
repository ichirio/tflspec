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
  expect_match(code, "adsl <- .levels(adsl)", fixed = TRUE)
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
