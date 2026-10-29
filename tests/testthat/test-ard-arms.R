# The arms in their numeric order: order_arms() on a dataset an analysis
# is by an arm of.

test_that("order_arms(): each arm variable by its N pair; a blank is missing; a code list after wins", {
  e <- new.env()
  sys.source(system.file("helpers", "study_helpers.R", package = "tflspec"), envir = e)
  d <- data.frame(TRT01A = c("Placebo", "High", "", "Low", "High"),
                  TRT01AN = c(1, 3, NA, 2, 3),
                  ARM = c("B", "A", "A", "B", "A"), ARMN = c(2, 1, 1, 2, 1),
                  TRT01P = c("x", "y", "x", "y", "x"),
                  AGE = 1:5, stringsAsFactors = FALSE)
  r <- e$order_arms(d)
  expect_identical(levels(r$TRT01A), c("Placebo", "Low", "High"))
  expect_true(is.na(r$TRT01A[3L]))
  expect_identical(levels(r$ARM), c("A", "B"))
  expect_type(r$TRT01P, "character")   # no pair: as it is
  expect_identical(r$TRT01AN, d$TRT01AN)
  expect_identical(r$AGE, d$AGE)
  # a code list after it: the list's order
  r2 <- e$set_levels(r, TRT01A = c(High = "High", Low = "Low", Placebo = "Placebo"))
  expect_identical(levels(r2$TRT01A), c("High", "Low", "Placebo"))
})

test_that("the program orders the arms of every dataset it reads", {
  skip_on_cran()
  skip_if_not_installed("cards")
  by_arm <- exact_spec(list(method = "categorical", dataset = "ADSL",
                            population_id = "SAF", by = "ARM", variables = "SEX"))
  code <- paste(tfl_ard_code(by_arm, part = "body"), collapse = "\n")
  expect_match(code, "adsl <- readRDS(\"adam/ADSL.rds\") |> order_arms()", fixed = TRUE)
  no_arm <- exact_spec(list(method = "categorical", dataset = "ADSL",
                            population_id = "SAF", by = "SEX", variables = "AGEGR1"))
  code2 <- paste(tfl_ard_code(no_arm, part = "body"), collapse = "\n")
  expect_true(grepl("order_arms()", code2, fixed = TRUE))   # (a model's reference arm too)
  # the ARD's groups in ARMN's order, not the alphabet's
  adam <- exact_data()
  adam$ADSL$ARMN <- c(Placebo = 1, `Xanomeline Low Dose` = 2, `Xanomeline High Dose` = 3)[adam$ADSL$ARM]
  dir <- exact_dir(adam)
  ard <- suppressMessages(tfl_build_ard(by_arm, dir = dir, save = FALSE))
  expect_identical(unique(vapply(ard$group1_level[ard$variable == "SEX"], as.character, "")),
                   c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
})
