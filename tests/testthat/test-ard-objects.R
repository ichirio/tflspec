# Named values (sheet `objects`; data.yml / a report's ard `objects:`):
# `id <- code` at the program's top, named by a denominator, an argument,
# a condition

ob_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)

ob_spec <- function(objects, den = "n_saf", where = NA, check = TRUE) {
  s <- list(
    study = ob_df(key = "id", value = "USUBJID"),
    datasets = ob_df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = ob_df(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    objects = objects,
    analyses = ob_df(
      output_id = c("T1", "T2"),
      analysis_id = c("SEX", "AE"),
      method = c("cards::ard_tabulate", "cards::ard_stack_hierarchical"),
      dataset = c("ADSL", "ADAE"),
      population_id = "SAF",
      where = c(NA, where),
      by = "TRT01A",
      variables = c("SEX", "AEBODSYS | AEDECOD"),
      denominator = c(den, "den_arm")))
  if (check) tfl_ard_spec(s) else s
}

ob_rows <- function() ob_df(
  output_id = c(NA, "T2", "T2"),
  object_id = c("n_saf", "den_arm", "min_age"),
  code = c("200L", "data.frame(TRT01A = rep(c(\"A\", \"B\"), c(10L, 20L)), USUBJID = 1:30)", "18"),
  label = c("Subjects of the safety set", NA, NA))

test_that("the program makes them at its top, under their labels; a report's in its part", {
  x <- ob_spec(ob_rows())
  one <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body")
  expect_true(all(c("# ---- objects ----", "# n_saf: Subjects of the safety set", "n_saf <- 200L")
                  %in% one))
  # (T2's are not T1's)
  expect_false(any(grepl("den_arm <-", one, fixed = TRUE)))
  expect_match(paste(one, collapse = "\n"), "denominator = n_saf,", fixed = TRUE)
  # before the analyses
  expect_lt(match("n_saf <- 200L", one), match("# ---- analyses ----", one))
  both <- tfl_ard_code(x, save = FALSE, part = "body")
  expect_identical(sum(both == "n_saf <- 200L"), 1L)
  i2 <- grep("^den_arm <- ", both)
  expect_length(i2, 1L)
  expect_gt(i2, match("# ---- T2 ----", both))
  expect_match(paste(both, collapse = "\n"), "denominator = den_arm,", fixed = TRUE)
})

test_that("a condition names one; the ARD's N is the object's", {
  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  adsl <- cards::ADSL
  adsl$TRT01A <- adsl$ARM
  saveRDS(adsl, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  ob <- ob_rows()
  arms <- sort(unique(adsl$TRT01A))
  ob$code[2L] <- sprintf("data.frame(TRT01A = rep(c(%s), c(10L, 20L, 30L)), USUBJID = 1:60)",
                         paste(sprintf("\"%s\"", arms), collapse = ", "))
  x <- ob_spec(ob, where = "AGE >= min_age")
  a <- tfl_build_ard(x, dir = dir, save = FALSE)
  n <- a[a$analysis_id == "SEX" & a$stat_name == "N", ]
  expect_identical(unique(as.numeric(unlist(n$stat))), 200)
  n <- a[a$analysis_id == "AE" & a$stat_name == "N" & !is.na(a$group1), ]
  got <- tapply(as.numeric(unlist(n$stat)), unlist(n$group1_level), unique)
  expect_equal(as.numeric(got[arms]), c(10, 20, 30))
})

test_that("the sheet's problems", {
  probs <- function(ob) tflspec:::.ard_spec_problems(ob_spec(ob, check = FALSE))$message
  ob <- ob_rows(); ob$object_id[1L] <- "2bad"
  expect_true(any(grepl("is an R name", probs(ob), fixed = TRUE)))
  ob <- ob_rows(); ob$code[3L] <- NA
  expect_true(any(grepl("min_age: its code", probs(ob), fixed = TRUE)))
  ob <- ob_rows(); ob$code[3L] <- "1 +"
  expect_true(any(grepl("min_age: the code is not R", probs(ob), fixed = TRUE)))
  ob <- ob_rows(); ob$object_id[3L] <- "n_saf"
  expect_true(any(grepl("named twice", probs(ob), fixed = TRUE)))
  ob <- ob_rows(); ob$object_id[3L] <- "pop_saf"
  expect_true(any(grepl("a dataset or a population", probs(ob), fixed = TRUE)))
  # a denominator naming no object, population, dataset nor analysis data
  ob <- ob_rows()[-1L, ]
  expect_true(any(grepl("denominator(s) n_saf", probs(ob), fixed = TRUE)))
})

test_that("YAML: data.yml's and a report's objects: are the sheet's, and come back", {
  skip_if_not_installed("haven")
  d <- withr_tempdir()
  tplfig_study(d, "adsl", "OBJ-1")
  cat("objects:\n  - {id: n_saf, code: !r 200L, label: Subjects of the safety set}\n",
      file = file.path(d, "spec", "data.yml"), append = TRUE)
  o <- tfl_output("T1", "table", ard = list(
    objects = list(list(id = "min_age", code = tfl_r("18"))),
    analyses = list(list(id = "SEX", method = "cards::ard_tabulate", dataset = "ADSL",
                         population = "SAF", where = tfl_r("AGE >= min_age"), by = "TRT01A",
                         variables = "SEX", denominator = "n_saf"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T1.yml"))
  sh <- tfl_sheets_from_yaml(d)
  ob <- sh$ard$objects
  expect_identical(ob$object_id, c("n_saf", "min_age"))
  expect_identical(ob$output_id, c(NA, "T1"))
  expect_identical(ob$code, c("200L", "18"))
  code <- tfl_ard_code(tfl_read_study_spec(d), output_id = "T1", save = FALSE, part = "body")
  expect_true(all(c("n_saf <- 200L", "min_age <- 18") %in% code))
  out <- withr_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, out))
  back <- tfl_sheets_from_yaml(out)
  expect_identical(back$ard$objects, ob)
})
